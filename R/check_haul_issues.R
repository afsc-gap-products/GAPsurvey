#' Check Haul Depths for Errors
#'
#' Connects to the RACE database via gapindex, extracts unfinalized haul and
#' event measurements for a specific cruise and region, and identifies hauls
#' with depth anomalies (e.g., missing depths or gear depth exceeding bottom depth) for the data finalization steps.
#'
#' @param cruise Numeric or character. Cruise identifier (e.g., \code{202601}).
#' @param region Character. Survey region code (e.g., \code{"AI"}, \code{"GOA"}, \code{"EBS"}).
#' @param channel Oracle connection object. Optional. If \code{NULL}, establishes a
#'   connection via \code{gapindex::get_connected()}.
#'
#' @return A data frame containing flagged haul records sorted by region, cruise, vessel, and haul.
#' @export
#'
#' @importFrom gapindex get_connected sql_query
#' @importFrom dplyr filter select arrange contains
#'
#' @examples
#' \dontrun{
#' check_haul_depths(cruise = 202601, region = "AI")
#' }
check_haul_depths <- function(cruise, region, channel = NULL) {
  # Local NULL bindings for NSE column names to pass R CMD check
  gear_depth <- bottom_depth <- haul_id <- station <- NULL
  performance <- vessel_id <- haul <- NULL

  # Map region codes to survey definition IDs
  survey_def_map <- list(
    "AI"  = c(52),
    "GOA" = c(39, 47),
    "EBS" = c(98),
    "BSS" = c(78),
    "NBS" = c(143)
  )

  reg_upper <- toupper(region)
  if (!reg_upper %in% names(survey_def_map)) {
    stop(sprintf("Invalid region '%s'. Must be one of: %s", region, paste(names(survey_def_map), collapse = ", ")))
  }
  target_def_ids <- survey_def_map[[reg_upper]]

  if (is.null(channel)) {
    channel <- gapindex::get_connected(check_access = FALSE)
  }

  query <- sprintf("
    SELECT
      h.HAUL_ID,
      c.CRUISE,
      c.REGION,
      c.VESSEL_ID,
      h.HAUL,
      h.STATION,
      h.PERFORMANCE,
      h.EDIT_GEAR_DEPTH AS GEAR_DEPTH,
      h.EDIT_BOTTOM_DEPTH AS BOTTOM_DEPTH
    FROM RACE_DATA.EDIT_HAULS h
    JOIN RACE_DATA.V_CRUISES c ON h.CRUISE_ID = c.CRUISE_ID
    WHERE c.CRUISE = %s
      AND c.SURVEY_DEFINITION_ID IN (%s)
  ", cruise, paste(target_def_ids, collapse = ","))

  haul_data <- gapindex::sql_query(channel = channel, query = query)

  if (!is.data.frame(haul_data) || nrow(haul_data) == 0) {
    stop(sprintf("No records found for Cruise: %s in Region: %s", cruise, region))
  }
  names(haul_data) <- tolower(names(haul_data))

  haul_data |>
    dplyr::filter(
      is.na(gear_depth) | is.na(bottom_depth) |
        gear_depth == 0 |
        bottom_depth == 0 |
        gear_depth >= bottom_depth
    ) |>
    dplyr::select(
      haul_id,
      cruise:station,
      performance,
      dplyr::contains("depth")
    ) |>
    dplyr::arrange(region, cruise, vessel_id, haul)
}


#' Check AI/GOA Hauls for Abundance Status
#'
#' Queries RACE edit tables directly to extract haul data, catch weights, and tow details,
#' then flags operational irregularities (e.g., improper gear, bad performance, short tow
#' duration relative to catch weight, or duplicate stations) for the AI/GOA data finalization steps.
#'
#' @param cruise Numeric or character. Cruise identifier (e.g., \code{202601}).
#' @param region Character. Survey region code (e.g., \code{"AI"}, \code{"GOA"}).
#' @param channel Oracle connection object. Optional. If \code{NULL}, establishes a
#'   connection via \code{gapindex::get_connected()}.
#'
#' @return A summary data frame listing flagged hauls and their specific issue
#'   labels, sorted by region, cruise, vessel, and haul. These hauls will be designated non-abundance hauls for finalization.
#' @export
#'
#' @importFrom gapindex get_connected sql_query
#' @importFrom dplyr group_by mutate right_join join_by ungroup select add_count case_when filter arrange
#'
#' @examples
#' \dontrun{
#' check_haul_issues(cruise = 202601, region = "AI")
#' }
check_haul_abundance <- function(cruise, region, channel = NULL) {
  # Local NULL bindings for NSE column names to pass R CMD check
  haul_id <- total_weight <- haul_weight_t <- vessel_id <- haul <- NULL
  duration <- station <- stratum <- n_station <- accessories <- NULL
  gear <- haul_type <- performance <- issue <- NULL

  # Map region codes to survey definition IDs
  survey_def_map <- list(
    "AI"  = c(52),
    "GOA" = c(39, 47),
    "EBS" = c(98),
    "BSS" = c(78),
    "NBS" = c(143)
  )

  reg_upper <- toupper(region)
  if (!reg_upper %in% names(survey_def_map)) {
    stop(sprintf("Invalid region '%s'. Must be one of: %s", region, paste(names(survey_def_map), collapse = ", ")))
  }
  target_def_ids <- survey_def_map[[reg_upper]]

  if (is.null(channel)) {
    channel <- gapindex::get_connected(check_access = FALSE)
  }

  # Query 1: Extract haul information filtered by CRUISE and SURVEY_DEFINITION_ID
  haul_query <- sprintf("
    SELECT
      h.HAUL_ID,
      c.CRUISE,
      c.REGION,
      c.VESSEL_ID,
      h.HAUL,
      h.STATION,
      h.STRATUM,
      h.HAUL_TYPE,
      h.PERFORMANCE,
      h.ACCESSORIES,
      h.GEAR,
      ROUND(m.EDIT_DURATION_OB_FB * 60, 2) AS DURATION
    FROM RACE_DATA.EDIT_HAULS h
    JOIN RACE_DATA.V_CRUISES c ON h.CRUISE_ID = c.CRUISE_ID
    LEFT JOIN RACE_DATA.EDIT_HAUL_MEASUREMENTS m ON h.HAUL_ID = m.HAUL_ID
    WHERE c.CRUISE = %s
      AND c.SURVEY_DEFINITION_ID IN (%s)
  ", cruise, paste(target_def_ids, collapse = ","))

  new_haul <- gapindex::sql_query(channel = channel, query = haul_query)

  if (!is.data.frame(new_haul) || nrow(new_haul) == 0) {
    stop(sprintf("No haul records found in edit tables for Cruise: %s in Region: %s", cruise, region))
  }
  names(new_haul) <- tolower(names(new_haul))

  # Clean valid haul_ids for SQL query
  valid_haul_ids <- stats::na.omit(unique(new_haul$haul_id))

  # Query 2: Extract catch species totals
  catch_query <- sprintf("
    SELECT
      c.HAUL_ID,
      cs.SPECIES_CODE,
      cs.TOTAL_WEIGHT_IN_HAUL AS TOTAL_WEIGHT
    FROM RACE_DATA.EDIT_CATCH_SPECIES cs
    JOIN RACE_DATA.EDIT_CATCH_SAMPLES c
      ON cs.CATCH_SAMPLE_ID = c.CATCH_SAMPLE_ID
    WHERE c.HAUL_ID IN (%s)
  ", paste(valid_haul_ids, collapse = ","))

  new_catch <- gapindex::sql_query(channel = channel, query = catch_query)

  # Calculate total haul weight per haul_id directly
  if (is.data.frame(new_catch) && nrow(new_catch) > 0) {
    names(new_catch) <- tolower(names(new_catch))

    catch_summary <- new_catch |>
      dplyr::group_by(haul_id) |>
      dplyr::summarize(
        haul_weight_t = round(sum(total_weight, na.rm = TRUE) / 1000, 1),
        .groups = "drop"
      )
  } else {
    catch_summary <- data.frame(
      haul_id = numeric(0),
      haul_weight_t = numeric(0)
    )
  }

  # Join catch weight back to hauls and evaluate operational issues
  new_haul |>
    dplyr::left_join(catch_summary, by = dplyr::join_by(haul_id)) |>
    dplyr::mutate(
      haul_weight_t = ifelse(is.na(haul_weight_t), 0, haul_weight_t)
    ) |>
    dplyr::select(cruise, region, vessel_id, haul:duration, haul_weight_t) |>
    unique() |>
    dplyr::group_by(cruise, region, station, stratum) |>
    dplyr::add_count(name = "n_station") |>
    dplyr::mutate(issue = dplyr::case_when(
      !accessories %in% c(15, 130) ~ "improper accessories",
      !gear %in% c(44, 172) ~ "improper gear",
      haul_type != 3 ~ "haul type not 3",
      performance < 0 ~ "performance inadequate",
      duration < 10 & haul_weight_t < duration ~ "catch too small for tow duration",
      n_station > 1 & sum(haul_type == 3) > 1 & sum(performance >= 0) > 1 ~ "station duplicated",
      TRUE ~ "none"
    )) |>
    dplyr::select(-n_station) |>
    dplyr::filter(issue != "none") |>
    dplyr::arrange(region, cruise, vessel_id, haul)
}

#' Find GPS coordinates with duplicate timestamps
#'
#' @param cruise cruise id, e.g., 202601
#' @param vessel_id vessel code (OEX = 148, AKP = 176)
#' @param haul haul number
#' @param channel ODBC channel, established by setting channel = gapindex::get_connected()
#' @details
#' When the GPS coordinates are recorded very frequently, you can occasionally get multiple GPS coordinate points for the same hh:mm:ss timestamp. This causes an error in GIDES (edit check ID 843; 'x child records have dpulicate header and date-time values...'). You can use this function to identify the duplicate records and use the POSITION_ID of those duplicates in the "Position Data" section of GIDES to change their Datum Code Description in GIDES to 2 ("DUPLICATE DATE_TIME, DIFFERENT VALUE(S) (Use N)"). This function may become obsolete in the future if/when the OFIS folks change the position recording frequency to something closer to 1 sec.
#'
#'
#' @returns a dataframe containing duplicated GPS points for the same timestamp.
#' @export
#'
#' @examples
#' \dontrun{
#' find_duplicate_timestamps(cruise = 202601,vessel_id = 176, haul = 121, channel = channel)
#' }
find_duplicate_timestamps <- function(cruise, vessel_id, haul, channel = NULL) {
  if (is.null(channel)) {
    channel <- gapindex::get_connected(check_access = FALSE)
  }

  # SQL query to find duplicate timestamps with different GPS coords.
  duplicate_query <- paste0("
    select *
    from (
        select
            c.*,
            a.haul_id as haul_id_from_hauls,
            a.haul as haul_number,
            count(*) over (partition by c.edit_date_time) as record_count
        from race_data.edit_hauls a,
             race_data.edit_position_headers b,
             race_data.edit_positions c,
             race_data.edit_events g,
             race_data.edit_events h
        where a.cruise_id = (
            select cruise_id
            from race_data.cruises
            where vessel_id = ", vessel_id, "
              and cruise = ", cruise, "
        )
        and a.haul = ", haul, "
        and a.haul_id = b.haul_id
        and a.haul_id = g.haul_id
        and a.haul_id = h.haul_id
        and g.event_type_id = 15
        and h.event_type_id = 16
        and b.position_header_id = c.position_header_id
        and c.edit_date_time between g.edit_date_time and h.edit_date_time
    )
    where record_count > 1
    order by edit_date_time
  ")

  result <- gapindex::sql_query(channel = channel, query = duplicate_query)

  if (nrow(result) == 0) {
    message("No duplicate timestamps found for this cruise/vessel/haul.")
  }

  return(result)
}
