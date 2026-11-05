#' Pull Haul Data from RACE Database
#'
#' Connects to the RACE database (or uses an existing connection) to query haul
#' information for a given cruise and vessel. The output is written to a CSV file
#' and returned invisibly as a data frame.
#'
#' @param cruise Numeric or character. The cruise identifier (e.g., `"202601"`).
#' @param vessel Numeric or character. The vessel ID (e.g., `"176"`).
#' @param channel Oracle channel object. An active Oracle connection created via
#'   `gapindex::get_connected()`. If `NULL` (default), a new connection is established.
#' @param output_path Character. File path where the output CSV will be saved.
#'   Defaults to `"haul_data.csv"`.
#'
#' @return A data frame containing filtered haul information with columns `HAUL_ID`
#'   and `CRUISE_ID` removed. Returned invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example using an existing connection
#' con <- gapindex::get_connected(check_access = FALSE)
#' haul_data <- pull_repick_data(cruise = "202601", vessel = "176", channel = con)
#'
#' # Example auto-connecting and saving to a custom path
#' pull_repick_data(cruise = 202601, vessel = 176, output_path = "repick_hauls.csv")
#' }
pull_repick_data <- function(cruise, vessel, channel = NULL, output_path = "haul_data.csv") {
  # Connect to database if no open channel provided
  if (is.null(channel)) {
    channel <- gapindex::get_connected(check_access = FALSE)
  }

  haul_info_query <- sprintf(
    "SELECT
        e.haul,
        c.cruise,
        c.vessel_id,
        e.cruise_id,
        e.haul_id,
        e.haul_type,
        e.station,
        e.stratum,
        e.performance,
        e.edit_gear_depth,
        e.edit_bottom_depth,
        e.edit_distance_fished,
        TO_CHAR(v.edit_date_time, 'mm/dd/yyyy') AS haul_date,
        v.edit_latitude,
        v.edit_longitude,
        TO_CHAR(v.edit_date_time, 'hh24:mi:ss') AS onbottom,
        TO_CHAR(x.edit_date_time, 'hh24:mi:ss') AS offbottom
      FROM race_data.edit_hauls e
      JOIN race_data.cruises c
        ON e.cruise_id = c.cruise_id
      LEFT JOIN race_data.edit_events v
        ON e.haul_id = v.haul_id AND v.event_type_id = 3
      LEFT JOIN race_data.edit_events x
        ON e.haul_id = x.haul_id AND x.event_type_id = 7
      WHERE c.vessel_id = %s
        AND c.cruise = %s
      ORDER BY e.cruise_id, e.haul;",
    vessel, cruise
  )

  haul_info <- gapindex::sql_query(channel = channel, query = haul_info_query)
  # haul_info <- subset(haul_info, select = -c(HAUL_ID, CRUISE_ID))
  haul_info <- haul_info[, !(names(haul_info) %in% c("HAUL_ID", "CRUISE_ID")), drop = FALSE]

  if (nrow(haul_info) == 0) {
    stop(sprintf("No matching haul found for Cruise: %s, Vessel: %s", cruise, vessel))
  }

  utils::write.csv(haul_info, output_path, row.names = FALSE)
  message(sprintf("Successfully wrote haul file to '%s'.", output_path))

  # Return data invisibly
  invisible(haul_info)
}
