#' Plot Haul Track Over Survey Grid Cells
#'
#' Queries Oracle database tables via \code{gapindex} to extract a haul track and 
#' plot it over corresponding survey grid cells shaded by depth stratum.
#'
#' @param cruise Numeric or character. Cruise identifier (e.g., \code{202601}).
#' @param vessel Numeric or character. Vessel ID number (e.g., \code{176}).
#' @param haul Numeric or character. Haul number (e.g., \code{194}).
#' @param region Character. Region code passed to \code{akgfmaps::get_base_layers()}. 
#'   Defaults to \code{"ai"}.
#' @param channel Oracle connection to database. If \code{NULL}, a new connection is created.
#'
#' @return Invisibly returns a named list containing:
#'   \item{haul_id}{Numeric haul ID retrieved from RACE_DATA.}
#'   \item{station}{Target station assigned to the haul.}
#'   \item{track}{\code{sf} LINESTRING geometry of the haul track.}
#'   \item{midpoint}{\code{sf} POINT geometry representing the haul midpoint.}
#'   \item{grid}{\code{sf} POLYGON layer of the plotted survey grid cells.}
#'
#' @export
#'
#' @importFrom gapindex get_connected sql_query
#' @importFrom dplyr filter mutate group_by summarize select distinct arrange 
#' @importFrom sf st_as_sf st_cast st_crs st_transform st_centroid st_intersects st_geometry st_coordinates st_drop_geometry st_union
#' @importFrom akgfmaps get_base_layers
#' @importFrom graphics par plot text legend layout frame
#'
#' @examples
#' \dontrun{
#' # Example using an existing connection
#' con <- gapindex::get_connected(check_access = FALSE)
#' plot_haul_track(cruise = 202601, vessel = 176, haul = 164, channel = con)
#' 
#' # Example auto-connecting to Oracle database
#' plot_haul_track(cruise = 202601, vessel = 176, haul = 194)
#' }

plot_haul_track <- function(cruise, vessel, haul, region = "ai", channel = NULL) {
  # Bind dplyr NSE column names to suppress R CMD check notes
  STATION <- STRATUM <- STRATUM_NUM <- STRATUM_RANK <- fill_color <- geometry <- NULL
  
  # Save graphic parameters and layout matrix to restore on function exit
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1)
    graphics::par(old_par)
  }, add = TRUE)
  
  if (is.null(channel)) {
    channel <- gapindex::get_connected(check_access = FALSE)
  }
  
  haul_info_query <- sprintf("
    SELECT 
      h.HAUL_ID, 
      h.STATION
    FROM RACE_DATA.EDIT_HAULS h
    JOIN RACE_DATA.CRUISES c ON h.CRUISE_ID = c.CRUISE_ID
    WHERE c.CRUISE = %s 
      AND c.VESSEL_ID = %s 
      AND h.HAUL = %s
  ", cruise, vessel, haul)
  
  haul_info <- gapindex::sql_query(channel = channel, query = haul_info_query)
  
  if (nrow(haul_info) == 0) {
    stop(sprintf("No matching haul found for Cruise: %s, Vessel: %s, Haul: %s", cruise, vessel, haul))
  }
  
  haul_id <- haul_info$HAUL_ID[1]
  target_station <- haul_info$STATION[1]
  
  track_query <- sprintf("
    SELECT
      SIGN(EDIT_LONGITUDE) * ( TRUNC(ABS(EDIT_LONGITUDE) / 100) + (MOD(ABS(EDIT_LONGITUDE), 100) / 60) ) AS lon,
      FLOOR(edit_latitude/100) + (edit_latitude - (FLOOR(edit_latitude/100) * 100)) / 60 AS lat
    FROM RACE_DATA.EDIT_EVENTS
    WHERE HAUL_ID = %s
      AND EVENT_TYPE_ID IN (3, 7)
  ", haul_id)
  
  track_pts <- gapindex::sql_query(channel = channel, query = track_query)
  
  if (nrow(track_pts) < 2) {
    warning("Fewer than 2 event positions found for this haul_id. Cannot draw line track.")
    return(NULL)
  }
  
  track <- track_pts |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
    dplyr::summarize(do_union = FALSE) |>
    sf::st_cast("LINESTRING")
  
  suppressWarnings(
    base <- akgfmaps::get_base_layers(select.region = region)
  )
  
  track_proj <- sf::st_transform(track, sf::st_crs(base$survey.grid))
  track_midpoint <- sf::st_centroid(track_proj)
  
  intersecting_subpolygons <- sf::st_intersects(base$survey.grid, track_proj, sparse = FALSE)[, 1]
  intersecting_stations <- unique(base$survey.grid$STATION[intersecting_subpolygons])
  
  target_grids <- base$survey.grid |> 
    dplyr::filter(STATION %in% intersecting_stations)
  
  if (nrow(target_grids) == 0) {
    warning("No grid cells intersect with the haul track.")
    return(NULL)
  }
  
  blue_palette <- c("#E0F3F8B3", "#ABD9E9B3", "#74ADD1B3", "#4575B4B3", "#313695B3")
  
  target_grids <- target_grids |>
    dplyr::mutate(
      STRATUM_NUM = as.numeric(as.character(STRATUM)),
      STRATUM_RANK = as.numeric(factor(STRATUM_NUM, levels = sort(unique(STRATUM_NUM)))),
      fill_color = blue_palette[pmin(STRATUM_RANK, 5)]
    )
  
  legend_data <- target_grids |>
    sf::st_drop_geometry() |>
    dplyr::select(STRATUM_NUM, fill_color) |>
    dplyr::distinct() |>
    dplyr::arrange(STRATUM_NUM)
  
  station_polygons <- target_grids |>
    dplyr::group_by(STATION) |>
    dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop")
  
  suppressWarnings(
    grid_centroids <- sf::st_centroid(station_polygons)
  )
  centroid_coords <- sf::st_coordinates(grid_centroids)
  
  # Set up a 2-column layout: 80% map canvas on left, 20% legend canvas on right
  graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(0.8, 0.2))
  
  # Panel 1: Map Plot
  graphics::par(mar = c(2, 2, 2, 0.5))
  
  graphics::plot(
    sf::st_geometry(target_grids), 
    col = target_grids$fill_color, 
    border = "grey50", 
    main = paste("Haul", haul),
    line = 0.2
  )
  
  graphics::text(
    x = centroid_coords[, 1], 
    y = centroid_coords[, 2], 
    labels = station_polygons$STATION, 
    col = "black", 
    cex = 0.8, 
    font = 2
  )
  
  graphics::plot(sf::st_geometry(track_proj), add = TRUE, col = "red", lwd = 4)
  graphics::plot(sf::st_geometry(track_midpoint), add = TRUE, col = "black", pch = 19, cex = 1)
  
  # Panel 2: Legend Plot
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::frame()
  
  graphics::legend(
    x = "left",
    legend = paste("Stratum", legend_data$STRATUM_NUM),
    fill = legend_data$fill_color,
    border = rep("grey50", nrow(legend_data)),
    bg = "white",
    box.col = "grey70",
    cex = 0.8,
    x.intersp = 0.5,
    y.intersp = 0.9,
    bty = "n"
  )
  
  invisible(list(
    haul_id = haul_id, 
    station = target_station, 
    track = track_proj, 
    midpoint = track_midpoint,
    grid = target_grids
  ))
}
