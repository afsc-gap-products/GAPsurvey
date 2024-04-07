#' @title Station centroid locations for each station from akgfmaps
#' @description Station centroid coordinates for each station for all surveys, as defined by the {akgfmaps} package. 
#' @usage data('station_coords')
#' @author Sean Rohan (sean.rohan AT noaa.gov)
#' @format A data frame with 31594 observations on the following 4 variables.
#' \describe{
#'   \item{\code{srvy}}{Survey abbreviation. Abbreviated survey names. The column 'srvy' is associated with the 'survey' and 'survey_definition_id' columns. Northern Bering Sea (NBS), Southeastern Bering Sea (EBS), Bering Sea Slope (BSS), Gulf of Alaska (GOA), Aleutian Islands (AI).}
#'   \item{\code{station}}{Station ID. Alpha-numeric designation for the station established in the design of a survey.}
#'   \item{\code{latitude_dd}}{Latitude (decimal degrees). Latitude (one hundred thousandth of a decimal degree).}
#'   \item{\code{longitude_dd}}{Longitude (decimal degrees). Longitude (one hundred thousandth of a decimal degree).}#'   }
#' @source https://github.com/afsc-gap-products/akgfmaps
#' @keywords station survey data
#' @examples
#' data(station_coords)
#' @details Find code to create this table in ./inst/run.R
'station_coords'
