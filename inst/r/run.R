
# Connect to oracle ------------------------------------------------------------

library(magrittr)
library(readr)
library(dplyr)

if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
  channel <- channel_products
} else {
  # library(devtools)
  # devtools::install_github("afsc-gap-products/gapindex")
  library(gapindex)
  channel <- gapindex::get_connected()
}

# Load column metadata table ---------------------------------------------------

metadata_table_comment <- dplyr::bind_rows(
  # tables
  RODBC::sqlQuery(
    channel = channel,
    query = "SELECT table_name, comments
FROM all_tab_comments
WHERE owner = 'GAP_PRODUCTS'
ORDER BY table_name") %>%
    data.frame(),
  # materialized view
  RODBC::sqlQuery(
    channel = channel,
    query = "SELECT * FROM user_mview_comments") %>%
    data.frame() %>%
    dplyr::rename(TABLE_NAME = MVIEW_NAME) )

metadata_colname <- RODBC::sqlQuery(
  channel = channel,
  query = "SELECT * FROM GAP_PRODUCTS.METADATA_COLUMN") %>%
  janitor::clean_names()

## FOSS catch and haul data ----------------------------------------------------

public_data <- RODBC::sqlQuery(
  channel = channel,
  query =
    "SELECT
hh.YEAR,
hh.SRVY,
hh.SURVEY,
hh.SURVEY_DEFINITION_ID,
-- hh.SURVEY_NAME,
hh.CRUISE,
hh.CRUISEJOIN,
hh.HAUL,
hh.HAULJOIN,
hh.STRATUM,
hh.STATION,
hh.VESSEL_ID,
hh.VESSEL_NAME,
hh.DATE_TIME,
hh.LATITUDE_DD_START,
hh.LONGITUDE_DD_START,
hh.LATITUDE_DD_END,
hh.LONGITUDE_DD_END,
hh.BOTTOM_TEMPERATURE_C,
hh.SURFACE_TEMPERATURE_C,
hh.DEPTH_M,
cc.SPECIES_CODE,
ss.ITIS,
ss.WORMS,
ss.COMMON_NAME,
ss.SCIENTIFIC_NAME,
ss.ID_RANK,
CASE WHEN cc.CPUE_KGKM2 IS NULL THEN 0 ELSE cc.CPUE_KGKM2 END AS CPUE_KGKM2,
CASE WHEN cc.CPUE_NOKM2 IS NULL THEN 0 ELSE cc.CPUE_NOKM2 END AS CPUE_NOKM2,
CASE WHEN cc.COUNT IS NULL THEN 0 ELSE cc.COUNT END AS COUNT,
CASE WHEN cc.WEIGHT_KG IS NULL THEN 0 ELSE cc.WEIGHT_KG END AS WEIGHT_KG,
CASE WHEN cc.TAXON_CONFIDENCE IS NULL THEN NULL ELSE cc.TAXON_CONFIDENCE END AS TAXON_CONFIDENCE,
hh.AREA_SWEPT_KM2,
hh.DISTANCE_FISHED_KM,
hh.DURATION_HR,
hh.NET_WIDTH_M,
hh.NET_HEIGHT_M,
hh.PERFORMANCE
FROM GAP_PRODUCTS.FOSS_SURVEY_SPECIES sv
FULL OUTER JOIN GAP_PRODUCTS.FOSS_SPECIES ss
ON sv.SPECIES_CODE = ss.SPECIES_CODE
FULL OUTER JOIN GAP_PRODUCTS.FOSS_HAUL hh
ON sv.SURVEY_DEFINITION_ID = hh.SURVEY_DEFINITION_ID
FULL OUTER JOIN GAP_PRODUCTS.FOSS_CATCH cc
ON sv.SPECIES_CODE = cc.SPECIES_CODE
AND hh.HAULJOIN = cc.HAULJOIN
WHERE cc.WEIGHT_KG > 0" ) %>%
  janitor::clean_names()

# Save table to local directory
# https://stackoverflow.com/questions/70503726/warning-lazydata-db-of-mb-without-lazydatacompression-set
save(public_data, file ="./data/public_data.rda", compress = "xz")

column <- metadata_colname %>%
  dplyr::filter(metadata_colname %in% toupper(names(public_data))) %>%
  dplyr::mutate(metadata_colname = tolower(metadata_colname)) %>%
  dplyr::distinct()

str0 <- paste0("#' @title Presence-only public data from FOSS
#' @description ",metadata_table_comment$COMMENT[metadata_table_comment$TABLE_NAME == "FOSS_CATCH"],"
#' @usage data('public_data')
#' @author Emily Markowitz (Emily.Markowitz AT noaa.gov)
#' @format A data frame with ",nrow(public_data)," observations on the following ",
               ncol(public_data)," variables.
#' \\describe{
",
               paste0(paste0("#'   \\item{\\code{",column$metadata_colname,"}}{", column$metadata_colname_long, ". ", column$metadata_colname_desc,"}"), collapse = "\n"),
               "#'   }
#' @source https://github.com/afsc-gap-products/gap_products and https://www.fisheries.noaa.gov/foss/f?p=215:28:14951401791129:::::
#' @keywords species code data
#' @examples
#' data(public_data)
#' @details The Resource Assessment and Conservation Engineering (RACE) Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess the populations of demersal fish and crab stocks of Alaska.

'public_data'")

write.table(str0,
            file = here::here("R","public_data.R"),
            sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

## Station centroid data -------------------------------------------------------

# devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
library(akgfmaps)

sel_region <- c("ai", "goa", "ebs", "nbs")
stn_col <- c("ID", "ID", "STATIONID", "STATIONID")

station_coords <- data.frame()

for(ii in 1:length(sel_region)) {
  map_layers <- akgfmaps::get_base_layers(select.region = sel_region[ii], set.crs = "EPSG:3338")

  station_center <- map_layers$survey.grid |>
    sf::st_make_valid() |>
    sf::st_centroid() |>
    sf::st_transform(crs = "WGS84")

  station_center <- data.frame(station = station_center[[stn_col[ii]]]) |>
    dplyr::bind_cols(sf::st_coordinates(station_center)) |>
    dplyr::rename(longitude_dd = X, latitude_dd = Y) |>
    dplyr::mutate(srvy = toupper(sel_region[ii]))

  station_coords <- station_coords |>
    dplyr::bind_rows(station_center)
}

station_coords <- station_coords %>%
  dplyr::filter(!is.na(srvy)) %>%
  dplyr::filter(!is.na(station)) %>%
  dplyr::filter(!is.na(longitude_dd)) %>%
  dplyr::filter(!is.na(latitude_dd))

save(station_coords, file = "./data/station_coords.rda", compress = "xz")

column <- metadata_colname %>%
  dplyr::filter(metadata_colname %in% toupper(names(station_coords))) %>%
  dplyr::mutate(metadata_colname = tolower(metadata_colname)) %>%
  dplyr::distinct()

table <- "Station centroid coordinates for each station for all surveys, as defined by the {akgfmaps} package. "

str0 <- paste0("#' @title Station centroid locations for each station from akgfmaps
#' @description ",table,"
#' @usage data('station_coords')
#' @author Sean Rohan (sean.rohan AT noaa.gov)
#' @format A data frame with ",nrow(station_coords)," observations on the following ",ncol(station_coords)," variables.
#' \\describe{
",
               paste0(paste0("#'   \\item{\\code{",column$metadata_colname,"}}{", column$metadata_colname_long, ". ", column$metadata_colname_desc,"}"), collapse = "\n"),
               "#'   }
#' @source https://github.com/afsc-gap-products/akgfmaps
#' @keywords station survey data
#' @examples
#' data(station_coords)
#' @details Find code to create this table in ./inst/run.R
'station_coords'")

write.table(str0, file = "./R/station_coords.R", sep = "\t",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

## Taxonomic data --------------------------------------------------------------

species_data <- RODBC::sqlQuery(
  channel = channel,
  query =
    "SELECT *
FROM GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
WHERE SURVEY_SPECIES = 1" ) %>%
  janitor::clean_names() %>%
  dplyr::select(species_code,
                common_name,
                scientific_name = species_name)

save(species_data, file = "./data/species_data.rda", compress = "xz")

column <- metadata_colname %>%
  dplyr::filter(metadata_colname %in% toupper(names(species_data))) %>%
  dplyr::mutate(metadata_colname = tolower(metadata_colname)) %>%
  dplyr::distinct()

str0 <- paste0("#' @title Subsetted species data
#' @description ",metadata_table_comment$COMMENT[metadata_table_comment$TABLE_NAME == "TAXONOMIC_CLASSIFICATION"],"
#' @usage data('species_data')
#' @author Sarah Friedman (sarah.friedman AT noaa.gov)
#' @format A data frame with ",nrow(species_data)," observations on the following ",
               ncol(species_data)," variables.
#' \\describe{
",
paste0(paste0("#'   \\item{\\code{",column$metadata_colname,"}}{", column$metadata_colname_long, ". ",
              column$metadata_colname_desc,"}"), collapse = "\n"),
"#' }
#' @source https://github.com/afsc-gap-products/gap_products
#' @keywords species code data
#' @examples
#' data(species_data)
#' @details The Resource Assessment and Conservation Engineering (RACE) Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess the populations of demersal fish and crab stocks of Alaska.

'species_data'")

write.table(str0,
            file = here::here("R","species_data.R"),
            sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

# README -----------------------------------------------------------------------

library(here)
library(devtools)
library(usethis)
library(roxygen2)
library(RODBC)
library(gapctd)
library(akgfmaps)
library(magrittr)
library(readr)
library(dplyr)
library(pkgdown)
library(gapindex)

rmarkdown::render(here::here("inst", "r", "README.Rmd"),
                  output_dir = "./",
                  output_file = "README.md")

# Document and create Package --------------------------------------------------
.rs.restartR()

Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.3.1/bin;', Sys.getenv('PATH')))
library(here)
library(devtools)
library(usethis)
library(roxygen2)
library(RODBC)
devtools::document()
setwd("..")
install("GAPsurvey")
3
setwd(here::here())
# devtools::check()

## Create Documentation GitHub-Pages -------------------------------------------

.rs.restartR()
library(fontawesome) # # devtools::install_github("rstudio/fontawesome", force = T)
library(here)
library(usethis)
library(pkgdown)

# devtools::install_github("r-lib/pkgdown")
# pkgdown::build_favicons()
# devtools::build_vignettes()
# usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")
# usethis::use_vignette("my-vignette")
# pkgdown::clean_site()
pkgdown::build_site(pkg = here::here())
# usethis::use_github_action("pkgdown")

# Save Package tar.gz
date0 <- "2024.10.02"
devtools::build(path = here::here(paste0("GAPsurvey_",date0,".tar.gz")))


