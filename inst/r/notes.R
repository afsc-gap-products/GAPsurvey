

# Load new FOSS data -----------------------------------------------------------

# This has a specific username and password because I DONT want people to have access to this!
source("https://raw.githubusercontent.com/afsc-gap-products/metadata/main/code/functions_oracle.R")

locations <- c("Z:/Projects/ConnectToOracle.R",
               "C:/Users/emily.markowitz/Work/Projects/ConnectToOracle.R")
for (i in 1:length(locations)){
  if (file.exists(locations[i])){
    source(locations[i])
  }
}

locations<-c(
  # "RACEBASE_FOSS.FOSS_CPUE_PRESONLY",
  # "GAP_PRODUCTS.OLD_TAXONOMICS_WORMS",
  "GAP_PRODUCTS.METADATA_COLUMN"
)

oracle_dl(
  locations = locations,
  channel = channel,
  dir_out = paste0("../notforgit/"))

library(magrittr)
library(readr)
library(dplyr)

a<-list.files(path = paste0("../notforgit/"), pattern = ".csv")
for (i in 1:length(a)){
  b <- read_csv(file = paste0("../notforgit/", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
}

## public data -----------------------------------------------------------------\

oracle_dl_metadata(
  locations = "RACEBASE_FOSS",
  channel = channel,
  dir_out = paste0("../notforgit/"))

public_data <- racebase_foss_foss_cpue_presonly0 #%>%
save(public_data, file = "./data/public_data.rda")

column <- gap_products_metadata_column0 %>%
  dplyr::filter(metadata_colname %in% toupper(names(public_data))) %>%
  dplyr::mutate(metadata_colname = tolower(metadata_colname)) %>%
  dplyr::distinct()

# column <- read_csv(file = "../notforgit/metadata_column_current.csv")
# column <- column[column$TABLE_NAME == "FOSS_CPUE_PRESONLY",]
# column$col_name <- names(public_data)
table <- read_csv(file = "../notforgit/metadata_table_current.csv")

str0 <- paste0("#' @title Public data from FOSS
#' @description ",table$COMMENTS[table$TABLE_NAME == "FOSS_CPUE_PRESONLY"],"
#' @usage data('public_data')
#' @author Emily Markowitz (emily.markowitz AT noaa.gov)
#' @format A data frame with ",nrow(public_data)," observations on the following ",ncol(public_data)," variables.
#' \\describe{
",
paste0(paste0("#'   \\item{\\code{",column$metadata_colname,"}}{", column$metadata_colname_long, ". ", column$metadata_colname_desc,"}"), collapse = "\n"),
"#'   }
#' @source https://github.com/afsc-gap-products/gap_public_data
#' @keywords species code data
#' @examples
#' data(public_data)
#' @details DETAILS
'public_data'")

write.table(str0, file = "./R/public_data.R", sep = "\t",
            row.names = FALSE, col.names = FALSE, quote = FALSE)


## Station centroid data -------------------------------------------------------

library(akgfmaps)

sel_region <- c("ai", "goa", "ebs", "nbs")
stn_col <- c("ID", "ID", "STATIONID", "STATIONID")

station_coords <- data.frame()

for(ii in 1:length(sel_region)) {
  map_layers <- akgfmaps::get_base_layers(select.region = sel_region[ii], set.crs = "EPSG:4326")

  station_center <- map_layers$survey.grid |>
    sf::st_make_valid() |>
    sf::st_centroid()

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

save(station_coords, file = "./data/station_coords.rda")

column <- gap_products_metadata_column0 %>%
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
#' @details DETAILS
'station_coords'")

write.table(str0, file = "./R/station_coords.R", sep = "\t",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

## Taxonomic data --------------------------------------------------------------

species_data <- GAP_PRODUCTS_OLD_TAXONOMICS_WORMS0 %>%
  dplyr::filter(is.na(reason)) %>%
  dplyr::select(species_code,
                common_name,
                scientific_name = accepted_name,
                scientific_name_old = survey_name)
save(species_data, file = "./data/species_data.rda")

# Document Package ---------------------------------------------------
.rs.restartR()

# options(rmarkdown.html_vignette.check_title = FALSE)
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

# Catch is Windows 10, Wheelhouse is Windows 7

## Create Documentation GitHub-Pages -------------------------------------------

.rs.restartR()
# devtools::install_github("rstudio/fontawesome", force = T)
# library(fontawesome)
library(here)
library(usethis)
library(pkgdown)
rmarkdown::render(input = "./inst/r/README.Rmd",
                  output_dir = "./",
                  output_file = "README.md")


# devtools::install_github("r-lib/pkgdown")
# pkgdown::build_favicons()
# devtools::build_vignettes()
# usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")

pkgdown::build_site(pkg = here::here())
# usethis::use_github_action("pkgdown")

# Save Package tar.gz
date0 <- "2023.04.01"
file.remove(paste0(dirname(here::here()), "/GAPsurvey_",date0,".tar.gz"))
file.remove(paste0((here::here()), "/GAPsurvey_",date0,".tar.gz"))
devtools::build()
file.copy(from = paste0(dirname(here::here()), "/GAPsurvey_",date0,".tar.gz"),
          to = paste0(here::here(), "/GAPsurvey_",date0,".tar.gz"),
          overwrite = TRUE)
