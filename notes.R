# Good to know links:

# The computer we usefor the catch comptuer
# https://www.onlogic.com/ml350g-10/?cpsrc=StandardShoppingB&kw=&gclid=Cj0KCQjwo-aCBhC-ARIsAAkNQivOtLBIFqE1ZJ2Qch0B8HfpDnG0dYjGhwn88wNUINasV2rjACuXHrcaAi5PEALw_wcB

# test data:
# https://drive.google.com/drive/folders/1EyXHy1AO6jWppUi0qI85YLhCcanIo-x4?usp=sharing
# https://drive.google.com/drive/folders/13ahk5y7ygEcTSfDWXPEQn42vP2QUJV_C?usp=sharing

# 2021 GAP Survey CTD Team
# https://drive.google.com/drive/folders/1XqWqrvaq1ZAhrZZzxy9NfSlgEDeJRn9G?usp=sharing


# Data
# C:\Users\emily.markowitz\Documents\Projects\GAPsurvey_general\GAPsurvey\inst\exdata\catch\GOA
# G:\GOA\GOA 2019\DATA_2019\Ocean Explorer

# example of basket weight thing
# G:\ALEUTIAN\AI 2018\Data\Sea Storm\Leg 2\CATCH\OriginalData

# Things to fix in 2022
# 1. LOGtoGPS() fix 4pm double file problem
# 2. make CTD .hex and .xml files inputs into CTDtoBTD
# 
# gitlab vs github
# 
# Thoughts on GAPsurvey being on GitLab vs. GitHub? I would advocate for GitHub, but it's up to what you think is best. I don't think anything in the package is sensitive. It has also been so useful to share links to code with people in the group to make sure everything works like it should and see where improvements can be made. The only thing that might be debatably sensitive would be the empty data_ent.mbd file? I can also remove that file from the package if that is a concern. 
# In all honesty I'm not worried about this being public.  The data that's even in it is raw and the package doesn't have much utility outside of our program.   But, I need some education here, I thought we could share between our users using GitLab?
# Totally agree. You're right about GitLab, but the platform can seem a bit exclusionary to people unfamiliar with the platform. Its not actually that hard to access, but looks harder than it is. I imagine that if I had shared the GitLab link today for the package, probably only 5 or so people would have known how to access it/thought they could access it. 
# The issue mostly comes down to educating people about the platform, but that is still an issue. 
# Ok.  Thanks.  That was what I remembered but the rules have been changing on this stuff fast.  The next question is, what GitLab account?  I know at one point we were debating the creation of one for GAP...but then we sort of need an administrator.
# ah that's a bit more complicated of an answer. Right now it is on Jason's GitLab and my GitHub (should be updated on both, but i've needed some IT help to connect to GitLab). There isnt currently a GAP Git* account and I think there needs to be a bit more discussion about how to do that. 
# maybe that was less complicated to explain than I thought haha
# Maybe we need to have this be a topic in a group meeting.  Especially for something like this package which has multiple authorship.  Especially since updating and maintaining packages seems to typically fall on the owner of the storage hub.
# I think you're exactly right - Creating the physical account is easy, but no one wants to be the only person at the helm and we need to come up with a plan to work on it together. 
# In this case, I've made Liz, cecilia, megsie, duane, and jason collaborators on the GAPsurvey github page, so if they want to, they will be able to make changes as needed and will get "credit" for making that change, but its on my page as opposed to a more centrailized location
# History is off. Messages may disappear before people see them. Conversations with no messages may disappear.



install.packages("C:/Users/liz.dawson/Desktop/AKKNIGHT2021CATCHHAULS1-7/RODBC_1.3-16.zip", repos = NULL, type = "source")
install.packages("C:/Users/liz.dawson/Work/R_Code/gapsurvey_20210607/GAPsurvey_2.4.00.tar.gz", repos = NULL, type = "source")
dsnTablet <- "C:/Users/liz.dawson/Desktop/AKKNIGHT2021CATCHHAULS1-7"
dsnDataEnt <- "C:/Users/liz.dawson/Desktop/AKKNIGHT2021CATCHHAULS1-7/data_ent.mdb"



############## QUESTIONS ####################

# git rm -r --cached .

########### Document Package ############
.rs.restartR()

# options(rmarkdown.html_vignette.check_title = FALSE)
Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.3.1/bin;', Sys.getenv('PATH')))
library(here)
library(devtools)
library(roxygen2)
devtools::document()
setwd("..")
install("GAPsurvey")
3
setwd(here::here())
# devtools::check()

########### Create Documentation GitHub-Pages ############

.rs.restartR()
# devtools::install_github("rstudio/fontawesome", force = T)
# library(fontawesome)
library(here)
library(usethis)
library(pkgdown)
rmarkdown::render(input = "README.Rmd",
                  output_dir = "./",
                  output_file = "README.md")


# devtools::install_github("r-lib/pkgdown")
# pkgdown::build_favicons()
# devtools::build_vignettes()
usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")

pkgdown::build_site(pkg = here::here())
# usethis::use_github_action("pkgdown")

# Save Package tar.gz
date0 <- "2022.04.01"
file.remove(paste0(dirname(here::here()), "/GAPsurvey_",date0,".tar.gz"))
file.remove(paste0((here::here()), "/GAPsurvey_",date0,".tar.gz"))
devtools::build()
file.copy(from = paste0(dirname(here::here()), "/GAPsurvey_",date0,".tar.gz"), 
          to = paste0(here::here(), "/GAPsurvey_",date0,".tar.gz"),
          overwrite = TRUE)

########### Create Documentation GitHub-Pages ############

.rs.restartR()

# git rm -r --cached .
# git reset --hard HEAD^^

# devtools::install_github("rstudio/fontawesome", force = T)
# library(fontawesome)
# library(here)
# library(usethis)
# library(pkgdown)
# options(rmarkdown.html_vignette.check_title = FALSE)

########### Submit to CRAN ############

# devtools::check() # add the console output to cran-commentes.md
# devtools::check_rhub() # check that your email is 'validated.' You'll need a token emailed to you
# devtools::check_win_devel()
# devtools::release()

##########build a vignette#########

# usethis::use_vignette("testfunct")


# loading qpdf:
# https://stackoverflow.com/questions/41570633/how-to-build-qpdf-on-windows
# https://stackoverflow.com/questions/15035150/r-cmd-check-as-cran-warning
# https://sourceforge.net/projects/qpdf/files/qpdf/10.0.1/
# Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.0.1/bin;', Sys.getenv('PATH')))
# Sys.which(Sys.getenv("R_QPDF", "qpdf"))


CTDtoBTD(
  VESSEL = 94,
  CRUISE = 2001,
  HAUL = 4,
  MODEL_NUMBER = 123,
  VERSION_NUMBER = 456,
  SERIAL_NUMBER = 789,
  path_in = system.file("exdata/ctd2btd/SBE19plus_01908103_2021_06_01_94_0004_raw.cnv", package = "GAPsurvey"),   
  path_out = getwd(),
  filename_add = "newctd", 
  quiet = TRUE)


LOGtoGPS(
  VESSEL = 94,
  CRUISE = 202101,
  HAUL = 37,
  DATE = "06/07/2021",
  path_in = "C:/Users/emily.markowitz/Documents/Projects/GAPsurvey_general/GAPsurvey/inst/exdata/log2gps/Haul0037.log",
  path_out = getwd(),
  filename_add = "newlog",
  quiet = TRUE)


#' Find catch data from previous years
#'
#' @param histdat (dataframe) A dataframe containing historical survey data
#' @param sptable (dataframe) A dataframe containing speices codes and scientific names
#' @param station_query (character) A character string of the current station name (as a grid cell; e.g., "264-85")
#' @param grid_buffer (numeric) The number of cells around the current station where you would like to see catches from
#' @param topn (numeric) The number of rows of top catches in that area that you would like to see (e.g., the top 3 species in terms of catch or top 10)
#'
#' @return a data.frame of past catches
#' @export
#'
#' @examples
#' data(sp_table, package = "GAPsurvey")
#' data("local_racebase", package = "GAPsurvey")
#' get_station_history(
#'    histdat, 
#'    sptable, 
#'    station_query = "264-150", 
#'    grid_buffer = 3, 
#'    topn = 10)
get_station_history <- function(
    histdat,
    sptable,
    station_query = "264-150",
    grid_buffer = 3,
    topn = 10) {

  # y <- as.numeric(stringr::str_split(
  #   station_query,
  #   pattern = "-",
  #   simplify = TRUE))
  y <- as.numeric(strsplit(x = station_query, split = "-", fixed = TRUE)[[1]])

  if (grid_buffer != 3) {
    stop("the grid cell buffer is fixed at 3 for now.")
  }
  possible_stations <- expand.grid(
    data.frame(
      rbind(
        y + grid_buffer,
        y + grid_buffer - 1,
        y + grid_buffer - 2,
        y,
        y - grid_buffer,
        y - grid_buffer - 1,
        y - grid_buffer - 2
      )
    )
  )

  possible_stations$stationid <- paste(possible_stations$X1,
                                       possible_stations$X2,
                                       sep = "-"
  )

  x <- subset(x = histdat, stationid %in% possible_stations$stationid)

  xx <- merge(x, sptable, by = "species_code", all.x = TRUE)

  aa <- stats::aggregate(xx[, c("number_fish", "weight")],
                         by = list(
                           haul = factor(xx$haul),
                           year = factor(xx$year),
                           report_name_scientific = factor(xx$report_name_scientific),
                           station_id = factor(xx$stationid)
                         ),
                         sum
  )

  names(aa)[names(aa) == "weight"] <- "total_catch_kg"
  aa$year <- as.numeric(as.character(aa$year))
  aa <- aa[order(-aa$year, -aa$total_catch_kg), ]
  bb <- split(aa, aa$year)
  cc <- lapply(bb, function(df) {
    utils::head(df[order(-df$year, -df$total_catch_kg), ], n = topn)
  })

  return(cc)
}






#' @title RACEBASE Data Set
#' @description One dataframe containing all the racebase tables (experimental: this might be a bad idea)
#' @usage data(local_racebase)
#' @author Margaret Siple (margaret.siple AT noaa.gov)
#' @format A data frame with 1613690 rows and 38 variables:
#' \describe{
#'   \item{\code{cruisejoin}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{hauljoin}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{region}}{character Cruise join var}
#'   \item{\code{vessel}}{integer ID number of the vessel used to collect data for that haul. The column “vessel_id” is associated with the “vessel_name” column. Note that it is possible for a vessel to have a new name but the same vessel id number. For a complete list of vessel ID codes: https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual}
#'   \item{\code{cruise}}{integer This is a six-digit number identifying the cruise number of the form: YYYY99 (where YYYY = year of the cruise; 99 = 2-digit number and is sequential; 01 denotes the first cruise that vessel made in this year, 02 is the second, etc.)}
#'   \item{\code{haul}}{integer Haul number}
#'   \item{\code{haul_type}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{performance}}{double COLUMN_DESCRIPTION}
#'   \item{\code{start_time}}{character The date (MM/DD/YYYY) and time (HH:MM) of the beginning of the haul. }
#'   \item{\code{duration}}{double This is the elapsed time between start and end of a haul (decimal hours). }
#'   \item{\code{distance_fished}}{double Distance the net fished (thousandths of kilometers). }
#'   \item{\code{net_width}}{double Measured or estimated distance (meters) between wingtips of the trawl.}
#'   \item{\code{net_measured}}{character COLUMN_DESCRIPTION}
#'   \item{\code{net_height}}{double Measured or estimated distance (meters) between footrope and headrope of the trawl.}
#'   \item{\code{stratum}}{integer RACE database statistical area for analyzing data. Strata were designed using bathymetry and other geographic and habitat-related elements. The strata are unique to each survey series. Stratum of value 0 indicates experimental tows. }
#'   \item{\code{start_latitude}}{double Latitude (one hundred thousandth of a decimal degree) of the start of the haul.}
#'   \item{\code{end_latitude}}{double Latitude (one hundred thousandth of a decimal degree) of the end of the haul.}
#'   \item{\code{start_longitude}}{double Longitude (one hundred thousandth of a decimal degree) of the start of the haul.}
#'   \item{\code{end_longitude}}{double Longitude (one hundred thousandth of a decimal degree) of the end of the haul.}
#'   \item{\code{stationid}}{character Alpha-numeric designation for the station established in the design of a survey. }
#'   \item{\code{gear_depth}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{bottom_depth}}{integer Bottom depth (tenths of a meter).}
#'   \item{\code{bottom_type}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{surface_temperature}}{double COLUMN_DESCRIPTION}
#'   \item{\code{gear_temperature}}{double COLUMN_DESCRIPTION}
#'   \item{\code{wire_length}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{gear}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{accessories}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{subsample}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{abundance_haul}}{character COLUMN_DESCRIPTION}
#'   \item{\code{AreaSwept_km2}}{double COLUMN_DESCRIPTION}
#'   \item{\code{catchjoin}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{species_code}}{integer The species code of the organism associated with the “common_name” and “scientific_name” columns. For a complete species list go to https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual}
#'   \item{\code{weight}}{double Weight (thousandths of a kilogram) of individuals in a haul by taxon. }
#'   \item{\code{number_fish}}{double Total number of individuals caught in haul by taxon, represented in whole numbers. }
#'   \item{\code{subsample_code}}{logical COLUMN_DESCRIPTION}
#'   \item{\code{voucher}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{year}}{character Year the survey was conducted in.} 
#'}
#' @keywords catch data
#' @examples
#' data(local_racebase)
#' @details DETAILS
"local_racebase"

