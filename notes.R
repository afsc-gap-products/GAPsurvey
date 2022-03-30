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


#' @title PolySpecies Data Set
#' @description 
#' #' # library(GAPsurvey)
#' # dsnDataEnt <- "C:/TEMPEDIT/CATCH"
#' # PolySpecies <- selectDataEnt(dsnDataEnt, "select distinct a.species_code, a.poly_species_code, b.species_name, b.common_name
#' #     from LENGTH_WEIGHT_PARAMETERS as a, species_list as b
#' #     where a.SPECIES_CODE = b.SPECIES_CODE")
#' # names(PolySpecies) <- toupper(names(PolySpecies))
#' # devtools::use_data(PolySpecies, overwrite=T)
#' @usage data(PolySpecies)
#' @author Jason Conner (jason.conner AT noaa.gov)
#' @format A data frame with 172 rows and 4 variables:
#' \describe{
#'   \item{\code{SPECIES_CODE}}{integer Species code}
#'   \item{\code{POLY_SPECIES_CODE}}{integer Poly species code}
#'   \item{\code{SPECIES_NAME}}{character Species scientific latin name}
#'   \item{\code{COMMON_NAME}}{character Species common names} 
#'}
#' @details DETAILS
#' @keywords catch data
#' @examples
#' data(PolySpecies)
"PolySpecies"


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



#' @title species data codes
#' @description RACEBASE Species Codes and Scientific Names Data Set
#' @usage data(sp_table)
#' @author Margaret Siple (margaret.siple AT noaa.gov)
#' @format A data frame with 2754 rows and 2 variables:
#' \describe{
#'   \item{\code{species_code}}{integer Species code}
#'   \item{\code{report_name_scientific}}{Species scientific names} 
#'}
#' @details DETAILS
#' @keywords species scientific code data
#' @examples
#' data(sp_table)
"sp_table"



#' @title Public data from FOSS
#' @description
#' @usage data("public_data")
#' @author Emily Markowitz (emily.markowitz AT noaa.gov)
#' @format A data frame with 878805 observations on the following 33 variables.
#' \describe{
#'   \item{\code{year}}{a numeric vector; Year the survey was conducted in. }
#'   \item{\code{srvy}}{a character vector; Abbreviated survey names. The column “srvy” is associated with the “survey” and “survey_id” columns. Northern Bering Sea (NBS), Southeastern Bering Sea (EBS), Bering Sea Slope (BSS), Gulf of Alaska (GOA), Aleutian Islands (AI). }
#'   \item{\code{survey}}{a character vector; Name and description of survey. The column “survey” is associated with the “srvy” and “survey_id” columns. }
#'   \item{\code{survey_id}}{a numeric vector; This number uniquely identifies a survey. Name and description of survey. The column “survey_id” is associated with the “srvy” and “survey” columns. For a complete list of surveys go to https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual}
#'   \item{\code{cruise}}{a numeric vector; This is a six-digit number identifying the cruise number of the form: YYYY99 (where YYYY = year of the cruise; 99 = 2-digit number and is sequential; 01 denotes the first cruise that vessel made in this year, 02 is the second, etc.)}
#'   \item{\code{haul}}{a numeric vector; This number uniquely identifies a sampling event (haul) within a cruise. It is a sequential number, in chronological order of occurrence.}
#'   \item{\code{stratum}}{a numeric vector; RACE database statistical area for analyzing data. Strata were designed using bathymetry and other geographic and habitat-related elements. The strata are unique to each survey series. Stratum of value 0 indicates experimental tows. }
#'   \item{\code{station}}{a character vector; Alpha-numeric designation for the station established in the design of a survey. }
#'   \item{\code{vessel_name}}{a character vector; Name of the vessel used to collect data for that haul. The column “vessel_name” is associated with the “vessel_id” column. Note that it is possible for a vessel to have a new name but the same vessel id number. For a complete list of vessel ID codes: https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual}
#'   \item{\code{vessel_id}}{a numeric vector; ID number of the vessel used to collect data for that haul. The column “vessel_id” is associated with the “vessel_name” column. Note that it is possible for a vessel to have a new name but the same vessel id number. For a complete list of vessel ID codes: https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual}
#'   \item{\code{date_time}}{a character vector; The date (MM/DD/YYYY) and time (HH:MM) of the beginning of the haul. }
#'   \item{\code{latitude_dd}}{a numeric vector; Latitude (one hundred thousandth of a decimal degree) of the start of the haul.}
#'   \item{\code{longitude_dd}}{a numeric vector; Longitude (one hundred thousandth of a decimal degree) of the start of the haul.}
#'   \item{\code{species_code}}{a numeric vector; The species code of the organism associated with the “common_name” and “scientific_name” columns. For a complete species list go to https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual}
#'   \item{\code{common_name}}{a character vector; The common name of the marine organism associated with the “scientific_name” and “species_code” columns. For a complete species list go to https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual}
#'   \item{\code{scientific_name}}{a character vector; The scientific name of the organism associated with the “common_name” and “species_code” columns. For a complete taxon list go to https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual}
#'   \item{\code{taxon_confidence}}{a character vector; Confidence in the ability of the survey team to correctly identify the taxon to the specified level, based solely on identification skill (e.g., not likelihood of a taxon being caught at that station on a location-by-location basis). Quality codes follow: “High”: High confidence and consistency. Taxonomy is stable and reliable at this level, and field identification characteristics are well known and reliable. “Moderate”: Moderate confidence. Taxonomy may be questionable at this level, or field identification characteristics may be variable and difficult to assess consistently. “Low”: Low confidence. Taxonomy is incompletely known, or reliable field identification characteristics are unknown. Species identification confidence in the eastern Bering Sea shelf survey (1982-2008): http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2009-04.pdf Species identification confidence in the eastern Bering Sea slope survey (1976-2010): http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-05.pdf Species identification confidence in the Gulf of Alaska and Aleutian Islands surveys (1980-2011): http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-01.pdf}
#'   \item{\code{cpue_kgha}}{a numeric vector; Relative Density. Catch weight (kilograms) divided by area (hectares) swept by the net. }
#'   \item{\code{cpue_kgkm2}}{a numeric vector; Relative Density. Catch weight (kilograms) divided by area (squared kilometers) swept by the net. }
#'   \item{\code{cpue_kg1000km2}}{a numeric vector; Relative Density. Catch weight (kilograms) divided by area (thousand square kilometers) swept by the net. }
#'   \item{\code{cpue_noha}}{a numeric vector; Relative Abundance. Catch number (in number of organisms) per area (hectares) swept by the net. }
#'   \item{\code{cpue_nokm2}}{a numeric vector; Relative Abundance. Catch number (in number of organisms) per area (squared kilometers) swept by the net. }
#'   \item{\code{cpue_no1000km2}}{a numeric vector; Relative Abundance. Catch weight (in number of organisms) divided by area (thousand square kilometers) swept by the net.}
#'   \item{\code{weight_kg}}{a numeric vector; Weight (thousandths of a kilogram) of individuals in a haul by taxon. }
#'   \item{\code{count}}{a numeric vector; Total number of individuals caught in haul by taxon, represented in whole numbers. }
#'   \item{\code{bottom_temperature_c}}{a numeric vector; Bottom temperature (tenths of a degree Celsius); NA indicates removed or missing values.}
#'   \item{\code{surface_temperature_c}}{a numeric vector; Surface temperature (tenths of a degree Celsius); NA indicates removed or missing values. }
#'   \item{\code{depth_m}}{a numeric vector; Bottom depth (tenths of a meter).}
#'   \item{\code{distance_fished_km}}{a numeric vector; Distance the net fished (thousandths of kilometers). }
#'   \item{\code{net_width_m}}{a numeric vector; Measured or estimated distance (meters) between wingtips of the trawl.}
#'   \item{\code{net_height_m}}{a numeric vector; Measured or estimated distance (meters) between footrope and headrope of the trawl.}
#'   \item{\code{area_swept_ha}}{a numeric vector; The area the net covered while the net was fishing (hectares), defined as the distance fished times the net width. }
#'   \item{\code{duration_hr}}{a numeric vector; This is the elapsed time between start and end of a haul (decimal hours). }
#'   }
#' @source FOSS
#' @references REF
#' @keywords species scientific code data
#' @examples
#' data(public_data)
#' @details DETAILS
"public_data"

