# R Package "GAPsurvey"
# Example Script
# Last updated April 2023

# Notes ------------------------------------------------------------------------

# Open Rstudio script "example_script.R" (should be located on desktop,
# if you have this opened in R, you are already here! This is the example_script.R file!)

# Load Packages (only need to do this once) ------------------------------------

# install R package ------------------------------------------------------------

# rerun this only when there is a new version of the package to install

# devtools::install_github("afsc-gap-products/GAPsurvey")
# or
install.packages('C:/Users/User/Downloads/GAPsurvey_2023.04.01.tar.gz',
                 repos=NULL, type='source')

# Load libraries as needed -----------------------------------------------------

library("GAPsurvey")

## What have we historically caught at this station? ---------------------------

# Learn more about and find examples for functions in GAPsurvey using...
# ?get_catch_haul_history

get_catch_haul_history(
  # years = 2021, # optional; if you only want to see a specific year, not the last 10
  # species_codes = c(21720, 21740), # optional; pacific cod and walleye pollock ONLY
  survey = "EBS", # for example
  station = "I-13") # for example

## What time is sunrise and sunset? --------------------------------------------

## Learn more about and find examples for functions in GAPsurvey using...
# ?get_sunrise_sunset

get_sunrise_sunset(chosen_date = Sys.Date(),
                  survey = "EBS",
                  station = "I-13")

## other examples:
# get_sunrise_sunset(chosen_date = "2023-06-10",
#                    survey = "GOA",
#                    station = "7-7")
# get_sunrise_sunset(chosen_date = "2023-06-10",
#                    latitude = 52.6,
#                    longitude = -169.4364)


## Convert CTD data to BTD as a backup for SBE39 (aka 'the BT') ----------------

convert_ctd_btd(
  filepath_hex = system.file(paste0("exdata/convert_ctd_btd/",
                                    "SBE19plus_01908106_2023_06_18_0001.hex"),
                             package = "GAPsurvey"),
  filepath_xmlcon = system.file(paste0("exdata/convert_ctd_btd/",
                                       "SBE19plusV2_8106_ph_DO_leg2.xmlcon"),
                                package = "GAPsurvey"),
  VESSEL = 162,
  CRUISE = 202301,
  HAUL = 97,
  latitude = 59.01693, # Approximate - for depth estimation
  MODEL_NUMBER = "",
  VERSION_NUMBER = "",
  SERIAL_NUMBER = 8106)
