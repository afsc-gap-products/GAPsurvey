# R Package "GAPsurvey"
# Catch, Length, and Specimen Data
# Import Program Instructions
# Last updated August 2022

# Notes ------------------------------------------------------------------------

# Open Rstudio script "importCatchData.R" (should be located on desktop,
# if you have this opened in R, you are already here! This is the importCatchData.R file!)

# Load Packages (only need to do this once) ------------------------------------

# Load libraries as needed -----------------------------------------------------

library("GAPsurvey")

## What have we historically caught at this station? ---------------------------

# Learn more about and find examples for functions in GAPsurvey using...
# ?get_catch_haul_history # look here for examples of how to use this function

get_catch_haul_history(
  # years = 2021, # optional; if you only want to see a specific year, not the last 10
  # species_codes = c(21720, 21740), # optional; pacific cod and walleye pollock ONLY
  survey = "EBS", # for example
  station = "I-13") # for example
