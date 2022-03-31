# R Package "GAPsurvey"
# Catch, Length, and Specimen Data 
# Import Program Instructions
# Last updated April 2022


# Notes ------------------------------------------------------------------------


# Open Rstudio script "importCatchData.R" (should be located on desktop,
# if you have this opened in R, you are already here! This is the importCatchData.R file!)

# Below is the annotated script for importing catch, length, and specimen 
# data using the GAPsurvey package:

#---> 32-bit R Studio is required to run this program
#
#---> To use the program you must first:
#  Bluetooth transfer the following files from tablets to catch computer 
# (tablet files should automatically go into C:Users/NOAADATA/Documents/Bluetooth/inbox)
#   [each of these file names will begin with the tablet name and end with 
#   the haul number]
#     DATAENT_CATCH_xxxx.csv
#     DATAENT_BBAG_xxxx.csv     -- If there was a benthic bag
#     RAW_BBAG_xxxx.csv         -- If there was a benthic bag
#     RAW_CATCH_HAUL_xxxx.csv
#     RAW_CATCH_HAUL_ATTR_xxxx.csv
#     RAW_CATCH_SAMPLE_xxxx.csv
#     RAW_CATCH_VALUE_xxxx.csv
#     SPECIMEN_xxxx.csv         -- The specimen data
#     HAUL_xxxx.csv             -- A length file for each tablet


# Load Packages (only need to do this once) ------------------------------------

## Make sure the necessary packages are installed ##
install.packages("C:/R/RODBC_1.3-16.zip", repos = NULL, type = "source")
install.packages("C:/R/GAPsurvey_2022.04.01.tar.gz", repos = NULL, type = "source")

# Load libraries as needed -----------------------------------------------------

library("GAPsurvey", "RODBC")

## What have we historically caught at this station? ---------------------------

# Learn more about and find examples for functions in GAPsurvey using...
# ?get_catch_haul_history # look here for examples of how to use this function

get_catch_haul_history( 
  # years = 2021, # optional; if you only want to see a specific year, not the last 10
  # species_codes = c(21720, 21740), # optional; pacific cod and walleye pollock ONLY
  survey = "EBS", # for example
  station = "I-13") # for example


## Modify data_ent.mdb ---------------------------------------------------------

# Learn more about and find examples for functions in GAPsurvey using...
# ?catchData
# ?specimenData
# ?deleteDataEnt

# Point functions to correct tablet directory and "dataEnt" file
dsnTablet <- "C:/Users/NOAADATA/Documents/Bluetooth/inbox"
dsnDataEnt <- "C:/Data/data_ent.mdb"
importLength <- TRUE ## Change this "T" to "F" if not importing length data ##

# Define the current haul number to import #
haul <- 59

# Import catch, length, and specimen data #
catchData(haul,dsnTablet,dsnDataEnt,importLength)
specimenData(haul,dsnTablet,dsnDataEnt)
# benthicData(haul,dsnTablet,dsnDataEnt) # GOA and AI surveys only


# Delete previously loaded data from data_ent.mdb (use if data was partially 
# transferred and trying to re-transfer all data).
dMix <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
                      tablename = 'MIXTURE')
dMixHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
                          tablename = 'MIXTURE_HEADER')
dCatch <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
                        tablename = 'CATCH')
dCatchHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
                            tablename = 'CATCH_HEADER')
dLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
                         tablename = 'LENGTH')
dRawLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = 
                              haul, tablename = 'RAW_LENGTH')
# Specimen data must be manually deleted. 


# After defining your current haul number and running the import script, 
# open DataEnt to complete the calculations and check the data. 
# 
#In DataEnt:
#  Click "EDIT", then "EDIT CATCH"
#  Select current haul from the dropdown menu
#  Tab through each line of the catch and mixture data to complete extrapolation 
#   calculations
#  Visually inspect the data for accuracy
#  Click "QUIT" and "Yes" to exit
#  Click "EDIT LENGTH" and inspect the length data for completeness and accuracy
#  Click "EDIT SPECIMEN" and inspect the specimen data for completeness and accuracy


## Convert other sources to wheelhouse formats (.BTD/.BTH) ---------------------


# For conversion of wheelhouse data run the appropriate function:

# Learn more about and find examples for functions in GAPsurvey using...
# ?CTDtoBTD
# ?TEDtoBTD
# ?LOGtoGPS

# CTDtoBTD()
# TEDtoBTD()
# LOGtoGPS()
