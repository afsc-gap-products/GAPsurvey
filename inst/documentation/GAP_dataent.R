## Make sure the necessary packages are installed ##
install.packages("C:/R/GAPsurvey_2.1.4.zip", repos = NULL, type = "win.binary")
install.packages("C:/R/RODBC_1.3-15.zip", repos = NULL, type = "source")

# Load package libraries #
library("GAPsurvey", "RODBC", "tidyverse")

# Point functions to correct directories #
dsnTablet <- "C:/Users/duane.stevenson/Work/Bluetooth"
dsnDataEnt <- "C:/Users/duane.stevenson/Work/Data/data_ent.mdb"
lengthData <- T  ## Change this "T" to "F" if not importing length data ##

# Define the current haul number to import #
haul <- 999

# Import catch, length, and specimen data #
catchData(haul,dsnTablet,dsnDataEnt,lengthData)
specimenData(haul,dsnTablet,dsnDataEnt)


##After defining your current haul number and running the import script, 
##open DataEnt to complete the calculations and check the data. 

##In DataEnt:
##Click "EDIT", then "EDIT CATCH"
##Select current haul from the dropdown menu
##Tab through each line of the catch and mixture data to complete extrapolation calculations
##Visually inspect the data for accuracy
##Click "QUIT" and "Yes" to exit
##Click "EDIT LENGTH" and inspect the length data for completeness and accuracy
##Click "EDIT SPECIMEN" and inspect the specimen data for completeness and accuracy
