# GAPsurvey

Catch, Length, and Specimen Data Import Program Instructions (R Package "GAPsurvey")


---> 32-bit R Studio is required to run this program

(R 3.6.0 32/64 bit)[https://cran.r-project.org/bin/windows/base/old/3.6.0/]

(https://www.npackd.org/p/rstudio/1.1.463)[RStudio Desktop 1.1.463]

(RTools35 for R 3.3.x to 3.6.x)[https://cran.r-project.org/bin/windows/Rtools/history.html]

    devtools::install.packages("processx") # Is a dependancy of devtools but does not seem to install correctly when devtools is installed.

    devtools::install.packages("devtools"); library(devtools) # to install packages below

    devtools::install_version("RODBC", version = "1.3-16", repos = "http://cran.us.r-project.org"); library(RODBC)

    install.packages("git2r"); library(git2r)
    


---> To use the program you must first:
	Bluetooth transfer all data from tablets (catch, length, and specimen) to catch computer (should automatically go into C:Users/NOAADATA/Documents/Bluetooth/inbox)
	Open Rstudio script "importCatchData.R" (should be located on desktop)

Below is the annotated script for importing catch, length, and specimen data using the GAPsurvey package:

	## Make sure the necessary packages are installed ##
	install.packages("C:/R/GAPsurvey_2.1.4.zip", repos = NULL, type = "win.binary")
	install.packages("C:/R/RODBC_1.3-15.zip", repos = NULL, type = "source")
	install.packages("C:/R/oce_1.3-0.tar.gz", repos = NULL, type = "source") https://cran.r-project.org/src/contrib/Archive/oce/oce_1.3-0.tar.gz

	# Load package libraries #
	library("GAPsurvey", "RODBC", "oce")

	# Point functions to correct directories - Note: for tablet data it is the directory for data_ent include the file name #
	dsnTablet <- "C:/Users/NOAADATA/Documents/Bluetooth/inbox"
	dsnDataEnt <- "C:/Data/data_ent.mdb"
	lengthData <- T  ## Change this "T" to "F" if not importing length data ##

	# Define the current haul number to import #
	haul <- 1

	# Import catch, length, and specimen data #
	catchData(haul,dsnTablet,dsnDataEnt,lengthData)
	specimenData(haul,dsnTablet,dsnDataEnt)
	
	# Import benthic bag data #
	benthicData(haul,dsnTablet,dsnDataEnt)

If an error occurs where there is a mismatch with species in length and catch, the length data should already be uploaded, but 
you will have to re-upload the catch data with 
  lengthData <- F
  catchData(haul,dsnTablet,dsnDataEnt,lengthData)
and enter the subsample numbers manually in the data_ent application afterward.

After successfully running the import script, open DataEnt to complete the calculations and check the data. In DataEnt:
Click "EDIT", then "EDIT CATCH"
Select current haul from the dropdown menu
Tab through each line of the catch and mixture data to complete extrapolation calculations
Visually inspect the data for accuracy
Click "QUIT" and "Yes" to exit
Click "EDIT LENGTH" and inspect the length data for completeness and accuracy
Click "EDIT SPECIMEN" and inspect the specimen data for completeness and accuracy

If you need to delete data from a haul and start over:

    dsnDataEnt <- "C:/Data/data_ent.mdb"
    haul <- 999
    
    dMix <- deleteDataEnt(dsnDataEnt,haul,'MIXTURE')
    dMixHead <- deleteDataEnt(dsnDataEnt,haul,'MIXTURE_HEADER')
    dCatch <- deleteDataEnt(dsnDataEnt,haul,'CATCH')
    dCatchHead <- deleteDataEnt(dsnDataEnt,haul,'CATCH_HEADER')
    dLength <- deleteDataEnt(dsnDataEnt,haul,'LENGTH')
    dRawLength <- deleteDataEnt(dsnDataEnt,haul,'RAW_LENGTH')

  
