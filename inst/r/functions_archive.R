# Work with catch data ----------------------------------------------

#' Check files before import
#'
#' @param haul Number of the haul you want to upload
#' @param dsnTablet String for the drive folder (in R notation) where tablet files exist.
#' @param dsnDataEnt String for the drive folder (in R notation) - include full file name and extension.
#'
#' @return Print statements indicating if any raw data files are missing and which length tablet data are being used.
#' @export
#'
#' @examples
#' dsnTablet <- system.file("exdata/catch/GOA/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/GOA/data_ent.mdb", package = "GAPsurvey")
#'
#' # Define the current haul number
#' haul <- 8
#'
#' # Import length data
#' checkFiles(haul, dsnTablet, dsnDataEnt)
checkFiles <- function(haul, dsnTablet, dsnDataEnt) {
  x <- list.files(path = dsnTablet, pattern = paste0(sprintf("%04d", haul)))
  
  reqd_files <- c("DATAENT_CATCH", "DATAENT_BBAG", "RAW_BBAG", "RAW_CATCH_HAUL", "RAW_CATCH_SAMPLE", "RAW_CATCH_VALUE", "RAW_BBAG", "S_SPECIMEN", "RAW_SPECIMEN")
  
  mf <- sapply(X = reqd_files, function(y) {
    if (any(grepl(pattern = y, x = x))) {
      0
    } else {
      1
    }
  })
  
  if (any(mf == 1)) {
    print(paste("Missing", reqd_files[mf == 1], "file"))
  }
  
  if (!any(grepl(pattern = "_HAUL", x = x))) {
    print("Missing _HAUL files. These are from lengthing tablets (usually named green, orange, etc.)")
  }
  
  # Print which length tablets are being used
  lengthfiles <- list.files(path = dsnTablet, pattern = paste0("_HAUL", sprintf("%04d", haul)))
  cat(
    "Using length data from the following tablets: \n",
    paste(sub("\\_HAUL.*", "", lengthfiles), "\n")
  )
}


#' Import Length Tablet Files Into DataEnt.mdb
#'
#' NOTE: Must be run on 32-bit R!
#' NOTE: This function is typically run within the catchData()
#'
#' Looks for tablet length files in given DSN and uploads them to LENGTH and RAW_LENGTH in the given data_ent.mdb.
#'
#' @param haul Number of the haul you want to upload
#' @param dsnTablet String for the drive folder (in R notation) where tablet files exist.
#' @param dsnDataEnt String for the drive folder (in R notation) - include full file name and extension.
#'
#' @return changes to catch data in the access database (.mbd)
#' @export
#'
#' @examples
#' # Gulf of Alaska Example
#' # Point functions to correct tablet directory and "dataEnt" file
#' dsnTablet <- system.file("exdata/catch/GOA/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/GOA/data_ent.mdb", package = "GAPsurvey")
#' 
#' # Define the current haul number
#' haul <- 8
#' 
#' # Import length data
#' lengthData(haul,dsnTablet,dsnDataEnt)  
#' 
#' 
#' 
#' # Eastern Bering Sea Example
#' # Point functions to correct tablet directory and "dataEnt" file
#' dsnTablet <- system.file("exdata/catch/EBS/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/EBS/data_ent.mdb", package = "GAPsurvey")
#' 
#' # Define the current haul number
#' haul <- 59
#' 
#' # Import length data
#' lengthData(haul,dsnTablet,dsnDataEnt) 
lengthData <- function(haul, dsnTablet, dsnDataEnt) {
  
  data("PolySpecies", package = "GAPsurvey")
  
  options(stringsAsFactors = F)
  
  # Check to see if length files exist in data_ent.mdb for this haul
  entRawLength <- selectDataEnt(dsnDataEnt = dsnDataEnt, 
                                query = paste0("select * from RAW_LENGTH where HAUL = ", haul))
  if (nrow(entRawLength) != 0) {
    stop("L1: stop! Length data already exist in data_ent.mdb")
  }
  
  # Get starting index number from RAW_LENGTH
  i_rawLength <- as.numeric(selectDataEnt(dsnDataEnt, "select max(index) from RAW_LENGTH") + 1)
  i_rawLength <- ifelse(is.na(i_rawLength), 1, i_rawLength)
  
  # Get list of tablet length files in folder
  lengthFiles <- list.files(path=dsnTablet, pattern=paste0("_HAUL",sprintf("%04d", haul)))
  if (length(lengthFiles) == 0) {
    stop("L2: stop! No tablet length files were found for this haul in the given DSN")
  }
  
  tabRawLength <- data.frame(stringsAsFactors=F)
  
  # Loop through each length file
  for (i in 1:length(lengthFiles)) {
    tabLength <- data.frame(stringsAsFactors=F)
    tabLength <- utils::read.csv(file.path(dsnTablet,lengthFiles[i]))[,c("HAUL","POLY_SPECIES_CODE","SEX","LENGTH","LENGTH_TYPE","POLY_NUMBER","TABLET_ID")]
    tabRawLength <- rbind(tabRawLength,tabLength)
  }
  
  # Add index to raw length data
  entRawLength <- cbind(tabRawLength,
                        INDEX = seq(from=i_rawLength, 
                                    length.out=nrow(tabRawLength)))
  
  # Calculate length frequencies
  tabRawLength$FREQUENCY <- 1
  entLengthFreq <- stats::aggregate.data.frame(tabRawLength$FREQUENCY,
                                               by = tabRawLength[c("HAUL","POLY_SPECIES_CODE","SEX","LENGTH")], 
                                               FUN = sum)
  colnames(entLengthFreq)[5] <- "FREQUENCY"
  
  entLengthFreq <- base::merge(x = entLengthFreq, 
                               y = PolySpecies, 
                               by = "POLY_SPECIES_CODE")[,c("HAUL","SPECIES_CODE","SEX","LENGTH","FREQUENCY")]
  entLengthFreq$SUBSAMPLE_TYPE <- 1L
  entLengthFreq$LENGTH_TYPE <- NA
  
  # Upload length data
  writeDataEnt(dsnDataEnt,entRawLength,"RAW_LENGTH")
  writeDataEnt(dsnDataEnt,entLengthFreq,"LENGTH")
  
  # Verify lengths were uploaded
  checkRawLength <- selectDataEnt(dsnDataEnt, 
                                  paste0("select * from RAW_LENGTH where HAUL = ",haul))
  if (nrow(checkRawLength) > 0) {
    message(paste0("Success! ", nrow(checkRawLength), " rows inserted into data_ent.mdb"))
  } else {
    message("Something weird happened.")
  }
  
  return(entLengthFreq)
}




#' Import Specimen Tablet Files Into DataEnt.mdb
#'
#' Looks for tablet specimen files in given DSN and uploads them to SPECIMEN in the given data_ent.mdb.
#' 
#' #---> To use the program you must first:
#'  Bluetooth transfer the following files from tablets to catch computer (should automatically go into C:Users/NOAADATA/Documents/Bluetooth/inbox)
#'  Each of these file names will begin with the tablet name and end with the haul number. 
#'     DATAENT_CATCH_xxxx.csv
#'     DATAENT_BBAG_xxxx.csv     -- If there was a benthic bag
#'     RAW_BBAG_xxxx.csv         -- If there was a benthic bag
#'     RAW_CATCH_HAUL_xxxx.csv
#'     RAW_CATCH_HAUL_ATTR_xxxx.csv
#'     RAW_CATCH_SAMPLE_xxxx.csv
#'     RAW_CATCH_VALUE_xxxx.csv
#'     SPECIMEN_xxxx.csv         -- The specimen data
#'     HAUL_xxxx.csv             -- A length file for each tablet
#' 
#'  PLEASE NOTE: You cannot have any species in your catch data files that have a blank subsample_weight!  If you erroneously tap on a species during data collection, you must delete that species from the tablet before transfering the data to the catch computer!
#' 
#' @param haul Number of the haul you want to upload
#' @param dsnTablet String for the drive folder (in R notation) where tablet files exist.
#' @param dsnDataEnt String for the drive folder (in R notation) - include full file name and extension.
#' 
#' @return changes to catch data in the access database (.mbd)
#' @export
#'
#' @examples
#' # Gulf of Alaska Example
#' # Point functions to correct tablet directory and "dataEnt" file
#' dsnTablet <- system.file("exdata/catch/GOA/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/GOA/data_ent.mdb", package = "GAPsurvey")
#' importLength <- TRUE ## Change this "T" to "F" if not importing length data ##
#' 
#' # Define the current haul number
#' haul <- 8
#' 
#' # Import specimen data
#' specimenData(haul,dsnTablet,dsnDataEnt)
#' 
#' 
#' # Eastern Bering Sea Example
#' # Point functions to correct tablet directory and "dataEnt" file
#' dsnTablet <- system.file("exdata/catch/EBS/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/EBS/data_ent.mdb", package = "GAPsurvey")
#' 
#' # Define the current haul number
#' haul <- 59
#' 
#' # Import specimen data
#' specimenData(haul,dsnTablet,dsnDataEnt)
#' 
specimenData <- function(haul, dsnTablet, dsnDataEnt) {
  options(stringsAsFactors = F)
  
  # Check to see if specimen files exist in data_ent.mdb for this haul
  entSpecimen <- selectDataEnt(dsnDataEnt, paste0("select * from SPECIMEN where HAUL = ",haul))
  if (nrow(entSpecimen) != 0) {
    stop("S1: stop! Specimen data for this haul already exist in data_ent.mdb")
  }
  
  # Get starting index number from RAW_LENGTH
  i_spec <- as.numeric(selectDataEnt(dsnDataEnt, "select max(ENTRY_ORDER) from SPECIMEN") + 1)
  i_spec <- ifelse(is.na(i_spec), 1, i_spec)
  
  # Get list of tablet specimen files in folder
  specimenFiles <- list.files(path=dsnTablet, pattern=paste0("SPECIMEN_",sprintf("%04d", haul)))
  specimenFiles <- specimenFiles[!grepl("RAW",specimenFiles)]
  
  # Import specimen files as a dataframe
  if (length(specimenFiles) == 0) {
    stop("S2: stop! No tablet specimen files were found for this haul in the given DSN")
  } else {
    tabSpec <- utils::read.csv(file.path(dsnTablet,specimenFiles))
  }
  
  # Add index to specimen data from data_ent
  tabSpec$ENTRY_ORDER <- seq(from=i_spec, length.out=nrow(tabSpec))
  
  # Upload specimen data
  writeDataEnt(dsnDataEnt,tabSpec,"SPECIMEN")
  
}




#' Import Benthic Bag Tablet Files Into DataEnt.mdb
#'
#' NOTE: Must be run on 32-bit R!
#' NOTE: Benthic Bag data is not collected by the eastern Bering Sea team, so this function is not required for those trawls. 
#' 
#' @param haul haul number
#' @param dsnTablet full file path of the location of raw tablet data
#' @param dsnDataEnt full file path of the location of the dataent.mdb - include full file name and extension.
#'
#' @return changes to catch data in the access database (.mbd)
#' @export
#'
#' @examples
#' # Point functions to correct tablet directory and "dataEnt" file
#' dsnTablet <- system.file("exdata/catch/GOA/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/GOA/data_ent.mdb", package = "GAPsurvey")
#' importLength <- TRUE ## Change this "T" to "F" if not importing length data ##
#' 
#' # Define the current haul number
#' haul <- 8
#' 
#' # Import catch data
#' # benthicData(haul,dsnTablet,dsnDataEnt)
benthicData <- function(haul, dsnTablet, dsnDataEnt) {
  options(stringsAsFactors = F)
  
  # Grab data_ent formatted tablet data
  benthicFile <- list.files(path=dsnTablet, pattern=paste0("DATAENT_BBAG_",sprintf("%04d", haul)))
  
  if (length(benthicFile) != 1) {
    stop("BB1: Benthic Bag file missing or duplicate")
  } else {
    tabBenthic <- utils::read.csv(file.path(dsnTablet,benthicFile))
  }
  
  # Get benthic bag index
  i_benthic <- as.numeric(selectDataEnt(dsnDataEnt, "select max(ENTRY_ORDER) from BENTHIC_BAG") + 1)
  i_benthic <- ifelse(is.na(i_benthic), 1, i_benthic)
  tabBenthic$ENTRY_ORDER <- seq(from=i_benthic, length.out=nrow(tabBenthic))
  
  # Upload benthic bag data
  tabBenthic[tabBenthic == 'null'] <- NA
  writeDataEnt(dsnDataEnt,tabBenthic,"BENTHIC_BAG")
}


#' Import Catch Tablet Files Into DataEnt.mdb
#'
#' Processes catch data exported from catch tablets into summarized values into the designated dataent.mdb. You must have exported from the tablet:
#' DATAENT_CATCH... and RAW_CATCH_HAUL... for the designated haul.
#'
#' NOTE: Must be run on 32-bit R!
#' 
#' To use the program you must first:
#' Bluetooth transfer the following files from tablets to catch computer (should automatically go into C:Users/NOAADATA/Documents/Bluetooth/inbox)
#'  Each of these file names will begin with the tablet name and end with the haul number. 
#'     DATAENT_CATCH_xxxx.csv
#'     DATAENT_BBAG_xxxx.csv     -- If there was a benthic bag
#'     RAW_BBAG_xxxx.csv         -- If there was a benthic bag
#'     RAW_CATCH_HAUL_xxxx.csv
#'     RAW_CATCH_HAUL_ATTR_xxxx.csv
#'     RAW_CATCH_SAMPLE_xxxx.csv
#'     RAW_CATCH_VALUE_xxxx.csv
#'     SPECIMEN_xxxx.csv         -- The specimen data
#'     HAUL_xxxx.csv             -- A length file for each tablet
#'  
#'  PLEASE NOTE: You cannot have any species in your catch data files that have a blank subsample_weight!  If you erroneously tap on a species during data collection, you must delete that species from the tablet before transfering the data to the catch computer!
#'
#'  IF the catch was subsampled, the user will be given an option to define the subsample type (1 = 100% Processed (No subsample), 2 = Subsampled, Load cell estimate of total weight, 4 = Subsampled, Visual estimate of total weight, 6 = Subsampled, basket weights of both fractions (table split), 9 = Non-quantitative catch sampling, 12 = Volumetric method. Density from Density Lookup Table, 13 = Volumetric method. Density calculated on deck from haul sample).
#' 
#' @param haul haul number
#' @param dsnTablet full file path of the location of raw tablet data
#' @param dsnDataEnt full file path and filename of the location of the dataent.mdb - include full file name and extension.
#' @param importLength logical - import length data (GAPsurvey::lengthData()) prior to importing catch data
#'
#' @return changes to catch data in the access database (.mbd)
#' @export
#'
#' @examples
#' # Gulf of Alaska Example (without error)
#' # Point functions to correct tablet directory and "dataEnt" file
#' dsnTablet <- system.file("exdata/catch/GOA/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/GOA/data_ent.mdb", package = "GAPsurvey")
#' importLength <- TRUE 
#' 
#' # Define the current haul number
#' haul <- 8
#' 
#' # Import catch data 
#' catchData(haul,dsnTablet,dsnDataEnt,importLength)
#' 
#' # Delete data for next example run
#' dMix <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                        tablename = 'MIXTURE')
#' dMixHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                          tablename = 'MIXTURE_HEADER')
#' dCatch <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                          tablename = 'CATCH')
#' dCatchHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                            tablename = 'CATCH_HEADER')
#' dLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                         tablename = 'LENGTH')
#' dRawLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                             tablename = 'RAW_LENGTH')
#'
#' # Eastern Bering Sea Example (Will cause error)
#' # Point functions to correct tablet directory and "dataEnt" file
#' dsnTablet <- system.file("exdata/catch/EBS/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/EBS/data_ent.mdb", package = "GAPsurvey")
#' importLength <- TRUE 
#' 
#' # Define the current haul number
#' haul <- 59
#' 
#' # Import catch data (This will error)
#' # catchData(haul,dsnTablet,dsnDataEnt,importLength)
#' # Row 33 for "empty bivalve shells" requires a value for SUBSAMPLE_WEIGHT. 
#' # Return to the tablet and edit the data there before running it through 
#' # this function again. 
catchData <- function(haul, 
                      dsnTablet, 
                      dsnDataEnt, 
                      importLength = TRUE) {
  
  options(stringsAsFactors = F)
  
  # Grab data_ent formatted tablet data
  catchFile <- list.files(path=dsnTablet, 
                          pattern=paste0("DATAENT_CATCH_",
                                         sprintf("%04d", haul)))
  
  if (length(catchFile) != 1) {
    stop("C1: Data Ent Catch file missing or duplicate.")
  } else {
    tabCatch <- utils::read.csv(file.path(dsnTablet,catchFile))
  }
  tabCatch$seq <- seq(1:nrow(tabCatch))
  
  
  if (!is.character(tabCatch$SAMPLED_ALL)) {
    stop("C3: SAMPLED_ALL flag has not been set for any species in this haul. Correct data in tablet and try again.")
  }
  
  if (anyNA(tabCatch$SAMPLED_ALL)) {
    stop("C3b: SAMPLED_ALL flag has not been set for one or more species in this haul. Correct data in tablet and try again.")
  }
  
  
  if (sum((is.na(tabCatch$SUBSAMPLE_WEIGHT) & is.na(tabCatch$NONSUB_WEIGHT)))>0) {
    stop(paste0("C3c: Both SUBSAMPLE_WEIGHT and NONSUB_WEIGHT have no values for ", 
                sum(is.na(tabCatch$SUBSAMPLE_WEIGHT) & is.na(tabCatch$NONSUB_WEIGHT)),
                " species. Delete species entry if added in error. A value is required for one or both types of weights (subsample or non-subsample). Correct data in tablet and try again."))
  }
  
  
  # If importLength = TRUE, upload length files first
  if (importLength) {
    
    lengthFreq <- lengthData(haul,dsnTablet, dsnDataEnt)
    
    lengthSum <- stats::aggregate.data.frame(lengthFreq$FREQUENCY,by=lengthFreq[c("SPECIES_CODE")], FUN = sum)
    
    halibut <- lengthFreq[lengthFreq$SPECIES_CODE==10120,]
    if (nrow(halibut) > 0) {
      halibutWeight <- sapply(halibut$LENGTH,FUN= function(x) 0.00237*x^3.24) * halibut$FREQUENCY/1000000
      halibutWeight <- data.frame(SPECIES_CODE = 10120,
                                  SPECIES_NAME = "Hippoglossus stenolepis",
                                  NONSUB_WEIGHT = NA,
                                  SUBSAMPLE_WEIGHT = sum(halibutWeight),
                                  SUBSAMPLE_NUMBERS = NA,
                                  SAMPLED_ALL = 'Y',
                                  VOUCHER_NUMBER = NA,
                                  seq = 0
      )
      
      tabCatch <- rbind(halibutWeight, tabCatch)
    }
    
    # Check for lengths not incatch
    noCatch <- setdiff(lengthFreq$SPECIES_CODE,tabCatch$SPECIES_CODE)
    if (!sum(noCatch)==0) {
      stop(paste0("C5: there is no catch data for these species in Length: ", paste(noCatch, collapse=", "),". Re-run with importLength=FALSE and fix in data_ent.mdb"))
    }
    
    
    allCatch <- merge(tabCatch, lengthSum, by = "SPECIES_CODE", all=T)
    
    # if (nrow(allCatch) != nrow(lengthSum)) {
    #   stop("C4: There are species in the length file not found in the catch, correct and try again.")
    # } else {
    #   allCatch$SUBSAMPLE_NUMBERS <- ifelse(is.na(allCatch$x),allCatch$SUBSAMPLE_NUMBERS,allCatch$x)
    #   tabCatch <- subset(allCatch, select = -x)
    # }
    
    # Add subsample numbers from length to catch, then re-sort from original seq
    allCatch$SUBSAMPLE_NUMBERS <- ifelse(is.na(allCatch$x),allCatch$SUBSAMPLE_NUMBERS,allCatch$x)
    allCatch <- allCatch[order(allCatch$seq),]
    
    tabCatch <- subset(allCatch, select = c(SPECIES_CODE,SPECIES_NAME,NONSUB_WEIGHT,SUBSAMPLE_WEIGHT,SUBSAMPLE_NUMBERS,SAMPLED_ALL,VOUCHER_NUMBER))
  } else {
    message("Length data not processed, handle subsample numbers manually in data_ent.mdb")
    
    # Remove Seq column
    tabCatch <- subset(tabCatch, select = c(SPECIES_CODE,SPECIES_NAME,NONSUB_WEIGHT,SUBSAMPLE_WEIGHT,SUBSAMPLE_NUMBERS,SAMPLED_ALL,VOUCHER_NUMBER))
    
  }
  
  # Grab catch header files
  catchHeader <- list.files(path=dsnTablet, 
                            pattern=paste0("RAW_CATCH_HAUL_",sprintf("%04d", haul)))
  if (length(catchHeader) != 1) {
    stop("C4: Raw Catch Haul file missing or duplicated.")
  } else {
    tabCatchHeader <- utils::read.csv(file.path(dsnTablet,catchHeader))[,c("HAUL","WEIGHT","HUNDRED_PERCENT_SAMPLED")]
  }
  names(tabCatchHeader) <- c("HAUL","TOTAL_CATCH_WEIGHT","HUNDRED_PERCENT_SAMPLED")
  
  # Test data_ent for catch header data
  CATCH_HEADER <- selectDataEnt(dsnDataEnt, "select * from CATCH_HEADER")
  if (nrow(CATCH_HEADER[CATCH_HEADER$HAUL==haul,]) > 0) {
    stop("C2: CATCH_HEADER data already exists for this haul, abort!")
  }
  
  # If not 100% sampled, ask for catch subsample type
  if (tabCatchHeader$HUNDRED_PERCENT_SAMPLED == 0) {
    ADPsubsample <- selectDataEnt(dsnDataEnt, "select * from ADP_CATCH_SUBSAMPLE")
    print(ADPsubsample)
    subsample <- readline(prompt="Enter subsample code: ")
    tabCatchHeader$SUBSAMPLE_CODE <- subsample
    
    # Handle SAME_PROPORTION flags based on SAMPLED_ALL flags
    if (sum(tabCatch$SAMPLED_ALL=='N')==0) {
      tabCatchHeader$SAME_PROPORTION <- 1
    } else {
      tabCatchHeader$SAME_PROPORTION <- 0
    }
  } else {
    tabCatchHeader$SUBSAMPLE_CODE <- 1
    tabCatchHeader$SAME_PROPORTION <- -1
    tabCatchHeader$TOTAL_CATCH_WEIGHT <- 0
  }
  
  # Format header for data_ent
  entCatchHeader <- subset(tabCatchHeader, select = c(HAUL,SUBSAMPLE_CODE,SAME_PROPORTION,TOTAL_CATCH_WEIGHT))
  
  # Get starting index number from CATCH
  i_catch <- as.numeric(selectDataEnt(dsnDataEnt, "select max(ENTRY_ORDER) from CATCH") + 1)
  i_catch <- ifelse(is.na(i_catch), 1, i_catch)
  
  # Do math for entCatch
  calcCatch <- tabCatch
  calcCatch$NONSUB_WEIGHT[is.na(calcCatch$NONSUB_WEIGHT)] <- 0
  calcCatch$SUBSAMPLE_NUMBERS[is.na(calcCatch$SUBSAMPLE_NUMBERS)] <- 0
  calcCatch$CALCULATED_WEIGHT <- calcCatch$NONSUB_WEIGHT + calcCatch$SUBSAMPLE_WEIGHT
  calcCatch$AVERAGE_WEIGHT <- calcCatch$SUBSAMPLE_WEIGHT/calcCatch$SUBSAMPLE_NUMBERS
  calcCatch$CALCULATED_NUMBERS <- round(calcCatch$SUBSAMPLE_NUMBERS + (calcCatch$NONSUB_WEIGHT/calcCatch$AVERAGE_WEIGHT), digits=0)
  calcCatch$ENTRY_ORDER <- seq(from=i_catch, length.out=nrow(calcCatch))
  calcCatch$HAUL <- haul
  
  # Look for mix data
  mixes <- grep("Mix", tabCatch$SPECIES_NAME)
  if (length(mixes) == 0) {
    entCatch <- calcCatch
  } else if (length(mixes) == 1) {
    entCatch <- calcCatch[1:(mixes-1),]
    entMixHeader <- calcCatch[mixes,]
    entMix <- calcCatch[(mixes+1):nrow(calcCatch),]
    
    # Set up Mix calculations
    entMixHeader$SPECIES_CODE <- 99000
    entMixHeader$SUBSAMPLE_WEIGHT <- sum(entMix$SUBSAMPLE_WEIGHT)
    entMixHeader$CALCULATED_WEIGHT <- entMixHeader$SUBSAMPLE_WEIGHT + entMixHeader$NONSUB_WEIGHT
    entCatch <- rbind(entCatch,entMixHeader)
    
  } else {
    for (i in 1:length(mixes)) {
      stop("Too many mixes!")
    }
    
  }
  
  # Total weight for extrapolation (minus sampled all)
  if (entCatchHeader$SUBSAMPLE_CODE==1) {
    entCatch$TOTAL_WEIGHT <- entCatch$CALCULATED_WEIGHT
    entCatch$TOTAL_NUMBERS <- entCatch$CALCULATED_NUMBERS
  } else {
    extrapWeight <- sum(entCatch[entCatch$SAMPLED_ALL == 'N', "CALCULATED_WEIGHT"])
    extrapTotal <- entCatchHeader$TOTAL_CATCH_WEIGHT - sum(entCatch$CALCULATED_WEIGHT)
    entCatch$TOTAL_WEIGHT <- ifelse(entCatch$SAMPLED_ALL == 'Y', entCatch$CALCULATED_WEIGHT, (entCatch$CALCULATED_WEIGHT/extrapWeight * entCatchHeader$TOTAL_CATCH_WEIGHT))
    entCatch$TOTAL_NUMBERS <- round(entCatch$TOTAL_WEIGHT/entCatch$AVERAGE_WEIGHT, digits=0)
  }
  
  entCatch[entCatch == Inf] <- NA
  
  # Format mixture for data ent
  if (exists("entMixHeader")) {
    i_mix <- as.numeric(selectDataEnt(dsnDataEnt, "select max(ENTRY_ORDER) from MIXTURE") + 1)
    i_mix <- ifelse(is.na(i_mix), 1, i_mix)
    
    entMixHeader$TOTAL_WEIGHT <- entCatch[grep("Mix", entCatch$SPECIES_NAME),"TOTAL_WEIGHT"]
    
    entMix$ENTRY_ORDER <- seq(from=i_mix, length.out=nrow(entMix))
    entMix$TOTAL_WEIGHT <- if (entMixHeader$SAMPLED_ALL == 'Y') entMix$CALCULATED_WEIGHT else (entMix$CALCULATED_WEIGHT/sum(entMix$CALCULATED_WEIGHT)*entMixHeader$TOTAL_WEIGHT)
    entMix$TOTAL_NUMBERS <- round(entMix$TOTAL_WEIGHT/entMix$AVERAGE_WEIGHT, digits=0)
    entMix <- subset(entMix, select = c(HAUL,SPECIES_CODE,SPECIES_NAME,NONSUB_WEIGHT,SUBSAMPLE_WEIGHT,SUBSAMPLE_NUMBERS,CALCULATED_WEIGHT,
                                        CALCULATED_NUMBERS,AVERAGE_WEIGHT,ENTRY_ORDER,TOTAL_WEIGHT,TOTAL_NUMBERS))
    entMix[entMix == Inf] <- NA
    
    entMixHeader$MIXTURE_TYPE_CODE <- 2
    entMixHeader$MIXTURE_SAMPLED_ALL <- ifelse(entMixHeader$SAMPLED_ALL=='Y',-1,0)
    entMixHeader <- subset(entMixHeader, select = c(HAUL,MIXTURE_TYPE_CODE,NONSUB_WEIGHT,MIXTURE_SAMPLED_ALL))
    names(entMixHeader) <- c("HAUL","MIXTURE_TYPE_CODE","MIXTURE_SAMPLE_WEIGHT","MIXTURE_SAMPLED_ALL")
    
    # Convert zeros to NULLs for DB
    entMix[entMix == 0] <- NA
    # entMixHeader[entMixHeader == 0] <- NA
    
    writeDataEnt(dsnDataEnt,entMixHeader,"MIXTURE_HEADER")
    writeDataEnt(dsnDataEnt,entMix,"MIXTURE")
  }
  
  # Upload catch data
  # Convert zeros to NULLs for DB
  # entCatchHeader[entCatchHeader == 0] <- NA
  entCatch[entCatch == 0] <- NA
  writeDataEnt(dsnDataEnt,entCatchHeader,"CATCH_HEADER")
  writeDataEnt(dsnDataEnt,entCatch,"CATCH")
}






# Manage DataEnt -------------------------------------------------------------

#' Write records to DataEnt.mdb
#'
#' NOTE: Must be run on 32-bit R!
#'
#' Creates connection channel to data_ent.mdb, then appends data to given table.
#' @param dsnDataEnt String for the drive folder (in R notation) - include full file name and extension.
#' @param data matrix or data.frame containg data from catch, specimen and lengths tablets
#' @param tablename String of the data_ent table you are deleting data from
#'
#' @return Changes to the data_ent.mbd file. 
#' @export
#'
#' @examples
#' # writeDataEnt()
writeDataEnt <- function(dsnDataEnt, data, tablename) {
  odbcStr <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",dsnDataEnt)
  dataEnt <- RODBC::odbcDriverConnect(odbcStr)
  
  sqlReturn <- RODBC::sqlSave(dataEnt, data, tablename, append=T, rownames=F)
  
  # FUTURE: SQLupdate version
  # sqlReturn <- RODBC::sqlUpdate(dataEnt, data, tablename, index=index)
  
  RODBC::odbcClose(dataEnt)
  
  return(sqlReturn)
}



#' Delete data from DataEnt.mdb
#'
#' NOTE: Must be run on 32-bit R!
#'
#' Deletes data from data_ent.mdb for a haul.
#' @param dsnDataEnt String for the drive folder (in R notation) - include full file name and extension.
#' @param haul Number of the haul you want to delete
#' @param tablename String of the data_ent table you are deleting data from
#'
#' @return Changes to the data_ent.mbd file. 
#' @export
#'
#' @examples
#' # There will need to be observations in the data_ent.mbd for this example to work. 
#' dsnTablet <- system.file("exdata/catch/GOA/", package = "GAPsurvey")
#' dsnDataEnt <- system.file("exdata/catch/GOA/data_ent.mdb", package = "GAPsurvey")
#' importLength <- TRUE 
#' haul <- 8 # Define the current haul number
#' catchData(haul,dsnTablet,dsnDataEnt,importLength) # Import catch data 
#' 
#' dMix <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                         tablename = 'MIXTURE')
#' dMixHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                           tablename = 'MIXTURE_HEADER')
#' dCatch <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                          tablename = 'CATCH')
#' dCatchHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                              tablename = 'CATCH_HEADER')
#' dLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                           tablename = 'LENGTH')
#' dRawLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = 
#'                                haul, tablename = 'RAW_LENGTH')
deleteDataEnt <- function(dsnDataEnt, haul, tablename) {
  odbcStr <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",dsnDataEnt)
  dataEnt <- RODBC::odbcDriverConnect(odbcStr)
  
  deleteQuery <- paste0("delete from ",tablename," where haul = ", haul)
  
  sqlReturn <- RODBC::sqlQuery(dataEnt, deleteQuery)
  
  RODBC::odbcClose(dataEnt)
  
  return(sqlReturn)
}


#' Query DataEnt.mdb
#'
#' NOTE: Must be run on 32-bit R!
#'
#' Executes SQL queries of data_ent.mdb
#' @param dsnDataEnt String for the drive folder (in R notation) - include full file name and extension.
#' @param query A SQL query string
#'
#' @return Changes to the data_ent.mbd file. 
#' @export
#'
#' @examples
#' # selectDataEnt()
selectDataEnt <- function(dsnDataEnt, query) {
  odbcStr <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",dsnDataEnt)
  dataEnt <- RODBC::odbcDriverConnect(odbcStr)
  
  sqlReturn <- RODBC::sqlQuery(dataEnt, query)
  RODBC::odbcClose(dataEnt)
  
  return(sqlReturn)
}
