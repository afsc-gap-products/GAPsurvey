# #' ---
# #' title: GAPsurvey
# #' purpose: assist scientists on the survey collect data
# #' author: Jason Conner (jason.conner AT noaa.gov), Emily Markowitz (emily.markowitz AT noaa.gov), and Liz Dawson (Liz.Dawson AT noaa.gov)
# #' modified by: Emily Markowitz (emily.markowitz AT noaa.gov) and Liz Dawson (Liz.Dawson AT noaa.gov)
# #' start date: 2018?
# #' modified date: June 2021
# #' ---


# Converts ----------------------------------------------


#' BVDR Conversion to Create BTD data
#'
#' Converts Marport BVDR data (.ted and .tet files from Marport headrope sensor) to .BTD format.  You must first run the BVDR converter program (convert_bvdr.exe) to convert the Marport .bvdr files into .ted and .tet files that can be pulled into R. The BVDR program and instructions can be found in the RACE Survey App.  You will have to create your own .SGT file using the example in the BVDR instruction file with start and end time (be sure to include a carriage return after your (second and) final row of data!), because this is not a file that our current systems creates.  Once you have used the BVDR converter to output the .ted and .tet files you are ready to use the convert_ted_btd() function here!
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 162 for AK Knight, 94 for Vesteraalen). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year + sequential two digit cruise (e.g., 202101). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number that you are trying to convert data for (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = NA. The model number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual version number without any negative repercussions).
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual serial number without any negative repercussions).
#' @param path_in Optional. The default is the location on the catch computer ("C:/Program Files/Marport Server/Logs/") but any path can be entered.
#' @param path_out Optional. The default is the local working directory but can be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#'
#' @return .BTH and .BTD files to the path_out directory.
#' @export
#'
#' @examples
#' convert_ted_btd(
#'    VESSEL = 94,
#'    CRUISE = 201901,
#'    HAUL = 3,
#'    MODEL_NUMBER = 123,
#'    VERSION_NUMBER = 456,
#'    SERIAL_NUMBER = 789,
#'    path_in = system.file("exdata/convert_bvdr_btd/", package = "GAPsurvey"),
#'    path_out = getwd(),
#'    filename_add = "newted")
convert_ted_btd <- function(
    VESSEL = NA,
    CRUISE = NA,
    HAUL = NA,
    MODEL_NUMBER = NA,
    VERSION_NUMBER = NA,
    SERIAL_NUMBER = NA,
    path_in = "C:/Program Files/Marport Server/Logs/",
    path_out = "./",
    filename_add = "new"){

  format_date <- function(x, ...) {
    tmp <- format(x, ...)
    tmp <- sub("^[0]+", "", tmp)
    tmp <- sub('/0', "/", tmp)
    return(tmp)
  }

  if (is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if (is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if (is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if (is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number:  ") }
  if (is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number:  ") }
  if (is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of Marport height sensor:  ") }

  # make sure path_in comes in with correct format
  path_in <- fix_path(path_in)
  path_out <- fix_path(path_out)

  HAUL <- as.numeric(HAUL)
  shaul <- numbers0(x = HAUL, number_places = 4)

  file.name.ted <- paste(path_in,
                         CRUISE,"_",VESSEL,"_",shaul,".ted",sep="")
  file.name.tet <- paste(path_in,
                         CRUISE,"_",VESSEL,"_",shaul,".tet",sep="")

  ted.file=utils::read.csv(file.name.ted,header=F)
  tet.file=utils::read.csv(file.name.tet,header=F)
  #ted.file$V4=as.numeric(strptime(ted.file[,4], format = "%m/%d/%Y %H:%M:%S"))
  #tet.file$V4=as.numeric(strptime(tet.file[,4], format = "%m/%d/%Y %H:%M:%S"))
  ted.file$V4=strptime(ted.file[,4], format = "%m/%d/%Y %H:%M:%S")
  tet.file$V4=strptime(tet.file[,4], format = "%m/%d/%Y %H:%M:%S")
  ted.file=ted.file[,c(4,6)]
  tet.file=tet.file[,c(4,6)]
  colnames(ted.file)=c("date","depth")
  colnames(tet.file)=c("date","temp")
  # str(ted.file)
  # str(tet.file)
  merged<-base::merge(ted.file,tet.file,all=T)
  # str(merged)
  # head(merged)

  #which(ted.file[,4] %in% tet.file[,4])

  xx=merged$date
  DATE_TIME <- format(xx, format = "%m/%d/%Y %H:%M:%S")

  DATE_TIME_btd <- format(as.POSIXct(DATE_TIME, format = "%m/%d/%Y %H:%M:%S"),
                          format = "%m/%d/%Y %H:%M:%S")

  DATE_TIME_btd <- format_date(DATE_TIME_btd)
  DATE_TIME <- format(xx, format = "%m/%d/%y %H:%M:%S")

  HOST_TIME=max(DATE_TIME)
  LOGGER_TIME=max(DATE_TIME)
  LOGGING_START=min(DATE_TIME)
  LOGGING_END=max(DATE_TIME)
  TEMPERATURE=merged$temp
  DEPTH=merged$depth
  SAMPLE_PERIOD=3
  NUMBER_CHANNELS=2
  NUMBER_SAMPLES=0
  MODE=2

  # Write BTD file
  DATE_TIME <- DATE_TIME_btd
  new.BTD=cbind(VESSEL,CRUISE,HAUL,SERIAL_NUMBER,DATE_TIME,TEMPERATURE,DEPTH)
  new.BTD[which(is.na(new.BTD))]=""
  new.BTD <- data.frame(new.BTD)

  # Write BTH file
  new.BTH=cbind(VESSEL,CRUISE,HAUL,MODEL_NUMBER,VERSION_NUMBER,SERIAL_NUMBER,
                HOST_TIME,LOGGER_TIME,LOGGING_START,LOGGING_END,
                SAMPLE_PERIOD,NUMBER_CHANNELS,
                NUMBER_SAMPLES,MODE)
  new.BTH <- data.frame(new.BTH)

  new.BTD=new.BTD[new.BTD$DEPTH!="2000",]

  #head(new.BTD)
  #return(head(new.BTD))
  filename <- paste0(path_out, "HAUL",shaul,
                     ifelse(is.na(filename_add) | filename_add == "",
                            "", paste0("_", filename_add)))
  utils::write.csv(x = new.BTD,
                   file = paste0(filename, ".BTD"),
                   quote=F,
                   row.names=F,
                   eol=",\n"
  )

  utils::write.csv(x = new.BTH,
                   file = paste0(filename, ".BTH"),
                   quote=F,
                   row.names=F)

  message(paste0("Your new ", filename, " .BTD and .BTH files are saved."))

}



#' Convert CTD data in .cnv form to BTD and BTH
#'
#' This function uses SBE Data Processing software to convert a CTD binary-formatted .hex (hexadecimal) file to bathythermic data (.btd) and bathythermic header files (.bth). If you are unable to convert your file, please contact sean.rohan@@noaa.gov.
#'
#' If SBE Data Processing software is not installed on the same computer as the GAPsurvey package or if you encounter errors, you may need to convert the .hex file manual using SBE Data Processing software:
#' 1. use the SBE Data Processing Program, and in the "Run" menu select "1. Data conversion".
#' 2. Under "Program setup file" select the .psa file (should be DataCnv.psa located in CTD folder), under "Instrument configuration file" select the .xmlcon file for the CTD located in the
#' deployment_xmlcon folder- this is CTD specific, know your CTD's serial number, and under "Input directory" select the .hex file (from the CTD computer) that is specific to the haul that you are missing data.  Click "Start Process".
#' The conversion program will ask you for outputs. At the prompt, Select and Add "Time, Elapsed, seconds", "Depth, salt water, m", "Temperature, ITS-90, deg C", "Pressure, Strain Gauge, db", and "Conductivity, S/m".
#' 3. This .cnv file must include columns for "Time, Elapsed, seconds", "Depth , salt water, m", "Temperature, ITS-90, deg C", "Pressure, Strain Gauge, db", and "Conductivity, S/m".
#' 4. Press "Start Process" button again and a .cnv file should appear in your selected output directory.  The .cnv file can then be used for this convert_ctd_btd() function!
#' 5. Look in your output directory (usually the "Documents" folder, but you can find it using getwd()) for your new .BTD and .BTH files.
#'
#'
#' @param path_in Required. Filepath to the SBE19plus hexadecimal (.hex) file from a single cast as a character vector (e.g. "C:/CTD/202301_162_L1/sbe19plus01908091_05_04_0001.hex")/.
#' @param path_xmlcon Required. Filepath to the CTD configuration file (.xmlcon) for the CTD used for the cast (e.g. serial number 8091 would use the file with 8091 in the name ("C:/CTD/xmlcon configuration files/SBE19plusV2_8091.xmlcon").
#' @param path_out Optional. The default is the local working directory but may be specified with a string.
#' @param VESSEL Required. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Required. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Required. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = NA. The model number of the CTD (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the CTD (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the CTD (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#'
#' @return .BTH and .BTD files to the path_out directory.
#' @export
#'
#' @examples
#'
#' # Convert directly .hex to .btd and .bth using convert_ctd_btd to run SBE Data Processing.
#' GAPsurvey::convert_ctd_btd(path_in = system.file("exdata/convert_ctd_btd/2021_06_13_0003.hex",
#'                       package = "GAPsurvey"),
#'                    path_xmlcon = system.file("exdata/convert_ctd_btd/19-8102_Deploy2021.xmlcon",
#'                                     package = "GAPsurvey"))
#'
#' convert_ctd_btd(
#'    VESSEL = 94,
#'    CRUISE = 202101,
#'    HAUL = 107,
#'    MODEL_NUMBER = "", # for example, also can make up
#'    VERSION_NUMBER = "", # for example, also can make up
#'    SERIAL_NUMBER = 8105, # for example, also can make up
#'    path_in = system.file(paste0("exdata/convert_ctd_btd/",
#'      "2021_06_13_0003.hex"),
#'       package = "GAPsurvey"),
#'    path_xmlcon = system.file(paste0("exdata/convert_ctd_btd/",
#'      "19-8102_Deploy2021.xmlcon"),
#'       package = "GAPsurvey"),
#'    path_out = getwd(),
#'    filename_add = "newctd")
#'
#'
#' # Convert a .cnv file (after .hex files has already converted using SBE Data Processing)
#' convert_ctd_btd(
#'    VESSEL = 94,
#'    CRUISE = 2001,
#'    HAUL = 4,
#'    MODEL_NUMBER = 123, # for example, also can make up
#'    VERSION_NUMBER = 456, # for example, also can make up
#'    SERIAL_NUMBER = 789, # for example, also can make up
#'    path_in = system.file(paste0("exdata/convert_ctd_btd/",
#'       "SBE19plus_01908103_2021_06_01_94_0004_raw.cnv"),
#'        package = "GAPsurvey"),
#'    path_xmlcon = system.file(paste0("exdata/convert_ctd_btd/",
#'      "19-8102_Deploy2021.xmlcon"),
#'       package = "GAPsurvey"),
#'    path_out = getwd(),
#'    filename_add = "newctd")
#'
#'

convert_ctd_btd <- function(
    VESSEL = NA,
    CRUISE = NA,
    HAUL = NA,
    MODEL_NUMBER = NA,
    VERSION_NUMBER = NA,
    SERIAL_NUMBER = NA,
    path_in,
    path_out = "./",
    path_xmlcon = NULL,
    filename_add = ""){

  format_date <- function(x, ...) {
    tmp <- format(x, ...)
    tmp <- sub("^[0]+", "", tmp)
    tmp <- sub('/0', "/", tmp)
    return(tmp)
  }

  if (is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if (is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if (is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if (is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number:  ") }
  if (is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number:  ") }
  if (is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of CTD:  ") }

  path_in <- fix_path(path_in)
  file.name <- path_in

  if (grepl(pattern = ".hex", x = path_in)) {

    stopifnot("convert_ctd_btd: Must provide path_xmlcon if path_in is a .hex file." = !is.null(path_xmlcon))
    stopifnot("convert_ctd_btd: Must provide path_xmlcon if path_in is a .hex file." = file.exists(path_xmlcon))

    file.name <- GAPsurvey::convert_ctd_hex(hex_file_path = path_in,
                                            xmlcon_path = path_xmlcon,
                                            bat_file = NULL,
                                            datcnv_file = NULL)
  }

  # make sure path_in comes in with correct
  # options(stringsAsFactors = FALSE) # die, factors, die!
  path_out <- fix_path(path_out)

  HAUL <- as.numeric(HAUL)
  shaul <- numbers0(x = HAUL, number_places = 4)

  data0 <- (readLines(file.name))
  header0<-data0[(which(data0 == "# units = specified")+1):
                   (grep(pattern = "# span 0 =", x = data0)-1)]
  header0 <- lapply(strsplit(x = header0, split = " = "), `[[`, 2)
  header0 <- unlist(lapply(strsplit(x = unlist(header0), split = " "), `[[`, 2))
  header0 <- gsub(pattern = ",", replacement = "", x = header0)
  # header0 <- header0[header0 != ""] # flag column
  headers<-data.frame(bth = c("", "timeS", "depth", "temperature", "pressure",
                              "conductivity", "flag"),
                      ctd = c("", "Time", "Depth", "Temperature", "Pressure",
                              "Conductivity", ""))
  # headers<-headers[headers$ctd %in% header0,]
  headers<-headers[match(x = header0, table = headers$ctd), ]

  data1 <- data0[(which(data0 == "*END*")+1):length(data0)]
  # data1 <- unique(data1)

  # datapasta::df_paste(input_table = data1)

  data1 <- data.frame(matrix(data = unlist(strsplit(data1, "\\s+")),
                             ncol = nrow(headers)+1, byrow = TRUE))
  data1$X1 <- NULL
  names(data1)<-headers$bth
  # names(data1) <- header0


  # names(data1) <- c("timeS", "depth", "temperature", "pressure",
  #                  "conductivity", "flag") # , "salinity"

  data1$second <- floor(x = as.numeric(as.character(data1$timeS)))


  # data1 <- data1 %>%
  #   group_by(data1, second) %>%
  #   summarize(m_depth = mean(depth, na.rm = TRUE),
  #             m_temperature = mean(temperature, na.rm = TRUE),
  #             m_pressure = mean(pressure, na.rm = TRUE),
  #             m_conductivity = mean(conductivity, na.rm = TRUE))

  dat <- data1[,c("depth", "temperature", "pressure", "conductivity", "second")]
  dat <- data.frame(base::sapply(X = dat, as.character))
  dat <- data.frame(base::sapply(X = dat, as.numeric))

  data1 <- stats::aggregate.data.frame(x = dat[, c("depth", "temperature",
                                                   "pressure", "conductivity")],
                                       by = list(timeS = data1$second),
                                       FUN = mean,
                                       na.rm = TRUE
  )


  # data1$timeS <- as.POSIXct(data1$timeS)
  # data0 <- oce::read.ctd(file = file.name)
  # data1<-data.frame(data0@data1)

  xx <- data0[(grepl(x = data0, pattern = "* cast   "))]
  xx <- gsub(pattern = "* cast   ", replacement = "", x = xx)
  xx <- strsplit(x = xx, split = ",")[[1]][1]
  xx <- strsplit(x = xx, split = " samples")[[1]][1]
  xx <- gsub(pattern = "\\*[0-9]+ ", x = xx, replacement = "")
  xx <- as.POSIXlt(xx, format = "%d %b %Y %H:%M:%S")
  data1$timeS <- as.numeric(as.character(data1$timeS))
  DATE_TIME <- format((xx + data1$timeS),
                      format = "%Y-%m-%d %H:%M:%S")
  DATE_TIME_btd <- format(as.POSIXct(DATE_TIME),
                          format = "%m/%d/%Y %H:%M:%S")

  DATE_TIME_btd <- format_date(DATE_TIME_btd)

  # DATE_TIME_btd <- gsub(pattern = "^0",
  #                       replacement = "",
  #                       x = DATE_TIME_btd)
  # DATE_TIME_btd <- gsub(pattern = "^0",
  #                       replacement = "",
  #                       x = DATE_TIME_btd)
  DATE_TIME <- format(as.POSIXct(DATE_TIME),
                      format = "%m/%d/%y %H:%M:%S")

  data1$DATE_TIME <- DATE_TIME
  # xx <- as.POSIXlt(data0@metadata$startTime,
  #                         format = "%Y-%m-%d %H:%M:%S")
  #
  # DATE_TIME = format(xx, format = "%m/%d/%Y %H:%M:%S")
  # DATE_TIME <- data1$DATE_TIME <- format((xx + data1$timeS),
  #                                       format = "%m/%d/%Y %H:%M:%S")

  HOST_TIME=max(data1$DATE_TIME)
  LOGGER_TIME=max(data1$DATE_TIME)
  LOGGING_START=min(data1$DATE_TIME)
  LOGGING_END=max(data1$DATE_TIME)
  TEMPERATURE=data1$temperature
  DEPTH=data1$depth
  SAMPLE_PERIOD=3
  NUMBER_CHANNELS=2
  NUMBER_SAMPLES=0
  MODE=2

  # Write BTD file
  # DATE_TIME <- format(as.POSIXct(DATE_TIME, "%m/%d/%y %H:%M:%S"),
  #                     format = "%m/%e/%Y %H:%M:%S")
  DATE_TIME<-DATE_TIME_btd

  new.BTD=cbind(VESSEL,CRUISE,HAUL,SERIAL_NUMBER,DATE_TIME,TEMPERATURE,DEPTH)
  new.BTD[which(is.na(new.BTD))]=""
  new.BTD <- data.frame(new.BTD)

  # Write BTH file
  new.BTH=cbind(VESSEL,CRUISE,HAUL,MODEL_NUMBER,VERSION_NUMBER,SERIAL_NUMBER,
                HOST_TIME,LOGGER_TIME,LOGGING_START,LOGGING_END,
                SAMPLE_PERIOD,NUMBER_CHANNELS,
                NUMBER_SAMPLES,MODE)
  new.BTH <- data.frame(new.BTH)
  new.BTD=new.BTD[new.BTD$DEPTH!="2000",]

  #head(new.BTD)
  #return(head(new.BTD))
  filename <- paste0(path_out, "HAUL",shaul,
                     ifelse(is.na(filename_add) | filename_add == "",
                            "", paste0("_", filename_add)))
  utils::write.csv(x = new.BTD,
                   file = paste0(filename, ".BTD"),
                   quote=F,
                   row.names=F,
                   eol="\n")
  utils::write.csv(x = new.BTH,
                   file = paste0(filename, ".BTH"),
                   quote=F,
                   row.names=F)

  message(paste0("Your new ", filename," .BTD and .BTH files are saved."))

}



#' Convert CTD .hex file to .cnv using SBE Data Processing
#'
#' Converts SBE19plus V2 CTD hex file to cnv using Sea-Bird Data Processing software. cnv files generated with this function will be properly formatted for conversion to BTD using convert_ctd_btd().
#'
#' @param hex_file_path Path to a single .hex file from the CTD.
#' @param xmlcon_path Path to CTD the config file (.xmlcon) for the CTD that was used to collect data in the .hex file.
#' @param bat_file Optional. Filepath to a batch (.bat) file to be executed in command line to run SBE Data Processing. Default NULL automatically uses the .bat file that is included in GAPsurvey.
#' @param datcnv_file Optional. Filepath to .psa file for the SBE Data Processing Datcnv module. Default NULL automatically uses the .psa file that is included in GAPsurvey.
#' @return Writes decoded CTD data to a cnv file and returns the path to the file as a 1L character vector.
#' @export
#'
#' @examples
#' # Copy system files to working directory for example
#' file.copy(from = system.file("exdata/convert_ctd_btd/2021_06_13_0003.hex",
#' package = "GAPsurvey"),
#' to = gsub(pattern = system.file("exdata/convert_ctd_btd/", package = "GAPsurvey"),
#'           replacement = getwd(),
#'           x = system.file("exdata/convert_ctd_btd/2021_06_13_0003.hex",
#'           package = "GAPsurvey")))
#'
#' file.copy(from = system.file("exdata/convert_ctd_btd/19-8102_Deploy2021.xmlcon",
#'                              package = "GAPsurvey"),
#'           to = gsub(pattern = system.file("exdata/convert_ctd_btd/",
#'                                            package = "GAPsurvey"),
#'                                            replacement = getwd(),
#'                     x = system.file("exdata/convert_ctd_btd/19-8102_Deploy2021.xmlcon",
#'                                     package = "GAPsurvey")))
#'
#' # Run convert_ctd_hex()
#' GAPsurvey::convert_ctd_hex(hex_file_path = "2021_06_13_0003.hex",
#'                             xmlcon_path = "19-8102_Deploy2021.xmlcon",
#'                             bat_file = NULL,
#'                             datcnv_file = NULL)
convert_ctd_hex <- function(hex_file_path,
                            xmlcon_path,
                            bat_file = NULL,
                            datcnv_file = NULL) {

  # Replace double backslashes in filepaths with forward slashes
  hex_file_path <- gsub(pattern = "\\\\", replacement = "/", x = hex_file_path)
  xmlcon_path<- gsub(pattern = "\\\\", replacement = "/", x = xmlcon_path)

  # Handle case where file is in the root directory
  if(dirname(hex_file_path) == ".") {
    hex_file_path <- paste0(getwd(), "/", hex_file_path)
  }

  if(dirname(xmlcon_path) == ".") {
    xmlcon_path <- paste0(getwd(), "/", xmlcon_path)
  }

  if(!file.exists(hex_file_path)) {
    stop("convert_ctd_hex: Specified hex_file_path (", hex_file_path,  ") does not exist. Please replace with a valid path to a .hex file.")
  }

  message("convert_ctd_hex: Attempting to convert ", hex_file_path, " to .cnv")

  out_path <- gsub(pattern = ".hex", replacement = "_raw.cnv", x = hex_file_path)

  # Specify paths to .bat and .psa files for SBE data processing
  if(is.null(bat_file)) {
    bat_file <- system.file("exdata/convert_ctd_btd/atsea_getdata.bat", package = "GAPsurvey")
  }

  if(is.null(datcnv_file)) {
    datcnv_file <- system.file("exdata/convert_ctd_btd/DatCnv.psa", package = "GAPsurvey")
  }

  # Run SBE data processing
  system(command = paste0("sbebatch \"",
                          bat_file, "\" \"",
                          hex_file_path, "\" \"",
                          datcnv_file, "\" \"",
                          xmlcon_path, "\" \"",
                          sub("/[^/]+$", "", hex_file_path), "\""
  )
  )

  if(file.exists(out_path)) {
    message("convert_ctd_hex: .cnv file created at ", out_path, ".")
  } else {
    stop("convert_ctd_hex: Failed to convert .hex to .cnv.")
  }

  return(out_path)
}


#' Recover position data from Globe .log file
#'
#' In the event that the MARPORT server GPS fails or is incomplete, "convert_log_gps()" converts GLOBE LOG files into a format that can be uploaded into WHEELHOUSE.
#' To get a .log file that is usable in this function,
#' 1) Go the C:\ globe\ logs\ 2018\ directory and choose GLG file with proper date
#' 2) Use GLOBE Files>Logs> to convert .GLG (binary) to a .LOG (.csv) file
#' 3) convert_log_gps()will prompt you for Vessel code, Cruise no., Haul no. and Date
#' 4) The final prompt will ask for the location of the GLOBE LOG file
#' 5) convert_log_gps()will create csv file in the R directory with filename "new.gps"
#' 6) Rename "new.gps" to HAULXXXX.GPS where XXXX is the haul number
#' 7) Upload HAULXXXX.GPS into WHEELHOUSE
#' 8) NOTE: The raw GLOBE log data are in GMT time (-8 hrs or 4PM AKDT prior day to 4PM current day. Hence if haul with missing GPS spans the 4PM hour (e.g.,3:45-4:30 PM),YOU WILL HAVE TO CONVERT TWO GLG files (current day and next day)and run convert_log_gps()twice & manually combine the two GPS files
#' 9) ALSO NOTE: You may have to shut down GLOBE or wait until after 4pm on following day before all the incoming NMEA data are written to the GLG file.
#'
#' Now that you have a .log file, you can RUN the function by putting your cursor on the "convert_log_gps()" line below & press CTRL+R.
#'
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param DATE Optional. Default = NA. The date in MM/DD/YYYY format (e.g., "06/02/2019"). If NA or not called in the function, a prompt will appear asking for this data.
#' @param path_in Optional. Default = "./., or the local working directory but any path (as a string) may be entered.
#' @param path_out Optional. The default is the local working directory but may be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputted file. Here, you can additional information that may make this file helpful to find later.
#'
#' @return A .GPS file to the path_out directory.
#' @export
#'
#' @examples
#' convert_log_gps(
#'     VESSEL = 94,
#'     CRUISE = 201901,
#'     HAUL = 3,
#'     DATE = "06/06/2017",
#'     path_in = system.file("exdata/convert_log_gps/06062017.log",
#'         package = "GAPsurvey"),
#'     path_out = getwd(),
#'     filename_add = "newlog")
#'
#'  convert_log_gps(
#'     VESSEL = 94,
#'     CRUISE = 202101,
#'     HAUL = 37,
#'     DATE = "06/07/2021",
#'     path_in = system.file("exdata/convert_log_gps/Haul0037.log",
#'         package = "GAPsurvey"),
#'     path_out = getwd(),
#'     filename_add = "newlog")
convert_log_gps <- function(
    VESSEL = NA,
    CRUISE = NA,
    HAUL = NA,
    DATE = NA,
    path_in,
    path_out = "./",
    filename_add = ""){

  if (is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if (is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if (is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if (is.na(DATE)){ DATE <- readline("Type date of haul (MM/DD/YYYY):  ") }

  path_in <- fix_path(path_in)
  file.name <- path_in

  # make sure path_in comes in with correct format
  path_out <- fix_path(path_out)

  HAUL <- as.numeric(HAUL)
  shaul <- numbers0(x = HAUL, number_places = 4)

  log.file<-utils::read.csv(file.name,header=F, sep=",")

  only.GPRMC<-log.file[log.file$V1=="$GPRMC",]
  # head(only.GPRMC)
  only.GPRMC<-only.GPRMC[,c(2,4,5,6,7)]
  # head(only.GPRMC)
  info<-cbind(VESSEL, CRUISE, HAUL, DATE)
  infoselect<-cbind(info,only.GPRMC)
  colnames(infoselect)<-c("VESSEL","CRUISE","HAUL","DATE","TIME","LAT1","LAT2","LONG1","LONG2")
  # head(infoselect)

  hh=as.numeric(substr(infoselect$"TIME",start=1, stop=2))
  hh=ifelse(hh<8,hh+24,hh)-8
  hh=ifelse(hh<10,paste0(0,hh),as.character(hh))
  mm=substr(infoselect$"TIME",start=3, stop=4)
  ss=substr(infoselect$"TIME",start=5, stop=6)
  DATE_TIME=paste(infoselect$"DATE", paste(hh,mm,ss,sep=":"))

  lat1=as.numeric(as.character(infoselect$LAT1))
  LAT=ifelse(infoselect$"LAT2"=="N",lat1,-lat1)
  LAT <- formatC(x = LAT, digits = 4, format = "f")

  long1=as.numeric(as.character(infoselect$LONG1))
  LONG=ifelse(infoselect$"LONG2"=="E",long1,-long1)
  LONG <- formatC(x = LONG, digits = 4, format = "f")

  new_gps <- cbind.data.frame(VESSEL, CRUISE, HAUL, DATE_TIME, LAT, LONG)


  filename <- paste0(path_out, "HAUL",shaul,
                     ifelse(is.na(filename_add) | filename_add == "",
                            "", paste0("_", filename_add)),
                     ".gps")

  new_gps1<-new_gps
  names(new_gps1) <- NULL
  new_gps1 <- as.matrix(new_gps1)

  utils::write.table(x = new_gps1,
                     file = filename,
                     quote=FALSE,
                     sep = ",",
                     row.names=FALSE,
                     col.names = FALSE,
                     eol="\n")

  message(paste0("Your new .gps files are saved to ", filename))

}



#' Convert .bvdr files to .marp files
#' @description
#' If you mistakenly delete the marport data for a haul, you can retrieve that data through this converter.
#' Before using this script,
#' 1. Open the .bvdr file in Notepad ++ or a similar text editor.
#' 2. Find the uninterpretable character symbol. Often, depending on the editor, this will look like a box or the highlighted letters "SUB". Find and delete (via replace) these characters for the whole document. An error will appear and only part of the file will be read (stopping at the line before where this unsupported symbol is) if you do not edit the data ahead of time.
#' 3. Save the .bvdr file with these changes and use the link to that file below for path_bvdr
#' For an example of what a proper .marp file looks like, refer to system.file("exdata/convert_bvdr_marp/HAUL0001.marp", package = "GAPsurvey")
#' @param path_bvdr Character string. The full path of the .bvdr file you want to convert. For example, path_bvdr <- system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr", package = "GAPsurvey")
#' @param verbose Logical. Default = FALSE. If you would like a readout of what the file looks like in the console, set to TRUE.
#'
#' @export
#' @examples
#' head(convert_bvdr_marp(path_bvdr = system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr",
#'                                   package = "GAPsurvey"),
#'           verbose = TRUE), 20)
convert_bvdr_marp <- function(path_bvdr,
                              verbose = FALSE) {

  dat <- readLines(con = path_bvdr, skipNul = TRUE)
  dat1 <- strsplit(x = dat, split = "\\$G")
  dat2 <- strsplit(x = dat, split = "\\:::")
  dat3 <- strsplit(x = dat, split = "\\$01TE")
  dat4 <- strsplit(x = dat, split = "\\$01DST")

  for (i in 1:length(dat1)) {
    if (length(dat1[i][[1]])>1) {
      # if (substr(x = dat1[i][[1]][2], start = 1, stop = 1) == "G"){
      dat1[i][[1]][2] <- paste0("$G", dat1[i][[1]][2])
      # }
    }
    if (length(dat2[i][[1]])>1) {
      dat1[i]<-dat2[i]
      dat1[i][[1]][2] <- paste0(":::", dat1[i][[1]][2])
    }
    if (length(dat3[i][[1]])>1) {
      dat1[i]<-dat3[i]
      dat1[i][[1]][2] <- paste0("$01TE", dat1[i][[1]][2])
    }
    if (length(dat4[i][[1]])>1) {
      dat1[i]<-dat4[i]
      dat1[i][[1]][2] <- paste0("$01DST", dat1[i][[1]][2])
    }
  }
  dat <- sapply(X = dat1, "[", 2)
  dat <- dat[!is.na(dat)]
  dat <- dat[!grepl(pattern = "\\$Gf", x = dat)]
  file_name_out <- gsub(pattern = ".bvdr", replacement = ".marp", x = path_bvdr, fixed = TRUE)
  writeLines(text = dat, con = file_name_out)

  if (verbose) {
    return(dat)
  }
}

# Calculate during hauls -------------------------------------------------------


#' Calculate Net Spread for tows missing net width using a glm.
#'
#' The Marport Deep Sea Technologies Inc. net mensuration system was used during the deployment of each tow to record net spread and net height. Net width was measured as the horizontal distance between two sensors attached immediately forward of the junction of the upper breastline and the dandyline, and net height was measured from the headrope center to the seafloor. A custom-made AFSC bottom contact sensor (accelerometer) attached to the center of the footrope was used to determine tow duration based on footrope contact with the seafloor. Mean calc_net_spread values for estimating area swept for the tow duration were calculated according to the methods described by Lauth and Kotwicki (2014).
#'
#' In race_data, this will manifest as...
#' net_mensuration_code = Net Mensuration Method
#' 0* Unidentified method. Will make racebase.haul$net_mesured = "N".
#' 1 Scanmar net mensuration - don't use, historical
#' 2 NetMind net mensuration - don't use, historical
#' 3 Furuno net mensuration - don't use, historical
#' 4* Estimated from other hauls - when missing net spread, or spread and hieght. Will make racebase.haul$net_mesured = "N". Estimated using GLM
#' 5* Estimated from warp angle - hopefully, will not come up and shouldn't be used
#' 6 Marport net mensuration - shouldn't be there, indicates raw data
#' 7* Marport with sequential outlier rejection, smoothed mean, and adjusted for MKII offset (see AFSC Proc. Report Lauth & Kotwicki 2014). Will make racebase.haul$net_mesured = "Y".
#'
#' @param dat data.frame. This can be data from GIDES or race_data.hauls, including these columns: WIRE_OUT, NET_HEIGHT, NET_SPREAD
#'
#' @export
#' @return a list of equations
#'
#' @examples
#' # Here is an example using 202101 Alaska Night Data from race_data.hauls:
#' path <- system.file("exdata/calc_net_spread/VESTY202101_RACE_DATA.HAULS.csv",
#'                     package = "GAPsurvey")
#' dat <- read.csv(file=path, header=TRUE, sep=",", stringsAsFactors = FALSE)
#' dat <- dat[dat$PERFORMANCE >= 0 & dat$HAUL_TYPE == 3,]
#' dat[dat$NET_SPREAD_METHOD != 7, "NET_SPREAD"] <- NA # a normal net width
#' dat[dat$NET_HEIGHT_METHOD != 6, "NET_HEIGHT"] <- NA # a normal net height
#' calc_net_spread(dat)
calc_net_spread <- function(dat) {

  dat0 <- dat[,names(dat) %in%
                c("HAUL", "NET_SPREAD",	"NET_HEIGHT",	"WIRE_OUT")]
  dat0$NET_SPREAD_METHODS <- 7 # No Issues
  dat0$NET_HEIGHT_METHOD <- 6 # No Issues
  dat0$WIRE_OUT_METHODS <- 5 # No Issues

  out <- data.frame("method_col" = c(),
                    "method_code" = c(),
                    "n" = c(),
                    "desc" = c())
  # 7: NET_SPREAD_METHOD Marport with sequential outlier rejection, smoothed mean, and adjusted for MKII offset (see AFSC Proc. Report Lauth & Kotwicki 2014). Will make racebase.haul$net_mesured = "Y".
  # if (nrow(dat0[complete.cases(dat0), ])>0) {
  #   n7 <- nrow(dat0[complete.cases(dat0), ])

  if (sum(!is.na(dat0$NET_SPREAD), na.rm = TRUE)>0) {
    n7 <- sum(!is.na(dat0$NET_SPREAD), na.rm = TRUE)
    out <- rbind.data.frame(out,
                            data.frame("method_col" = "NET_SPREAD_METHOD",
                                       "method_code" = 7,
                                       "n" = n7,
                                       "desc" = paste0(
                                         "There were ",
                                         n7,
                                         " hauls successfully completed where the Marport sensor properly caclulated net mensuration using sequential outlier rejection, smoothed mean, and MKII offset adjustments (see AFSC Proc. Report Lauth & Kotwicki 2014).")
                            ))
  }

  # When NET_HEIGHT_METHOD != 6 (aka NA)
  # TOLEDO: Will not work if there are no other tows with the same wire out!
  if (nrow(dat0[!is.na(dat0$NET_HEIGHT), ]) != nrow(dat0)) {
    scope_where_height_missing <- dat0$WIRE_OUT[is.na(dat0$NET_HEIGHT)]
    # out$tows_without_height <- length(scope_where_height_missing)
    dat0$NET_HEIGHT_METHOD[which(is.na(dat0$NET_HEIGHT))] <- 4
    for (i in 1:length(unique(scope_where_height_missing))) {
      dat0$NET_HEIGHT[which(is.na(dat0$NET_HEIGHT) &
                              dat0$WIRE_OUT == unique(scope_where_height_missing)[i])] <-
        mean(dat0$NET_HEIGHT[dat0$WIRE_OUT == unique(scope_where_height_missing)[i]], na.rm = TRUE)
    }

    out <- rbind.data.frame(out,
                            data.frame("method_col" = "NET_HEIGHT_METHOD",
                                       "method_code" = 4,
                                       "n" = length(scope_where_height_missing),
                                       "desc" = paste0(
                                         "There were ",
                                         length(scope_where_height_missing),
                                         " missing net height values estimated by averaging the net height of tows with the same wire out scope.")
                            ))

  }

  # 4* NET_SPREAD_METHOD Estimated from other hauls - when missing net spread, or spread and height Will make racebase.haul$net_mesured = "N". Estimated using GLM
  glm_param <- ""
  if (nrow(dat0[!is.na(dat0$NET_SPREAD), ]) != nrow(dat0)) {

    # Find the best model
    dat1 <- dat0[!is.na(dat0$NET_SPREAD),]
    dat1$INVSCOPE <- 1/dat1$WIRE_OUT
    glm1 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT + NET_HEIGHT*INVSCOPE,
                      data=c(dat1), family="gaussian")
    glm2 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT,
                      data=c(dat1), family="gaussian")
    glm3 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT*INVSCOPE,
                      data=c(dat1), family="gaussian")
    glm4 = stats::glm(NET_SPREAD  ~ NET_HEIGHT + NET_HEIGHT*INVSCOPE,
                      data=c(dat1), family="gaussian")
    glm5 = stats::glm(NET_SPREAD  ~ INVSCOPE,
                      data=c(dat1), family="gaussian")
    glm6 = stats::glm(NET_SPREAD  ~ NET_HEIGHT,
                      data=c(dat1), family="gaussian")
    glm7 = stats::glm(NET_SPREAD  ~ NET_HEIGHT*INVSCOPE,
                      data=c(dat1), family="gaussian")

    best_model <- data.frame(stats::AIC(glm1, glm2, glm3, glm4, glm5, glm6, glm7))
    best_model<-best_model[best_model$AIC <= min(best_model$AIC)+3,] # +3 because basically the same if within 3 of min
    best_model<-best_model[best_model$df <= min(best_model$df),]
    best_model<-best_model[best_model$AIC <= min(best_model$AIC),]

    glm0 <- get(x = rownames(best_model)[1])
    glm0_sum <- summary(glm0)

    # Predict data
    dat0$NET_SPREAD_METHODS[is.na(dat0$NET_SPREAD)] <- 4

    dat1 <- dat0[is.na(dat0$NET_SPREAD),]
    dat1$INVSCOPE <- 1/dat1$WIRE_OUT
    n4 <- nrow(dat1)

    dat0$NET_SPREAD[is.na(dat0$NET_SPREAD)] <-
      stats::predict.glm(object = glm0,
                         newdata = dat1,
                         type="response")

    # Collect coeficents
    glm0_param <- data.frame(glm0_sum$coefficients)
    names(glm0_param) <- c("est0","se","tvalue","prob")
    glm0_param$var <- rownames(glm0_param)
    glm0_param$est <- round(x = glm0_param$est0, digits = 3)
    glm0_param$case <- NA
    for (i in 1:nrow(glm0_param)) {
      if (glm0_param$prob[i] < 0.001) {
        glm0_param$case[i] <- "very signifcant (P < 0.001)"
        # } else if (glm0_param$prob[i] < 0.001) {
        #   glm0_param$case[i] <- "mostly significant (P < 0.001)"
      } else if (glm0_param$prob[i] < 0.01) {
        glm0_param$case[i] <- "significant (P < 0.001)"
      } else if (glm0_param$prob[i] < 0.05) {
        glm0_param$case[i] <- "somewhat significant (P < 0.001)"
      }
    }
    glm0_param$var0 <- tolower(row.names(glm0_param))
    glm0_param$var0 <- gsub(pattern = "(",
                            replacement = "",
                            x = glm0_param$var0,
                            fixed = TRUE)
    glm0_param$var0 <- gsub(pattern = ")",
                            replacement = "",
                            x = glm0_param$var0,
                            fixed = TRUE)
    glm0_param$var0 <- gsub(pattern = ":",
                            replacement = " x ",
                            x = glm0_param$var0,
                            fixed = TRUE)
    glm0_param$var0 <- gsub(pattern = "invscope",
                            replacement = "inversed scope",
                            x = glm0_param$var0,
                            fixed = TRUE)
    glm0_param$var0 <- gsub(pattern = "_",
                            replacement = " ",
                            x = glm0_param$var0,
                            fixed = TRUE)



    str0 <- c()
    if (length(unique(glm0_param$case)) == 1) {
      str0 <- paste0("both predictor variables and their interaction were significant (P < 0.001)")
    } else {
      for (i in 1:length(unique(glm0_param$case))) {
        str0 <- c(str0, paste0(
          text_list(glm0_param$var[glm0_param$case == unique(glm0_param$case)[i]]),
          " were ",
          unique(glm0_param$case)[i]
        ))
      }
      str0 <- text_list(str0)
    }


    fm <- as.character(glm0$formula)
    fm <- paste0(fm[2], " ",
                 fm[1], " ",
                 glm0_param$est[glm0_param$var == "(Intercept)"], " + ",
                 fm[3])

    fm <- gsub(pattern = "NET_SPREAD",
               replacement = "w",
               x = fm, fixed = TRUE)
    if (sum(glm0_param$var == "INVSCOPE:NET_HEIGHT")>0) {
      fm <- gsub(pattern = "NET_HEIGHT * INVSCOPE",
                 replacement = paste0(glm0_param$est[glm0_param$var == "INVSCOPE:NET_HEIGHT"],
                                      " * (h/s)"),
                 x = fm, fixed = TRUE)
    }
    if (sum(glm0_param$var == "NET_HEIGHT")>0) {
      fm <- gsub(pattern = "NET_HEIGHT",
                 replacement = paste0(glm0_param$est[glm0_param$var == "NET_HEIGHT"],
                                      " * h"),
                 x = fm, fixed = TRUE)
    }
    if (sum(glm0_param$var == "INVSCOPE")>0) {
      fm <- gsub(pattern = "INVSCOPE",
                 replacement = paste0(glm0_param$est[glm0_param$var == "INVSCOPE"],
                                      "/s"),
                 x = fm, fixed = TRUE)
    }
    fm <- gsub(pattern = " + -",
               replacement = " - ",
               x = fm, fixed = TRUE)

    out <- rbind.data.frame(out,
                            data.frame("method_col" = "NET_SPREAD_METHOD",
                                       "method_code" = 4,
                                       "n" = n4,
                                       "desc" = paste0(fm,
                                                       ": For ",
                                                       n4,
                                                       " hauls, the net width was estimated using a generalized linear model. The ",
                                                       str0)
                            ))


    # # if the interaction between INVSLOPE and HEIGHT are not significant
    # if ((glm1_sum$coefficients[4,4]>0.05)) {
    #   # Find the best model
    #   glm2 = glm(SPREAD ~ INVSCOPE + HEIGHT,
    #              data=c(dat),family="gaussian")
    #   glm2_sum <- summary(glm2)
    #   glm0<-glm2
    #   eq <- "w ~ 1/s + h: inverse scope and net height were significant (P < 0.001) but not their interaction term"
    # }
    # str <- paste0(str, eq, "
    #
    #               ")
  }

  # 0* Unidentified method. Will make racebase.haul$net_mesured = "N".

  # 5 Estimated from warp angle - hopefully, will not come up and shouldn't be used


  out <- list(#"plot" = plot(dat$NET_SPREAD,1/dat$WIRE_OUT),
    "actions" = out,
    "glm_summary" = glm0_param,
    "dat" = dat0)

  return(out)
}

#' Get sunrise and sunset times by day, latitude, and longitude
#'
#' @param chosen_date Date or charater. Formatted as "YYYY-MM-DD"
#' @param latitude Numeric. Fill in only if survey and station are not entered. latitude in either decimal degrees or a character latitude in degrees and decimal minutes
#' @param longitude Numeric. Fill in only if survey and station are not entered. Longitude in either decimal degrees or a character longitude in degrees and decimal minutes
#' @param survey Character. Fill in only if latitude and longitude are not entered. A character string of the survey you are interested in reivewing. Options are those from public_data$survey, which are "AI", "GOA", "EBS", "NBS", "BSS".
#' @param station Character. Fill in only if latitude and longitude are not entered. A character string of the current station name (as a grid cell; e.g., "264-85"). Stations defined in the station_coords dataset.
#' @param verbose Logical. Default = FALSE. If you would like a readout of what the file looks like in the console, set to TRUE.
#' @param timezone Character. Default = "US/Alaska." Other options include: "US/Aleutian"
#'
#' @return Time of sunrise and sunset in text. Also shows a pop-up with sunrise and sunset times.
#' @export
#'
#' @examples
#' # Find times based on lat/lon for today's date, where date is a date object
#' get_sunrise_sunset(chosen_date = Sys.Date(),
#'                    latitude = 63.3,
#'                    longitude = -170.5)
#'
#' # Find times based on lat/lon for today's date, where date is a character
#' get_sunrise_sunset(chosen_date = "2023-06-05",
#'                    latitude = 63.3,
#'                    longitude = -170.5)
#' # Find times based on lat/lon for today's date, where date is a character
#' # and lat/lon in degree decimal-minutes
#' get_sunrise_sunset(chosen_date = "2023-06-05",
#'                    latitude = "63 18.0",
#'                    longitude = "-170 30.0")
#' # Find times based on a survey (EBS) station's recorded lat/lon for today's date
#' get_sunrise_sunset(chosen_date = Sys.Date(),
#'                    survey = "EBS",
#'                    station = "I-13")
#' # Find times based on a survey (GOA) station's recorded lat/lon for today's date
#' get_sunrise_sunset(chosen_date = Sys.Date(),
#'                    survey = "GOA",
#'                    station = "323-176")
#' # Find times based on a survey (AI) station's recorded lat/lon for today's date
#' # get_sunrise_sunset(chosen_date = "2023-06-10",
#' #                     survey = "AI",
#' #                    station = "33-47")

get_sunrise_sunset <- function(
    chosen_date,
    latitude = NULL,
    longitude = NULL,
    survey = NULL,
    station = NULL,
    verbose = FALSE,
    timezone = "US/Alaska") {

  # Function to format dates
  format_date <- function(x) {

    if(x > 24) {
      x <- x - 24
    }

    hour <- unlist(strsplit(as.character(floor(x)), split = ""))

    hour_vec <- c("0", "0")

    if(length(hour) == 2) {
      hour_vec <- hour
    } else {
      hour_vec[2] <- hour
    }

    min_vec <- c("0", "0")

    minutes <- unlist(strsplit(as.character(floor(x%%1*60)), split = ""))

    if(length(minutes) == 2) {
      min_vec <- minutes
    } else {
      min_vec[2] <- minutes
    }

    out <- paste0(paste(hour_vec, collapse = ""), ":", paste(min_vec, collapse = ""))

    return(out)

  }

  chosen_date <- as.POSIXct(x = as.character(chosen_date), tz = timezone)

  if (timezone == "US/Alaska") {
    sel_tz <- -8
  } else if (timezone == "US/Aleutian") {
    sel_tz <- -9
  }

  # Are lat/long in degrees and decimal mins? If so, convert to decimal degrees.
  if (!is.null(latitude) | !is.null(longitude)) {
    message("Using latitude and longitude to calcualte sunrise and sunset. ")
    ddm <- is.character(latitude) | is.character(longitude)
    if (ddm) {
      if (!grepl(" ", x = latitude) | !grepl(" ", x = longitude)) {
        stop("You have chosen degrees and decimal minutes but have no space in the character string you entered. Please format your lat and/or long as D mm.m OR enter a numeric value for decimal degrees")
      }
      lat_deg <- as.numeric(gsub(" .*$", "", latitude))
      lat_min <- as.numeric(gsub("^\\S+\\s+", "", latitude)) / 60
      latitude <- lat_deg + lat_min

      lon_deg <- as.numeric(gsub(" .*$", "", longitude))
      lon_min <- as.numeric(gsub("^\\S+\\s+", "", longitude)) / 60
      longitude <- lon_deg + lon_min
    }
  }

  if (!is.null(survey) | !is.null(station)) {

    utils::data("station_coords", envir=environment())

    station_coords0 <-
      GAPsurvey::station_coords[GAPsurvey::station_coords$srvy == survey &
                                  GAPsurvey::station_coords$station == station,
                                c("srvy", "station",
                                  "latitude_dd", "longitude_dd")]
    if (nrow(station_coords0) == 0) {
      stop("This station does not exist in this survey. ")
    }

    latitude <- station_coords0$latitude[1]
    longitude <- station_coords0$longitude[1]
    message(paste0("Using survey station (",survey,
                   " ",station,
                   ") centroid location information (lat = ",
                   round(x = latitude, digits = 3),
                   ", lon = ",
                   round(x = longitude, digits = 3),
                   ") to calculate sunrise and sunset. "))

  }

  # crds0 = matrix(c(longitude, latitude),
  #                nrow = 1)

  date_vec <- unlist(strsplit(as.character(chosen_date), split = ""))

  ac4r_output <- astrcalc4r(
    day = as.numeric(paste(date_vec[9:10], collapse = "")),
    month = as.numeric(paste(date_vec[6:7], collapse = "")),
    year = as.numeric(paste(date_vec[1:4], collapse = "")),
    hour = 7,
    # Arguments longitude and timezone must have the same sign if input time is
    # not UTC (timezone != 0).  In particular, if timezone !=0, both lon and timezone must
    # be negative for locations in western hemisphere and positive for locations in the
    # eastern hemisphere.  Check and fix input data if warranted. If data are correct
    # then convert input time (argument hour) to UTC and use timezone=zero.
    # This problem  occurs  1  times at rows:  1
    timezone = sel_tz,
    lat = latitude,
    lon = longitude,
    withinput = FALSE,
    seaorland = "maritime",
    acknowledgment = FALSE)

  sunrise <- format_date(ac4r_output$sunrise)
  sunset <- format_date(ac4r_output$sunset)

  # sunrise <- maptools::sunriset(
  #   crds = crds0,
  #   dateTime = chosen_date,
  #   direction = "sunrise",
  #   POSIXct.out = TRUE
  # )$time
  #
  # sunset <- maptools::sunriset(
  #   crds = crds0,
  #   dateTime = chosen_date,
  #   direction = "sunset",
  #   POSIXct.out = TRUE
  # )$time

  message(
    "Sunrise is at ", sunrise, " AKST",
    "\nSunset is at ", sunset, " AKST"
  )
}



#' astrocalc4r: Solar zenith angles for biological research
#'
#' From Jacobsen et al. (2011). Documentation copied from Jacobsen et al. (2011). This function calculates the solar zenith, azimuth and declination angles, time at sunrise, local noon and sunset, day length, and PAR (photosynthetically available radiation, 400-700 nm) under clear skies and average atmospheric conditions (marine or continental) anywhere on the surface of the earth based on date, time, and location.
#'
#' @param day day of month in the local time zone (integers). Value is required. Multiple observations should be enclosed with the c() function.
#' @param month month of year in the local time zone (integers). Value is required. Multiple observations should be enclosed with the c() function.
#' @param year year in the local time zone (integers).Value is required. Multiple observations should be enclosed with the c() function.
#' @param hour local time for each observation (decimal hours, e.g. 11:30 PM is 23.5, real numbers). Value is required. Multiple observations should be enclosed with the c() function.
#' @param timezone local time zone in +/- hours relative to GMT to link local time and GMT. For example, the difference between Eastern Standard Time and GMT is -5 hours. Value is required. Multiple observations should be enclosed with the c() function. timezone should include any necessary adjustments for daylight savings time.
#' @param lat Latitude in decimal degrees (0o to 90 o in the northern hemisphere and -90 o to 0 o degrees in the southern hemisphere, real numbers). For example, 42o 30' N is 42.5 o and 42o 30' S is -42.5o. Value is required. Multiple observations should be enclosed with the c() function.
#' @param lon Longitude in decimal degrees (-0 o to 180 o in the western hemisphere and 0o to 180 o in the eastern hemisphere, real numbers). For example, 110o 15' W is -110.25 o and 110o 15' E is 110.25o. Value is required. Multiple observations should be enclosed with the c() function.
#' @param withinput logical:TRUE to return results in a dataframe with the input data; otherwise FALSE returns a dataframe with just results. Default is FALSE.
#' @param seaorland text: "maritime" for typical maritime conditions or "continental" for typical continental conditions. Users must select one option or the other based on proximity to the ocean or other factors.
#' @param acknowledgement logical: use TRUE to output acknowledgement. Default is FALSE.
#' @noRd
#' @details Astronomical definitions are based on definitions in Meeus (2009) and Seidelmann (2006). The solar zenith angle is measured between a line drawn "straight up" from the center of the earth through the observer and a line drawn from the observer to the center of the solar disk. The zenith angle reaches its lowest daily value at local noon when the sun is highest. It reaches its maximum value at night after the sun drops below the horizon. The zenith angle and all of the solar variables calculated by astrocalc4r depend on latitude, longitude, date and time of day. For example, solar zenith angles measured at the same time of day and two different locations would differ due to differences in location. Zenith angles at the same location and two different dates or times of day also differ. Local noon is the time of day when the sun reaches its maximum elevation and minimum solar zenith angle at the observers location. This angle occurs when the leading edge of the sun first appears above, or the trailing edge disappears below the horizon (0.83o accounts for the radius of the sun when seen from the earth and for refraction by the atmosphere). Day length is the time in hours between sunrise and sunset. Solar declination and azimuth angles describe the exact position of the sun in the sky relative to an observer based on an equatorial coordinate system (Meeus 2009). Solar declination is the angular displacement of the sun above the equatorial plane. The equation of time accounts for the relative position of the observer within the time zone and is provided because it is complicated to calculate. PAR isirradiance in lux (lx, approximately W m-2) at the surface of the earth under clear skies calculated based on the solar zenith angle and assumptions about marine or terrestrial atmospheric properties. astrocalc4r calculates PAR for wavelengths between 400-700 nm. Calculations for other wavelengths can be carried out by modifying the code to use parameters from Frouin et al. (1989). Following Frouin et al. (1989), PAR is assumed to be zero at solar zenith angles >= 90o although some sunlight may be visible in the sky when the solar zenith angle is < 108o. Angles in astrocalc4r output are in degrees although radians are used internally for calculations. Time data and results are in decimal hours (e.g. 11:30 pm = 23.5 h) local time but internal calculations are in Greenwich Mean Time (GMT). The user must specify the local time zone in terms of +/- hours relative to GMT to link local time and GMT. For example, the difference between Eastern Standard Time and GMT is -5 hours. The user must ensure that any adjustments for daylight savings time are included in the timezone value. For example, timezone=-6 for Eastern daylight time.
#' @return Time of solar noon, sunrise and sunset, angles of azimuth and zenith, eqtime, declination of sun, daylight length (hours) and PAR.
#' @author Larry Jacobson, Alan Seaver, and Jiashen Tang NOAA National Marine Fisheries Service Northeast Fisheries Science Center, 166 Water St., Woods Hole, MA 02543
#' @examples astrocalc4r(day=12,month=9,year=2000,hour=12,timezone=-5,lat=40.9,lon=-110)

astrcalc4r <- function (day, month, year, hour, timezone, lat, lon, withinput = FALSE,
                        seaorland = "maritime", acknowledgment = FALSE)
{
  if (acknowledgment) {
    cat("\n", "---------------------------------------------------------")
    cat("\n", "                AstroCalcPureR Version 2.3")
    cat("\n", "Documentation: Jacobson L, Seaver A, Tang J. 2011. AstroCalc4R:")
    cat("\n", "software to calculate solar zenith angle; time at sunrise,")
    cat("\n", "local noon and sunset; and photosynthetically available")
    cat("\n", "radiation based on date, time and location. US Dept Commer,")
    cat("\n", "Northeast Fish Sci Cent Ref Doc. 11-14; 10 p. Available from:")
    cat("\n", "National Marine Fisheries Service, 166 Water Street, ")
    cat("\n", "Woods Hole, MA 02543-1026, or online at")
    cat("\n", "http://nefsc.noaa.gov/publications/")
    cat("\n \n", "Available in fishmethods library.  Contact the fishmethods")
    cat("\n", "administrator or Larry Jacobson (NOAA, National Marine")
    cat("\n", "Fisheries Service-retired) at larryjacobson6@gmail.com")
    cat("\n", "for assitance.")
    cat("\n\n", "Useage:")
    cat("\n", "    AstroCalcPureR(day,month,year,hour,timezone,")
    cat("\n", "                   lat,lon,withinput=F,")
    cat("\n", "                   seaorland='maritime',")
    cat("\n", "                   acknowledgment=TRUE)")
    cat("\n\n", "HINT: set acknowledgment=FALSE to avoid this message")
    cat("\n", "---------------------------------------------------------",
        "\n")
  }
  options(digits = 9)
  deg2rad <- pi/180
  null.c <- function(x) return(sum(is.null(x)))
  if (sum(null.c(day), null.c(month), null.c(year), null.c(hour),
          null.c(timezone), null.c(lat), null.c(lon)) > 0)
    stop("\n Null or missing required data vector for day, month, year, timezone, lat or lon \n")
  if ((length(day) != length(month)) | (length(month) != length(year)) |
      (length(year) != length(hour)) | (length(hour) != length(timezone)) |
      (length(timezone) != length(lat)) | (length(lat) != length(lon)))
    stop("\n Input vectors are not the same length \n")
  times <- length(day)
  na.c <- function(x) return(sum(is.na(x)))
  if (sum(na.c(day), na.c(month), na.c(year), na.c(hour), na.c(timezone),
          na.c(lat), na.c(lon)) > 0)
    stop("\n NA values in input data \n")
  logic1 <- year < 0
  if (sum(logic1) > 0)
    stop(cat("\n Error in year at rows:", (1:times)[logic1],
             " \n\n"))
  is.leap <- function(x) return((((x%%4 == 0) & (x%%100 !=
                                                   0))) | (x%%400 == 0))
  date.list <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30,
                 31)
  logic1 <- abs(month - 6) > 6
  if (sum(logic1) > 0)
    stop(cat("\n Error in month at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- day > (date.list[month] + is.leap(year) * (month ==
                                                         2))
  logic2 <- day <= 0
  if ((sum(logic1) > 0) | (sum(logic2) > 0))
    stop(cat("\n Incorrect month-day-year combination at rows: ",
             (1:times)[logic1 | logic2], " \n\n"))
  logic1 <- abs(hour - 12) > 12
  if (sum(logic1) > 0)
    stop(cat("\n Error in hour at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- abs(timezone) > 12
  if (sum(logic1) > 0)
    stop(cat("\n Error in time zone at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- abs(lon) > 180
  if (sum(logic1) > 0)
    stop(cat("\n Error in longitude at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- abs(lat) > 90
  if (sum(logic1) > 0)
    stop(cat("\n Error in latitude at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- sign(lon) == sign(timezone)
  logic2 <- timezone == 0
  logic3 <- !(logic1 | logic2)
  if (sum(logic3) != 0)
    stop(cat("\n \n Arguments longitude and timezone must have the same sign if input time is",
             "\n not UTC (timezone != 0).  In particular, if timezone !=0, both lon and timezone must",
             "\n be negative for locations in western hemisphere and positive for locations in the",
             "\n eastern hemisphere.  Check and fix input data if warranted. If data are correct",
             "\n then convert input time (argument hour) to UTC and use timezone=zero.",
             "\n This problem  occurs ", sum(logic3), " times at rows: ",
             (1:times)[logic3], "\n\n"))
  JulianDay <- function(xday, xmonth, xyear) {
    mm <- xmonth
    xmonth[mm <= 2] <- xmonth[mm <= 2] + 12
    xyear[mm <= 2] <- xyear[mm <= 2] - 1
    xa <- floor(xyear/100)
    xb <- 2 - xa + floor(xa/4)
    jd <- floor(365.25 * (xyear + 4716)) + floor(30.6001 *
                                                   (xmonth + 1)) + xday + xb - 1524.5
    return(jd)
  }
  daymonth <- function(mth, yr) {
    day[is.leap(yr)] <- c(31, 29, 31, 30, 31, 30, 31, 31,
                          30, 31, 30, 31)[mth[is.leap(yr)]]
    day[!is.leap(yr)] <- c(31, 28, 31, 30, 31, 30, 31, 31,
                           30, 31, 30, 31)[mth[!is.leap(yr)]]
    return(day)
  }
  parcalc <- function(zenith, setting = seaorland) {
    I0 <- 531.2
    V <- 23
    uv <- 1.4
    u0 <- 0.34
    r <- 0.05
    d <- 1
    if (!setting %in% c("maritime", "continental"))
      stop("setting value is neither 'maritime' nor 'continental'!")
    if (setting == "maritime") {
      a <- 0.068
      b <- 0.379
      a1 <- 0.117
      b1 <- 0.493
      av <- 0.002
      bv <- 0.87
      a0 <- 0.052
      b0 <- 0.99
    }
    else if (setting == "continental") {
      a <- 0.078
      b <- 0.882
      a1 <- 0.123
      b1 <- 0.594
      av <- 0.002
      bv <- 0.87
      a0 <- 0.052
      b0 <- 0.99
    }
    zrad <- zenith * deg2rad
    x1 <- uv/cos(zrad)
    xx <- exp(-av * x1^bv)
    x2 <- u0/cos(zrad)
    xxx <- exp(-a0 * x2^b0)
    xa <- a + b/V
    xb <- d - r * (a1 + b1/V)
    par <- I0 * cos(zrad) * exp(-xa/cos(zrad))/xb * xx *
      xxx
    par[zenith > 89.9999] <- 0
    return(par)
  }
  output <- as.data.frame(matrix(nrow = 0, ncol = 9))
  names(output) <- c("noon", "sunrise", "sunset", "azimuth",
                     "zenith", "eqtime", "declin", "daylight", "PAR")
  hourtemp <- hour - timezone
  hour <- ifelse(hourtemp > 24, hourtemp - 24, hourtemp)
  change_day <- !(hour == hourtemp)
  dm <- daymonth(month, year)
  daytemp <- day
  daytemp[change_day] <- ifelse((day[change_day] < dm[change_day]),
                                day[change_day] + 1, 1)
  change_month <- abs(day - daytemp) > 1
  monthtemp <- month
  monthtemp[change_month] <- ifelse(month[change_month] < 12,
                                    month[change_month] + 1, 1)
  change_year <- abs(month - monthtemp) > 1
  yeartemp <- year
  yeartemp[change_year] <- year[change_year] + 1
  xy <- yeartemp
  xm <- monthtemp
  xd <- daytemp + hourtemp/24
  jd <- JulianDay(xd, xm, xy) * 100/100
  jc <- (jd - 2451545)/36525
  xx <- 280.46646 + 36000.76983 * jc + 0.0003032 * jc^2
  gmls <- xx%%360
  xx <- 357.52911 + 35999.05029 * jc - 0.0001537 * jc^2
  gmas <- xx%%360
  eeo <- 0.016708634 - 4.2037e-05 * jc - 1.267e-07 * jc^2
  scx <- (1.914602 - 0.004817 * jc - 1.4e-05 * jc^2) * sin(gmas *
                                                             deg2rad) + (0.019993 - 0.000101 * jc) * sin(2 * gmas *
                                                                                                           deg2rad) + 0.000289 * sin(3 * gmas * deg2rad)
  Stl <- gmls + scx
  Sta <- gmas + scx
  srv <- 1.000001018 * (1 - eeo^2)/(1 + eeo * cos(Sta * deg2rad))
  omega <- 125.04 - 1934.136 * jc
  lambda <- Stl - 0.00569 - 0.00478 * sin(omega * deg2rad)
  epsilon <- (23 + 26/60 + 21.448/60^2) - (46.815/60^2) * jc -
    (0.00059/60^2) * jc^2 + (0.001813/60^2) * jc^3
  oblx <- 0.00256 * cos(omega * deg2rad)
  epsilon <- epsilon + oblx
  alpha <- atan2(cos(epsilon * deg2rad) * sin(lambda * deg2rad),
                 cos(lambda * deg2rad))/deg2rad
  declin <- asin(sin(epsilon * deg2rad) * sin(lambda * deg2rad))/deg2rad
  y <- tan(epsilon * deg2rad/2)^2
  eqtime <- (y * sin(2 * gmls * deg2rad) - 2 * eeo * sin(gmas *
                                                           deg2rad) + 4 * eeo * y * sin(gmas * deg2rad) * cos(2 *
                                                                                                                gmls * deg2rad) - y^2 * sin(4 * gmls * deg2rad)/2 - 5/4 *
               eeo^2 * sin(2 * gmas * deg2rad))/deg2rad * 4
  h0 <- -0.8333 * deg2rad
  phi <- lat * deg2rad
  hangle <- acos((sin(h0) - sin(declin * deg2rad) * sin(phi))/cos(declin *
                                                                    deg2rad)/cos(phi))/deg2rad
  noon <- (720 - 4 * lon + timezone * 60 - eqtime)/1440
  sunrise <- (noon * 1440 - hangle * 4)/1440 * 24
  sunset <- (noon * 1440 + hangle * 4)/1440 * 24
  noon <- noon * 24
  daylight <- hangle * 8
  tst <- (hourtemp * 60 + eqtime + 4 * lon)%%1440
  tsa <- ifelse(tst < 0, tst/4 + 180, tst/4 - 180)
  zenith <- 90 - asin(sin(lat * deg2rad) * sin(declin * deg2rad) +
                        cos(lat * deg2rad) * cos(declin * deg2rad) * cos(tsa *
                                                                           deg2rad))/deg2rad
  azimuth <- acos((sin(lat * deg2rad) * sin((90 - zenith) *
                                              deg2rad) - sin(declin * deg2rad))/cos(lat * deg2rad)/cos((90 -
                                                                                                          zenith) * deg2rad))/deg2rad + 180
  azimuth <- ifelse(tsa > 0, azimuth%%360, 360 - azimuth%%360)
  daylight <- daylight/60
  PAR <- parcalc(zenith)
  if (any(is.nan(sunrise))) {
    message(paste("Warning: Polar day/night (daylength 0 or 24 hrs) at record(s):",
                  (1:times)[is.nan(sunrise)], "\n Check input data (i.e. latitude)?"))
    daylight <- ifelse(PAR > 0, 24, 0)
  }
  output <- rbind(output, data.frame(noon = noon, sunrise = sunrise,
                                     sunset = sunset, azimuth = azimuth, zenith = zenith,
                                     eqtime = eqtime, declin = declin, daylight = daylight,
                                     PAR = PAR))
  if (withinput)
    return(cbind(data.frame(tzone = timezone, day = day,
                            month = month, year = year, hhour = hour, xlat = lat,
                            xlon = lon), output))
  else return(output)
}



#' Find historical catch data from previous years
#'
#' @param survey (character) A character string of the survey you are interested in reivewing. Options are those from public_data$survey, which are "AI", "GOA", "EBS", "NBS", "BSS".
#' @param species_codes (numeric) A species code number of a species or species you are specifically interested in reviewing data from. If NA/not entered, the function will return data for all species caught in the haul.
#' @param station (character) A character string of the current station name (as a grid cell; e.g., "264-85")
#' @param grid_buffer (numeric) GOA/AI only. The number of cells around the current station where you would like to see catches from. Typically, use grid_buffer = 3.
#' @param years (numeric) the years you want returned in the output. If years = NA, script will default to the last 10 years. If you would like to see all years, simply choose a large range that covers all years of the survey (e.g., 1970:2030)
#'
#' @export
#' @return a data.frame of past catches and hauls
#'
#' @examples
#' #' # EBS (or NBS) --------------------------------------------------------------
#'
#' ## for one year and only 1 station for all species --------------------------
#' get_catch_haul_history(
#'      survey = "EBS",
#'      years = 2021,
#'      station = "I-13")
#'
#' ## for default 10 years and only 1 station  for PCOD and walleye pollock ----
#' get_catch_haul_history(
#'      species_codes = c(21720, 21740), # pacific cod and walleye pollock
#'      survey = "EBS",
#'      station = "I-13")
#'
#' # AI (or GOA) ---------------------------------------------------------------
#'
#' ## for two specific years and nearby stations -------------------------------
#' get_catch_haul_history(
#'       survey = "AI",
#'       years = c(2016, 2018),
#'       station = "324-73",
#'       grid_buffer = 3)
#'
#' ## for default 10 years and nearby stations for all species (a typical use-case) ----
#' get_catch_haul_history(
#'      survey = "AI",
#'      years = NA, # default
#'      station = "324-73",
#'      grid_buffer = 3)
#'
#' ## for default 10 years and nearby stations for Bering Flounder (0 results returned!) ---
#' get_catch_haul_history(
#'      survey = "AI",
#'      species_codes = 10140, # Bering flounder which would be VERY unlikely to be found
#'      years = NA, # default
#'      station = "324-73",
#'      grid_buffer = 3)
get_catch_haul_history <- function(
    survey,
    species_codes = NA,
    years = NA,
    station,
    grid_buffer = NA) {

  utils::data("public_data", envir=environment())

  public_data0 <-
    GAPsurvey::public_data[GAPsurvey::public_data$srvy == survey,
                           c("year", "srvy", "haul", "stratum", "station",
                             "vessel_name", "vessel_id", "date_time", "latitude_dd_start", "longitude_dd_start",
                             "species_code", "common_name", "scientific_name", "taxon_confidence",
                             "cpue_kgkm2", "cpue_nokm2", "weight_kg", "count",
                             "bottom_temperature_c", "surface_temperature_c", "depth_m",
                             "distance_fished_km", "net_width_m", "net_height_m", "area_swept_km2", "duration_hr")]

  if (!is.na(years[1])) {
    public_data0 <- public_data0[public_data0$year %in% years,]
  } else { # default: show 10 years average
    public_data0 <- public_data0[public_data0$year %in% sort(unique(public_data0$year),
                                                             decreasing = TRUE)[1:10], ]
  }

  if (is.na(grid_buffer)) {
    public_data0 <- public_data0[public_data0$station == station,]
  }

  public_data1 <- public_data0 # so we can calculate the total_weight_kg

  if (!is.na(species_codes[1])) {
    public_data0 <- public_data0[public_data0$species_code %in% species_codes,]
  }

  # if (survey == "EBS" | survey == "NBS") {
  #
  #   lat <- mean(unique(public_data0$latitude_dd_start[public_data0$station == station]), na.rm = TRUE)
  #   lon <- mean(unique(public_data0$longitude_dd_start[public_data0$station == station]), na.rm = TRUE)
  #
  #   possible_stations <-
  #   unique(public_data0$station[
  #     (public_data0$latitude_dd_start >= lat-deg_range &
  #        public_data0$latitude_dd_start <= lat+deg_range) &
  #       (public_data0$longitude_dd_start >= lon-deg_range &
  #          public_data0$longitude_dd_start <= lon+deg_range)])
  #
  # }

  if (nrow(public_data0) == 0) {
    out <- "Your quiery returned 0 results."
  } else {

    if (survey == "AI" | survey == "GOA") {

      y <- as.numeric(strsplit(x = station, split = "-", fixed = TRUE)[[1]])

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

      possible_stations$station <- paste(possible_stations$X1,
                                         possible_stations$X2,
                                         sep = "-")
      possible_stations <- possible_stations$station

      xx <- public_data0[public_data0$station %in% possible_stations,]
      public_data1 <- public_data1[public_data1$station %in% possible_stations,] # for calc total weight of haul

      catch <- stats::aggregate(xx[, c("count", "weight_kg", "cpue_kgkm2", "cpue_nokm2")],
                                by = list(
                                  haul = factor(xx$haul),
                                  year = factor(xx$year),
                                  scientific_name = factor(xx$scientific_name),
                                  common_name = factor(xx$common_name),
                                  station = factor(xx$station)),
                                sum)

      haul <- unique(xx[,c("year", "haul", "station", "stratum",
                           "vessel_name", "date_time", "latitude_dd_start", "longitude_dd_start",
                           "bottom_temperature_c", "surface_temperature_c", "depth_m",
                           "distance_fished_km", "net_width_m", "net_height_m", "area_swept_km2", "duration_hr")])

    } else if (survey == "EBS" | survey == "NBS") {
      catch <- public_data0[,c("year", "station", "scientific_name", "common_name",
                               "count", "weight_kg", "cpue_kgkm2", "cpue_nokm2")]
      haul <- unique(public_data0[,c("year", "haul", "station", "stratum",
                                     "vessel_name", "date_time", "latitude_dd_start", "longitude_dd_start",
                                     "bottom_temperature_c", "surface_temperature_c", "depth_m",
                                     "distance_fished_km", "net_width_m", "net_height_m",
                                     "area_swept_km2", "duration_hr")])
    }

    # add total weight to haul table
    haul <- base::merge(
      x = haul,
      y = stats::aggregate(public_data1[, c("weight_kg")],
                           by = list(
                             year = factor(public_data1$year),
                             station = factor(public_data1$station)),
                           sum, na.rm = TRUE),
      by = c("year", "station"))
    names(haul)[ncol(haul)] <- "total_weight_kg"
    haul[,ncol(haul)] <- round(x = haul[,ncol(haul)], digits = 2)


    catch$year <- as.numeric(as.character(catch$year))
    catch <- catch[order(-catch$year, -catch$weight_kg), ]
    rownames(catch)<-1:nrow(catch)
    catch$count[catch$count == 0] <- NA

    cc <- split(catch, catch$year)
    cc <- lapply(cc, function(df) { (df[order(-df$year, -df$weight_kg), names(catch) != c("year")]) })

    if (length(unique(catch$year))>1 | length(unique(catch$station))>1) {

      temp <- data.frame(table(catch[,c("scientific_name")]))
      if (sum(names(temp) == "Var1") == 1) {
        names(temp)[names(temp) == "Var1"] <- "scientific_name"
      }
      catch_means <- base::merge(
        x = stats::aggregate(catch[, c("count", "weight_kg", "cpue_kgkm2", "cpue_nokm2")],
                             by = list(
                               scientific_name = factor(catch$scientific_name),
                               common_name = factor(catch$common_name),
                               station = factor(catch$station)),
                             mean, na.rm = TRUE),
        y = temp,
        by = "scientific_name"#,
        # by.x = "scientific_name",
        # by.y = "Var1"
      )
      if (nrow(catch_means) == 0) {
        catch_means <- "There was no data available for these function parameters"
      } else {
        catch_means <- catch_means[order(-catch_means$cpue_kgkm2),]
        catch_means$count <- round(x = catch_means$count, digits = 1)
        catch_means$weight_kg <- round(x = catch_means$weight_kg, digits = 2)
        catch_means$cpue_kgkm2 <- round(x = catch_means$cpue_kgkm2, digits = 2)
        catch_means$cpue_nokm2 <- round(x = catch_means$cpue_nokm2, digits = 2)
        rownames(catch_means) <- 1:nrow(catch_means)

      }

    } else {
      catch_means <- "A summary of catch data would not be helpful with these function parameters"
    }

    catch$weight_kg <- round(x = catch$weight_kg, digits = 2)
    catch$cpue_kgkm2 <- round(x = catch$cpue_kgkm2, digits = 2)
    catch$cpue_nokm2 <- round(x = catch$cpue_nokm2, digits = 2)

    out <- list("catch" = cc,
                "catch_means" = catch_means,
                "haul" = haul)
  }

  return(out)
}


# Helper Functions ------------------------------------------------------------------------

#' Takes a string of words and combines them into a sentance that lists them.
#'
#' This function alows you to take a string of words and combine them into a sentance list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
#' @param x Character strings you want in your string.
#' @param oxford T/F: would you like to use an oxford comma? Default = TRUE
#' @param sep string. default = "," but ";" might be what you need!
#' @keywords strings
#' @export
#' @examples text_list(c(1,2,"hello",4,"world",6))
text_list<-function(x, oxford = TRUE, sep = ",") {
  x<-x[which(x!="")]
  # x<-x[which(!is.null(x))]
  x<-x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x)==2) {
    str1<-paste(x, collapse = " and ")
  } else if (length(x)>2) {
    str1<-paste(x[1:(length(x)-1)], collapse = paste0(sep, " "))
    str1<-paste0(str1,
                 ifelse(oxford == TRUE, sep, ""),
                 " and ", x[length(x)])
  } else {
    str1<-x
  }
  return(str1)
}

#' Make numbers the same length preceeded by 0s
#'
#' @param x a single or vector of values that need to be converted from something like 1 to "001"
#' @param number_places default = NA. If equal to NA, the function will take use the longest length of a value provided in x (example 1). If equal to a number, it will make sure that every number is the same length of number_places (example 2) or larger (if a value of x has more places than number_places(example 3)).
#'
#' @export
#' @return A string of the values in x preceeded by "0"s
#'
#' @examples
#' # example 1
#' numbers0(x = c(1,11,111))
#' # example 2
#' numbers0(x = c(1,11,111), number_places = 4)
#' # example 3
#' numbers0(x = c(1,11,111), number_places = 2)
numbers0 <- function (x, number_places = NA) {
  x<-as.numeric(x)
  xx <- rep_len(x = NA, length.out = length(x))
  if (is.na(number_places)){
    number_places <- max(nchar(x))
  }
  for (i in 1:length(x)) {
    xx[i] <- paste0(ifelse(number_places<nchar(x[i]),
                           "",
                           paste(rep_len(x = 0,
                                         length.out = number_places-nchar(x[i])),
                                 collapse = "")), as.character(x[i]))
  }
  return(xx)
}


#' Make sure file path is complete
#'
#' Function adds '/' or '\\' to the end of directories and recognizes when there are file extentions at the end of strings.
#'
#' @param path A string with the complete path of the directory or file.
#'
#' @export
#' @return A fixed path string.
#'
#' @examples
#' fix_path("sdfg/sdfg/sdfg/dfg.dd")
#' fix_path("sdfg/sdfg/sdfg")
#' fix_path("sdfg/sdfg/sdfg/")
fix_path <- function(path) {
  path0 <- ifelse(
    # Does the string end with a back slash?
    substr(x = path,
           start = nchar(path),
           stop = nchar(path)) %in% c("/", "\\") |
      # or if there is a file extention?
      grepl(pattern = "\\.",
            x = substr(x = path,
                       start = nchar(path)-7,
                       stop = nchar(path))),
    path,
    paste0(path, "/") )

  return(path0)
}

# Data ------------------------------------------------------------------------------------

#' @title PolySpecies Data Set
#' @description polynumbers
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


#' @title species data codes
#' @description RACEBASE Species Codes and Scientific Names Data Set
#' @usage data(species_data)
#' @author Sarah Friedman (sarah.friedman AT noaa.gov)
#' @format A data frame with 172 rows and 4 variables:
#' \describe{
#'   \item{\code{species_code}}{integer Species code}
#'   \item{\code{common_name}}{integer Poly species code}
#'   \item{\code{scientific_name}}{character Species scientific latin name}
#'   \item{\code{scientific_name_old}}{character Species scientific latin name used previously}
#'}
#' @details DETAILS
#' @keywords species scientific code data
#' @examples
#' data(species_data)
"species_data"



