# #' ---
# #' title: GAPsurvey
# #' purpose: assist scientists on the survey collect data
# #' author: Jason Conner (jason.conner AT noaa.gov)
# #' start date:
# #' modified by: Emily Markowitz (emily.markowitz AT noaa.gov)
# #' modified date: March 2021
# #' ---


# R-create BTH and BTD files from other sources ----------------------------------------------



#' BVDR Conversion to Create BTD data
#'
#' Converts Marport BVDR data (TED) to RACE BTD format.
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = NA. The model number of the Marport reciever (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the Marport reciever (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the Marport reciever (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param path_in Optional. The default is the localion on the catch computer ("C:/Program Files/Marport Server/Logs/") but any path can be entered.
#' @param path_out Optional. The default is the local working directory but can be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#' @param quiet Optional logical TRUE/FALSE. Default = FALSE. If FALSE, will print a statement or a pop-up window will let the user know where the file has been saved to.
#'
#' @return A .BTH and .BTD file to the path_out directory.
#' @export
#'
#' @examples
#' path_in0 <- system.file("exdata", package = "GAPsurvey")
#' TEDtoBTD(
#'    VESSEL = 94,
#'    CRUISE = 201901,
#'    HAUL = 3,
#'    MODEL_NUMBER = 123,
#'    VERSION_NUMBER = 123,
#'    SERIAL_NUMBER = 123,
#'    path_in = paste0(path_in0, "/bvdr2btd/"),
#'    filename_add = "new", # is this important?
#'    quiet = TRUE)
TEDtoBTD <- function(
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
  MODEL_NUMBER = NA,
  VERSION_NUMBER = NA,
  SERIAL_NUMBER = NA,
  path_in = "C:/Program Files/Marport Server/Logs/",
  path_out = "./",
  filename_add = "new",
  quiet = FALSE){

  if (is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if (is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if (is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if (is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number:  ") }
  if (is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number:  ") }
  if (is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of Marport height sensor:  ") }

  HAUL <- as.numeric(HAUL)
  shaul <- numbers0(x = HAUL, number_places = 4)
  # if(HAUL<10) shaul=paste("000",HAUL,sep="")
  # if(HAUL>9 & HAUL<100) shaul=paste("00",HAUL,sep="")
  # if(HAUL>99) shaul=paste("0",HAUL,sep="")

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
  merged=merge(ted.file,tet.file,all=T)
  # str(merged)
  # head(merged)

  #which(ted.file[,4] %in% tet.file[,4])

  xx=merged$date
  DATE_TIME=format(xx, format = "%m/%d/%Y %H:%M:%S")
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

  new.BTD=cbind(VESSEL,CRUISE,HAUL,SERIAL_NUMBER,DATE_TIME,TEMPERATURE,DEPTH)
  new.BTH=cbind(VESSEL,CRUISE,HAUL,MODEL_NUMBER,VERSION_NUMBER,SERIAL_NUMBER,
                HOST_TIME,LOGGER_TIME,LOGGING_START,LOGGING_END,SAMPLE_PERIOD,NUMBER_CHANNELS,
                NUMBER_SAMPLES,MODE)
  new.BTD[which(is.na(new.BTD))]=""
  new.BTD=as.data.frame(new.BTD)
  new.BTD=new.BTD[new.BTD$DEPTH!="2000",]

  #head(new.BTD)
  #return(head(new.BTD))
  utils::write.csv(new.BTD,
            paste0(path_out, "HAUL",shaul,
                   ifelse(is.na(filename_add) | filename_add == "",
                          "", paste0("_", filename_add)),
                   ".BTD"),
            quote=F,row.names=F,eol=",\n")
  utils::write.csv(new.BTH,
            paste0(path_out, "HAUL",shaul,
                   ifelse(is.na(filename_add) | filename_add == "",
                          "", paste0("_", filename_add)),
                   ".BTH"),
            quote=F,row.names=F)

  if(!quiet){
    tcltk::tkmessageBox(title = "Message",
                 message = paste0("Your new .BTD and .BTH files are saved to the folder ", path_out),
                 icon = "info", type = "ok")
  }
}



#' Convert CTD data in XML form to BTD and BTH
#'
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param DATE Optional. Default = NA. The date in MM/DD/YYYY format (e.g., "06/02/2019"). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = NA. The model number of the CTD (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the CTD (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the CTD (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param path_in Optional. Default = "./., or the local working directory but any path (as a string) may be entered.
#' @param path_out Optional. The default is the local working directory but may be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#' @param quiet Optional logical TRUE/FALSE. Default = FALSE. If FALSE, will print a statement or a pop-up window will let the user know where the file has been saved to.
#'
#' @return A .BTH and .BTD file to the path_out directory.
#' @export
#'
#' @examples
#' path_in0 <- system.file("exdata", package = "GAPsurvey")
#' # CTDXMLtoBTD()
CTDXMLtoBTD <- function(
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
  DATE = NA,
  MODEL_NUMBER = NA,
  VERSION_NUMBER = NA,
  SERIAL_NUMBER = NA,
  path_in = "./",
  path_out = "./",
  filename_add = "",
  quiet = FALSE){

  if (is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if (is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if (is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if (is.na(DATE)){ DATE <- readline("Type date of haul (MM/DD/YYYY):  ") }
  if (is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number:  ") }
  if (is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number:  ") }
  if (is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of CTD:  ") }

  #   HAUL <- as.numeric(HAUL)
  #   shaul <- numbers0(x = HAUL, number_places = 4)
  #
  #   if (is.na(path_in)){
  #     file.name <- file.choose()
  #   } else {
  #     # check if path exists #TOLEDO
  #     file.name <- path_in
  #   }
  #
  #
  #   library(tibble)
  #   library(dplyr)
  #   library(readr)
  #   library(XML)
  #   library(xml2)
  #
  #   # read.ctd(file = path_in, type = "SBE19", )
  #   # a <- utils::read.csv(file = path_in)
  #   a <- read_xml(path_in)
  #   doc <- XML::xmlParse(file = path_in)
  #
  #   df.list<-list()
  #
  #   # top <- regex()
  #   #https://megapteraphile.wordpress.com/2020/03/29/converting-xml-to-tibble-in-r/
  #   topics <- c("Calibration", "CalibrationCoefficients", "ConfigurationData",
  #               "Headers", "EventCounters", "Data", "SBEDataUpload",
  #               "InstrumentRawData", "InstrumentState", "InternalSensors",
  #               "SBEDataUpload", "SBEDataUploadFile", "SBEDataUpload")
  #   for (i in 1:length(topics)){
  #     df <- xmlToDataFrame(nodes =
  #                            getNodeSet(doc,
  #                                       paste0("//", topics[i])))
  #     df.list <- c(df.list, list(df))
  #     names(df.list)[i] <- topics[i]
  #   }
  #
  #
  #   tb <- as_tibble(df.list$ConfigurationData) %>%
  #     transmute(
  #       AutoRun = AutoRun,
  #       ScansToAverage = parse_number(ScansToAverage),
  #       MinimumCondFreq = MinimumCondFreq,
  #       PumpDelay = as.numeric(PumpDelay)
  #     )
  #
  # df <- xmlToDataFrame(nodes = getNodeSet(doc, "//SBEDataUploadFile"))
  #   # df <- xmlToDataFrame(nodes = getNodeSet(doc, "//Sensor"))
  #   # df <- xmlToDataFrame(nodes = getNodeSet(doc, "//InternalSensors"))
  #
  #   read.ctd(file = path_in, type = "SBE19" )
  #

}






#' Convert raw CTD files to BTD/BTH format
#'
#' To RUN a function, put curser on line you want to run and press CTRL+R
#'
#' When first open "Survey R Functions" the function library("tcltk")
#' MUST be run ONCE
#' As long as R console remains open, the function library("tcltk")
#' does not need to be run a second time
#'
#' library("tcltk")
#'
#' The "CTDtoBTD()" function converts an exported CSV file from the Teledyne CTD Program into BTD and BTH files
#' File format for the CSV file must be DATE,TIME,TEMP,DEPTH (mm-dd-yyyy,hh:mm:ss.ss,(C),(m))
#' Program prompts you for: Vessel code, Cruise number, Haul number, CTD model number,
#' Version number (=999), CTD serial number, and then opens window for you  to select file from any directory.
#' The BTH and BTD files are written to the C: \ NM directory
#'
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param DATE Optional. Default = NA. The date in MM/DD/YYYY format (e.g., "06/02/2019"). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = NA. The model number of the CTD (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the CTD (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the CTD (I think) (e.g., 123) If NA or not called in the function, a prompt will appear asking for this data. # TOLEDO
#' @param path_in Optional. Default = "./., or the local working directory but any path (as a string) may be entered.
#' @param path_out Optional. The default is the local working directory but may be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#' @param quiet Optional logical TRUE/FALSE. Default = FALSE. If FALSE, will print a statement or a pop-up window will let the user know where the file has been saved to.
#'
#' @return A .BTH and .BTD file to the path_out directory.
#'
#' @importFrom magrittr %>%
#' @return
#' @export
#'
#' @examples
#' path_in0 <- system.file("exdata", package = "GAPsurvey")
#' # CTDtoBTD(
#' #     VESSEL = 94,
#' #     CRUISE = 201901,
#' #     HAUL = 3,
#' #     DATE = "06/02/2019",
#' #     MODEL_NUMBER = 123,
#' #     VERSION_NUMBER = 123,
#' #     SERIAL_NUMBER = 123,
#' #     path_in = paste0(path_in0, "ctd2btd/6-02-19/DATA01.ctd"),
#' #     path_out = path_out0,
#' #     filename_add = "", # is this important?
#' #     quiet = TRUE)
CTDtoBTD <- function(
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
  DATE = NA,
  MODEL_NUMBER = NA,
  VERSION_NUMBER = NA,
  SERIAL_NUMBER = NA,
  path_in = NA,
  path_out = "./",
  filename_add = "",
  quiet = FALSE){

  if (is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if (is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if (is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if (is.na(DATE)){ DATE <- readline("Type date of haul (MM/DD/YYYY):  ") }
  if (is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number:  ") }
  if (is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number:  ") }
  if (is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of CTD:  ") }

  HAUL <- as.numeric(HAUL)
  shaul <- numbers0(x = HAUL, number_places = 4)

  if (is.na(path_in)){
    file.name <- file.choose()
  } else {
    # check if path exists #TOLEDO
    file.name <- path_in
  }

  # ctd.names <- read.table(file = file.name, nrows = 2, header = FALSE)
  ctd.names <- readr::read_csv(file.name, n_max = 0) %>% names()

  # ctd.file <- utils::read.csv(file = file.name, col.names = ctd.names, skip = 2)
  # ctd.file <- ctd.file[,c(DATE, TIME, TEMP, DEPTH)]
  ctd.file <- readr::read_csv(file.name, col_names = ctd.names, skip = 2) %>%
    dplyr::select(DATE, TIME, TEMP, DEPTH)
  # ctd.file <- read.ctd(file = file.name)

  ctd.file <- ctd.file %>%
    dplyr::mutate(DATE = lubridate::mdy(DATE)) %>%
    dplyr::mutate(DATE_TIME = lubridate::ymd_hms(paste(DATE, TIME))) %>%
    dplyr::mutate(AKDT = lubridate::with_tz(DATE_TIME,tzone="Etc/GMT+8")) %>%
    dplyr::mutate(AKDT = gsub("-","/",AKDT))

  date_f <- format(as.Date(ctd.file$AKDT, '%Y/%m/%d'), "%m/%d/%Y")
  time_f <- stringr::str_sub(ctd.file$AKDT, start = 12)
  dt_f <- paste(date_f,time_f)

  ctd.file <- ctd.file %>%
    dplyr::mutate(AKDT_formatted = dt_f)

  DATE_TIME <- ctd.file$AKDT_formatted

  HOST_TIME=max(DATE_TIME)
  LOGGER_TIME=max(DATE_TIME)
  LOGGING_START=min(DATE_TIME)
  LOGGING_END=max(DATE_TIME)
  TEMPERATURE=round(ctd.file$TEMP,4)
  DEPTH=round(ctd.file$DEPTH,4)
  SAMPLE_PERIOD=3
  NUMBER_CHANNELS=2
  NUMBER_SAMPLES=0
  MODE=2

  new.BTD=as.data.frame(cbind(VESSEL,CRUISE,HAUL,SERIAL_NUMBER,DATE_TIME,TEMPERATURE,DEPTH))
  new.BTH=cbind(VESSEL,CRUISE,HAUL,MODEL_NUMBER,VERSION_NUMBER,SERIAL_NUMBER,
                HOST_TIME,LOGGER_TIME,LOGGING_START,LOGGING_END,SAMPLE_PERIOD,NUMBER_CHANNELS,
                NUMBER_SAMPLES,MODE)

  #haa=head(new.BTD,100)
  idx=which(duplicated(new.BTD$DATE_TIME)==FALSE)
  new.BTD=new.BTD[idx,]

  HAUL=as.numeric(HAUL)
  #head(new.BTD)
  #return(head(new.BTD))
  if(HAUL<10){
    pre.haul="HAUL000"}
  if(HAUL>9 & HAUL<100){
    pre.haul="HAUL00"}
  if(HAUL>99){
    pre.haul="HAUL0"}

  btd.name=paste(pre.haul,HAUL,"_new.BTD",sep="")
  bth.name=paste(pre.haul,HAUL,"_new.BTH",sep="")
  utils::write.csv(new.BTD,btd.name,quote=F,row.names=F)
  utils::write.csv(new.BTH,bth.name,quote=F,row.names=F)


  if(!quiet){
    tcltk::tkmessageBox(title = "Message",
                 message = paste0("Your new files are saved to the folder ", path_out),
                 icon = "info", type = "ok")
  }
}



#' Recover position data from Globe.log file
#'
#' Lorem
#'
#' In the event that the MARPORT server GPS fails or is incomplete, "LOGtoGPS()" converts GLOBE LOG files into a format that can be uploaded into WHEELHOUSE
#'
#' To RUN the function, put curser on the "LOGtoGPS()" line below & press CTRL+R.
#'
#' 1) Go the C:\ globe\ logs\ 2018\ directory and choose GLG file with proper date
#' 2) Use GLOBE Files>Logs> to convert GLG (binary) to a LOG (csv)file
#' 3) LOGtoGPS()will prompt you for Vessel code, Cruise no., Haul no. and Date
#' 4) The final prompt will ask for the location of the GLOBE LOG file
#' 5) LOGtoGPS()will create csv file in the R directory with filename "new.gps"
#' 6) Rename "new.gps" to HAULXXXX.GPS where XXXX is the haul number
#' 7) Upload HAULXXXX.GPS into WHEELHOUSE
#' 8) NOTE: The raw GLOBE log data are in GMT time (-8 hrs or 4PM AKDT prior day to 4PM current day. Hence if haul with missing GPS spans the 4PM hour (e.g.,3:45-4:30 PM),YOU WILL HAVE TO CONVERT TWO GLG files (current day and next day)and run LOGtoGPS()twice & manually combine the two GPS files
#' 9) ALSO NOTE: You may have to shut down GLOBE or wait until after 4pm on following day before all the incoming NMEA data are written to the GLG file.
#'
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param DATE Optional. Default = NA. The date in MM/DD/YYYY format (e.g., "06/02/2019"). If NA or not called in the function, a prompt will appear asking for this data.
#' @param path_in Optional. Default = "./., or the local working directory but any path (as a string) may be entered.
#' @param path_out Optional. The default is the local working directory but may be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#' @param quiet Optional logical TRUE/FALSE. Default = FALSE. If FALSE, will print a statement or a pop-up window will let the user know where the file has been saved to.
#'
#' @return A .BTH and .BTD file to the path_out directory.
#' @export
#'
#' @examples
#' path_in0 <- system.file("exdata", package = "GAPsurvey")
#' LOGtoGPS(
#'     VESSEL = 94,
#'     CRUISE = 201901,
#'     HAUL = 3,
#'     DATE = "06/06/2017",
#'     path_in = paste0(path_in0, "/log2gps/06062017.log"),
#'     filename_add = "",
#'     quiet = TRUE)
LOGtoGPS <- function(
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
  DATE = NA,
  path_in = NA,
  path_out = "./",
  filename_add = "",
  quiet = FALSE){

  if (is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if (is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if (is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if (is.na(DATE)){ DATE <- readline("Type date of haul (MM/DD/YYYY):  ") }

  HAUL <- as.numeric(HAUL)
  shaul <- numbers0(x = HAUL, number_places = 4)

  if (is.na(path_in)) {
    tcltk::tkmessageBox(title = "Message",
                        message = "In next window open the globe .log file", icon = "info", type = "ok")
    file.name <- tcltk::tclvalue(tcltk::tkgetOpenFile())
  } else {
    file.name <- path_in
  }

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
  # DATE_TIME[1:6]

  lat1=as.numeric(as.character(infoselect$"LAT1"[1:6]))
  LAT=ifelse(infoselect$"LAT2"=="N",lat1,-lat1)
  # LAT[1:6]

  long1=as.numeric(as.character(infoselect$"LONG1"[1:6]))
  LONG=ifelse(infoselect$"LONG2"=="E",long1,-long1)
  # LONG[1:6]

  df=cbind(VESSEL, CRUISE, HAUL, DATE_TIME, LAT, LONG)
  # head(df)
  utils::write.table(df,paste0(path_out,
                        "HAUL", shaul, ".gps"),
              quote=F,sep = ",",row.names=F,col.names=F)

  if (!quiet) {
    tcltk::tkmessageBox(title = "Message",
                 message = paste0("Your new files are saved to the folder ", path_out),
                 icon = "info", type = "ok")
  }

}


# Work with catch data ----------------------------------------------


#' Import Length Tablet Files Into DataEnt.mdb
#'
#' NOTE: Must be run on 32-bit R!
#'
#' Looks for tablet length files in given DSN and uploads them to LENGTH and RAW_LENGTH in the given data_ent.mdb.
#'
#'
#' @param haul Number of the haul you want to upload
#' @param dsnTablet String for the drive folder (in R notation) where tablet files exist.
#' @param dsnDataEnt String for the drive folder (in R notation) - include full file name and extension.
#'
#' @return ...
#' @export
lengthData <- function(haul, dsnTablet, dsnDataEnt) {

  PolySpecies <- utils::data("PolySpecies")

  options(stringsAsFactors = F)

  # Check to see if length files exist in data_ent.mdb for this haul
  entRawLength <- selectDataEnt(dsnDataEnt, paste0("select * from RAW_LENGTH where HAUL = ",haul))
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
  entRawLength <- cbind(tabRawLength,INDEX = seq(from=i_rawLength, length.out=nrow(tabRawLength)))

  # Calculate length frequencies
  tabRawLength$FREQUENCY <- 1
  entLengthFreq <- stats::aggregate.data.frame(tabRawLength$FREQUENCY,by=tabRawLength[c("HAUL","POLY_SPECIES_CODE","SEX","LENGTH")], FUN = sum)
  colnames(entLengthFreq)[5] <- "FREQUENCY"

  entLengthFreq <- merge(entLengthFreq, PolySpecies, by = "POLY_SPECIES_CODE")[,c("HAUL","SPECIES_CODE","SEX","LENGTH","FREQUENCY")]
  entLengthFreq$SUBSAMPLE_TYPE <- 1L
  entLengthFreq$LENGTH_TYPE <- NA

  # Upload length data
  writeDataEnt(dsnDataEnt,entRawLength,"RAW_LENGTH")
  writeDataEnt(dsnDataEnt,entLengthFreq,"LENGTH")

  # Verify lengths were uploaded
  checkRawLength <- selectDataEnt(dsnDataEnt, paste0("select * from RAW_LENGTH where HAUL = ",haul))
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
#' @param haul Number of the haul you want to upload
#' @param dsnTablet String for the drive folder (in R notation) where tablet files exist.
#' @param dsnDataEnt String for the drive folder (in R notation) - include full file name and extension.
#'
#' @return ...
#' @export
#'
#' @examples
#' # specimenData()
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
#' @param haul haul number
#' @param dsnTablet full file path of the location of raw tablet data
#' @param dsnDataEnt full file path of the location of the dataent.mdb - include full file name and extension.
#'
#' @return ...
#' @export
#'
#' @examples
#' # benthicData()
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
#' NOTE: Must be run on 32-bit R!
#'
#' Processes catch data exported from catch tablets into summarized values into the designated dataent.mdb. You must have exported from the tablet:
#' DATAENT_CATCH... and RAW_CATCH_HAUL... for the designated haul.
#' @param haul haul number
#' @param dsnTablet full file path of the location of raw tablet data
#' @param dsnDataEnt full file path and filename of the location of the dataent.mdb - include full file name and extension.
#' @param importLength logical - import length data (GAPsurvey::lengthData()) prior to importing catch data
#'
#' @return ...
#' @export
#'
#' @examples
#' # catchData()
catchData <- function(haul, dsnTablet, dsnDataEnt, importLength=TRUE) {
  options(stringsAsFactors = F)

  # Grab data_ent formatted tablet data
  catchFile <- list.files(path=dsnTablet, pattern=paste0("DATAENT_CATCH_",sprintf("%04d", haul)))

  if (length(catchFile) != 1) {
    stop("C1: Data Ent Catch file missing or duplicate")
  } else {
    tabCatch <- utils::read.csv(file.path(dsnTablet,catchFile))
  }
  tabCatch$seq <- seq(1:nrow(tabCatch))


  if (!is.character(tabCatch$SAMPLED_ALL)) {
    stop("C3: SAMPLED_ALL flag has not been set for any species in this haul, correct and try again.")
  }

  if (anyNA(tabCatch$SAMPLED_ALL)) {
    stop("C3b: SAMPLED_ALL flag has not been set for one or more species in this haul, correct on tablet and try again.")
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
  catchHeader <- list.files(path=dsnTablet, pattern=paste0("RAW_CATCH_HAUL_",sprintf("%04d", haul)))
  if (length(catchHeader) != 1) {
    stop("C4: Raw Catch Haul file missing or duplicated")
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
#' @param data TOLEDO
#' @param tablename String of the data_ent table you are deleting data from
#'
#' @return ...
#' @export
#'
#' @examples
#' # writeDataEnt()
writeDataEnt <- function(dsnDataEnt, data, tablename) {
  odbcStr <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",dsnDataEnt)
  dataEnt <- RODBC::odbcDriverConnect(odbcStr)

  sqlReturn <- RODBC::sqlSave(dataEnt, data, tablename, append=T, rownames=F)

  # FUTURE: SQLupdate version
  #sqlReturn <- RODBC::sqlUpdate(dataEnt, data, tablename, index=index)

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
#' @return ...
#' @export
#'
#' @examples
#' # deleteDataEnt(dsnDataEnt,haul,"BENTHIC_BAG")
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
#' @param query An SQL quiery # TOLEDO
#'
#' @return ...
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


# Helper Functions ------------------------------------------------------------------------



#' Make numbers the same length preceeded by 0s
#'
#' @param x a single or vector of values that need to be converted from something like 1 to "001"
#' @param number_places default = NA. If equal to NA, the function will take use the longest length of a value provided in x (example 1). If equal to a number, it will make sure that every number is the same length of number_places (example 2) or larger (if a value of x has more places than number_places(example 3)).
#'
#' @return A string of the values in x preceeded by "0"s
#' @export
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

# Data ------------------------------------------------------------------------------------


#' PolySpecies Data Set
#'
#' @docType data
#'
#' @usage data(PolySpecies)
#' @author Jason Conner (jason.conner AT noaa.gov)
#'
#' @format A data frame with 172 obs. of  4 variables:
#' \describe{
#'   \item{SPECIES_CODE}{Species code}
#'   \item{POLY_SPECIES_CODE}{Poly species code}
#'   \item{SPECIES_NAME}{Species scientific latin name}
#'   \item{COMMON_NAME}{Species common names)}
#' }
#'
#' @keywords catch data
#'
#' @examples
#' data(PolySpecies)
"PolySpecies"



