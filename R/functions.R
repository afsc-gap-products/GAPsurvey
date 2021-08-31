# #' ---
# #' title: GAPsurvey
# #' purpose: assist scientists on the survey collect data
# #' author: Jason Conner (jason.conner AT noaa.gov), Emily Markowitz (emily.markowitz AT noaa.gov), and Liz Dawson (Liz.Dawson AT noaa.gov)
# #' modified by: Emily Markowitz (emily.markowitz AT noaa.gov) and Liz Dawson (Liz.Dawson AT noaa.gov)
# #' start date: 2018?
# #' modified date: June 2021
# #' ---


# R-create BTH and BTD files from other sources ----------------------------------------------


#' BVDR Conversion to Create BTD data
#'
#' Converts Marport BVDR data (.ted and .tet files from Marport headrope sensor) to .BTD format.  You must first run the BVDR converter program (convert_bvdr.exe) to convert the Marport .bvdr files into .ted and .tet files that can be pulled into R. The BVDR program and instructions can be found in the RACE Survey App.  You will have to create your own .SGT file using the example in the BVDR instruction file with start and end time (be sure to include a carriage return after your (second and) final row of data!), because this is not a file that our current systems creates.  Once you have used the BVDR converter to output the .ted and .tet files you are ready to use the TEDtoBTD() function here!
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 162 for AK Knight, 94 for Vesteraalen). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year + sequential two digit cruise (e.g., 202101). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number that you are trying to convert data for (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = NA. The model number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual version number without any negative repercussions).
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual serial number without any negative repercussions).
#' @param path_in Optional. The default is the location on the catch computer ("C:/Program Files/Marport Server/Logs/") but any path can be entered.
#' @param path_out Optional. The default is the local working directory but can be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#' @param quiet Optional logical TRUE/FALSE. Default = FALSE. If FALSE, will print a statement or a pop-up window will let the user know where the file has been saved to.
#'
#' @return .BTH and .BTD files to the path_out directory.
#' @export
#'
#' @examples
#' TEDtoBTD(
#'    VESSEL = 94,
#'    CRUISE = 201901,
#'    HAUL = 3,
#'    MODEL_NUMBER = 123,
#'    VERSION_NUMBER = 456,
#'    SERIAL_NUMBER = 789,
#'    path_in = system.file("exdata/bvdr2btd/", package = "GAPsurvey"),
#'    path_out = getwd(),
#'    filename_add = "newted", 
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

  if(!quiet){
    tcltk::tkmessageBox(title = "Message",
                        message = paste0("Your new ", filename, 
                                         " .BTD and .BTH files are saved."),
                 icon = "info", type = "ok")
  }
}



#' Convert CTD data in .cnv form to BTD and BTH
#' 
#' Before running this CTDtoBTD function, you will need to use the CTD laptop to convert the raw CTD data to a .cnv file.  
#' 
#' To do this, 
#' 1. use the SBE Data Processing Program, and in the "Run" menu select "1. Data conversion".  
#' 2. Under "Program setup file" select the .psa file (should be DataCnv.psa located in CTD folder), under "Instrument configuration file" select the .xmlcon file for the CTD located in the 
#' deployment_xmlcon folder- this is CTD specific, know your CTD's serial number, and under "Input directory" select the .hex file (from the CTD computer) that is specific to the haul that you are missing data.  Click "Start Process".
#' The conversion program will ask you for outputs. At the prompt, Select and Add "Time, Elapsed, seconds", "Depth, salt water, m", "Temperature, ITS-90, deg C", "Pressure, Strain Gauge, db", and "Conductivity, S/m".
#' 3. This .cnv file must include columns for "Time, Elapsed, seconds", "Depth , salt water, m", "Temperature, ITS-90, deg C", "Pressure, Strain Gauge, db", and "Conductivity, S/m". 
#' 4. Press "Start Process" button again and a .cnv file should appear in your selected output directory.  The .cnv file can then be used for this CTDtoBTD() function!   
#' 5. Look in your output directory (usually the "Documents" folder, but you can find it using getwd()) for your new .BTD and .BTH files. 
#' 
#' Note that if there are multiple observations from the CTD per second, that they will be averaged by second (e.g., any observations from seconds 0 to >1.0 will be averaged together). 
#'
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = NA. The model number of the CTD (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the CTD (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the CTD (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param path_in Optional. Default = "./., or the local working directory but any path (as a string) may be entered.
#' @param path_out Optional. The default is the local working directory but may be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#' @param quiet Optional logical TRUE/FALSE. Default = FALSE. If FALSE, will print a statement or a pop-up window will let the user know where the file has been saved to.
#'
#' @return .BTH and .BTD files to the path_out directory.
#' @export
#'
#' @examples
#' CTDtoBTD(
#'    VESSEL = 94,
#'    CRUISE = 2001,
#'    HAUL = 4,
#'    MODEL_NUMBER = 123,
#'    VERSION_NUMBER = 456,
#'    SERIAL_NUMBER = 789,
#'    path_in = system.file(paste0("exdata/ctd2btd/",
#'       "SBE19plus_01908103_2021_06_01_94_0004_raw.cnv"), 
#'        package = "GAPsurvey"),
#'    path_out = getwd(),
#'    filename_add = "newctd", 
#'    quiet = TRUE)
#'   
#'  
#' CTDtoBTD(
#'    VESSEL = 94,
#'    CRUISE = 202101,
#'    HAUL = 107,
#'    MODEL_NUMBER = "",
#'    VERSION_NUMBER = "",
#'    SERIAL_NUMBER = 8105,
#'    path_in = system.file(paste0("exdata/ctd2btd/",
#'      "SBE19plus_01908105_2021_06_25_94_0005.cnv"), 
#'       package = "GAPsurvey"),
#'    path_out = getwd(),
#'    filename_add = "newctd", 
#'    quiet = TRUE)
CTDtoBTD <- function(
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
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
  if (is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number:  ") }
  if (is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number:  ") }
  if (is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of CTD:  ") }
  
  if (is.na(path_in)) {
    tcltk::tkmessageBox(title = "Message",
                        message = "In next window open the CTD .cnv file", 
                        icon = "info", type = "ok")
    file.name <- tcltk::tclvalue(tcltk::tkgetOpenFile())
  } else {
    path_in <- fix_path(path_in)
    file.name <- path_in
  }

  # make sure path_in comes in with correct 
  options(stringsAsFactors = FALSE) # die, factors, die!
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
  
  if(!quiet){
    tcltk::tkmessageBox(title = "Message",
                        message = paste0("Your new ", filename, 
                                         " .BTD and .BTH files are saved."),
                        icon = "info", type = "ok")
  }
  
}



#' Recover position data from Globe .log file
#'
#' In the event that the MARPORT server GPS fails or is incomplete, "LOGtoGPS()" converts GLOBE LOG files into a format that can be uploaded into WHEELHOUSE.
#' To get a .log file that is usable in this function,
#' 1) Go the C:\ globe\ logs\ 2018\ directory and choose GLG file with proper date
#' 2) Use GLOBE Files>Logs> to convert .GLG (binary) to a .LOG (.csv) file
#' 3) LOGtoGPS()will prompt you for Vessel code, Cruise no., Haul no. and Date
#' 4) The final prompt will ask for the location of the GLOBE LOG file
#' 5) LOGtoGPS()will create csv file in the R directory with filename "new.gps"
#' 6) Rename "new.gps" to HAULXXXX.GPS where XXXX is the haul number
#' 7) Upload HAULXXXX.GPS into WHEELHOUSE
#' 8) NOTE: The raw GLOBE log data are in GMT time (-8 hrs or 4PM AKDT prior day to 4PM current day. Hence if haul with missing GPS spans the 4PM hour (e.g.,3:45-4:30 PM),YOU WILL HAVE TO CONVERT TWO GLG files (current day and next day)and run LOGtoGPS()twice & manually combine the two GPS files
#' 9) ALSO NOTE: You may have to shut down GLOBE or wait until after 4pm on following day before all the incoming NMEA data are written to the GLG file.
#'
#' Now that you have a .log file, you can RUN the function by putting your cursor on the "LOGtoGPS()" line below & press CTRL+R.
#'
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param DATE Optional. Default = NA. The date in MM/DD/YYYY format (e.g., "06/02/2019"). If NA or not called in the function, a prompt will appear asking for this data.
#' @param path_in Optional. Default = "./., or the local working directory but any path (as a string) may be entered.
#' @param path_out Optional. The default is the local working directory but may be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputted file. Here, you can additional information that may make this file helpful to find later.
#' @param quiet Optional logical TRUE/FALSE. Default = FALSE. If FALSE, will print a statement or a pop-up window will let the user know where the file has been saved to.
#'
#' @return A .GPS file to the path_out directory.
#' @export
#'
#' @examples
#' LOGtoGPS(
#'     VESSEL = 94,
#'     CRUISE = 201901,
#'     HAUL = 3,
#'     DATE = "06/06/2017",
#'     path_in = system.file("exdata/log2gps/06062017.log", 
#'         package = "GAPsurvey"),
#'     path_out = getwd(),
#'     filename_add = "newlog",
#'     quiet = TRUE)
#'     
#'     LOGtoGPS(
#'     VESSEL = 94,
#'     CRUISE = 202101,
#'     HAUL = 37,
#'     DATE = "06/07/2021",
#'     path_in = system.file("exdata/log2gps/Haul0037.log", 
#'         package = "GAPsurvey"),
#'     path_out = getwd(),
#'     filename_add = "newlog",
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

  if (is.na(path_in)) {
    tcltk::tkmessageBox(title = "Message",
                        message = paste0("In next window, open the file named ", gsub(pattern = "/", replacement = "", x = DATE), ".log."), 
                        icon = "info", type = "ok")
    file.name <- tcltk::tclvalue(tcltk::tkgetOpenFile())
  } else {
    path_in <- fix_path(path_in)
    file.name <- path_in
  }
  
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

  if (!quiet) {
    tcltk::tkmessageBox(title = "Message",
                 message = paste0("Your new .gps files are saved to ", filename),
                 icon = "info", type = "ok")
  }

}


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

  PolySpecies <- GAPsurvey::PolySpecies

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

  entLengthFreq <- base::merge(entLengthFreq, PolySpecies, 
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
#'                           tablename = 'MIXTURE_HEADER')
#' dCatch <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                         tablename = 'CATCH')
#' dCatchHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                             tablename = 'CATCH_HEADER')
#' dLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                          tablename = 'LENGTH')
#' dRawLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = 
#'                               haul, tablename = 'RAW_LENGTH')
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
#'                        tablename = 'MIXTURE')
#' dMixHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                           tablename = 'MIXTURE_HEADER')
#' dCatch <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                         tablename = 'CATCH')
#' dCatchHead <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                             tablename = 'CATCH_HEADER')
#' dLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = haul, 
#'                          tablename = 'LENGTH')
#' dRawLength <- deleteDataEnt(dsnDataEnt = dsnDataEnt, haul = 
#'                               haul, tablename = 'RAW_LENGTH')
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
#' @param query A SQL query
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


#' Make sure file path is complete
#'
#' Function adds '/' or '\\' to the end of directories and recognizes when there are file extentions at the end of strings.
#'
#' @param path A string with the complete path of the directory or file. 
#'
#' @return A fixed path string. 
#' @export
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



format_date <- function(x, ...) {
  tmp <- format(x, ...)
  tmp <- sub("^[0]+", "", tmp)
  tmp <- sub('/0', "/", tmp)
  return(tmp)
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
