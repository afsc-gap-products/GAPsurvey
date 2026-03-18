#' Extract depth and temperature from NMEA strings
#'
#' Convert Marport Trawl Explorer NMEA strings to BTD and BTH files. Called internally by convert_bvdr_marp().
#'
#' @param nmea_strings Character vector of NMEA strings (e.g., from a .bvdr file) or a path to a .marp file.
#' @param interactive_editing Should the interactive point removal interface be used to manually clean temperature and depth data? If TRUE, must have graphic devices set to view plots in actual size (in R Studio: View > Actual Size or Ctrl+0)
#' @param min_depth Optional (default = -0.1). Minimum valid depth (m).
#' @param max_depth Optional (default = 1000). Maximum valid depth (m).
#' @param min_temperature Optional (default = -2). Maximum valid temperature (Celsius).
#' @param max_temperature Optional (default = 20). Maximum valid temperature (Celsius).
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 162 for AK Knight, 94 for Vesteraalen). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year + sequential two digit cruise (e.g., 202101). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number that you are trying to convert data for (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = "Marport TE". The model name/number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions). This field may have restrictions on length.
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual version number without any negative repercussions).
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual serial number without any negative repercussions).
#' @param ... additional arguments
#' @export
#' @examples \dontrun{
#' # Run this to select Marport (.marp files)
#' convert_nmea_btd()
#' }
#' @importFrom stats complete.cases
#' @import graphics
#' @author Sean Rohan <sean.rohan@@noaa.gov>


convert_nmea_btd <- function(nmea_strings = NULL, interactive_editing = TRUE, min_depth = -0.1, max_depth = 800, min_temperature = -2, max_temperature = 20, VESSEL = NA, CRUISE = NA, HAUL = NA, MODEL_NUMBER = "Marport TE", VERSION_NUMBER = NA, SERIAL_NUMBER = NA, ...) {

  nmea_strings = NULL
  filter_type = "none"
  interactive_editing = TRUE
  min_depth = -0.1
  max_depth = 800
  min_temperature = -2
  max_temperature = 20
  VESSEL = NA
  CRUISE = NA
  HAUL = NA
  MODEL_NUMBER = "Marport TE"
  VERSION_NUMBER = NA
  SERIAL_NUMBER = NA

  format_date <- function(x, ...) {
    tmp <- format(x, ...)
    tmp <- sub("^[0]+", "", tmp)
    tmp <- sub('/0', "/", tmp)
    return(tmp)
  }

  if(is.null(nmea_strings)) {
    message("convert_nmea_btd: nmea_strings is NULL. Select a .xml or .marp file.")
    nmea_strings <-
      choose.files(
        default = "*.xml",
        caption = "Select .marp or .xml file(s)",
        multi = TRUE,
        filters =
          matrix(
            c("XML SCS/Poseidon (.xml)", "*.xml",
              "Marport (.marp)", "*.marp"),
            byrow = TRUE,
            ncol = 2)
      )

    stopifnot("convert_nmea_btd: Must select a file." = length(nmea_strings) >= 1)
  }

  # Handle Wheelhouse/Calypso .marp files
  if(all(grepl(pattern = ".marp", x = nmea_strings))) {

    message("convert_nmea_btd: Extracting NMEA strings from .marp files.")

    nmea_list <- vector(mode = "list", length = length(nmea_strings))
    nmea_strings <- lapply(
      X = nmea_strings,
      FUN = function(x) {
        lines <- readLines(x)
        lines[any(
          c(grepl(lines, pattern = "\\$GPZDA"),
            grepl(lines, pattern = "\\$GPGLL"),
            grepl(lines, pattern = "\\$GPRMC"),
            grepl(lines, pattern = "\\$GPVTG"),
            grepl(lines, pattern = "\\$GPGGA"),
            grepl(lines, pattern = "\\$01TE"),
            grepl(lines, pattern = "\\:::m"),
            grepl(lines, pattern = "\\$01DST"))
        )]
      })

    nmea_strings <- unname(unlist(nmea_strings))

  }

  # Handle Poseidon/SCS .xml files
  if(all(grepl(pattern = ".xml", x = nmea_strings))) {

    message("convert_nmea_btd: Extracting NMEA strings from Poseidon/SCS .xml files.")

    nmea_list <- vector(mode = "list", length = length(nmea_strings))
    nmea_strings <- lapply(
      X = nmea_strings,
      FUN = function(x) {
        lines <- readLines(x)
        lines[any(
          c(grepl(lines, pattern = "\\PW,"),
            grepl(lines, pattern = "\\$GPRMC"),
            grepl(lines, pattern = "\\PD,"),
            grepl(lines, pattern = "\\HR,"))
        )]
      })

    nmea_strings <- unname(unlist(nmea_strings))

  }

  # Add tests to check that NMEA strings include temperature and depth
  if(is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if(is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if(is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if(is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number (optional):  ") }
  if(is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number (optional):  ") }
  if(is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of sensor (optional):  ") }

  # Initialize lists to store parsed data
  matched_bt <- list()

  # Function to convert HHMMSS.SSS to POSIXct
  parse_time_marp <-
    function(hhmmss, date_str) {
    as.POSIXct(
      strptime(
        paste0(
          date_str,
          # sprintf("%06.3f", as.numeric(hhmmss))
          gsub(
            pattern = " ",
            replacement = "0",
            x =
              format(
                as.numeric(hhmmss),
                nsmall = 3,
                width = 10,
                trim = FALSE)
          )
        ),
        "%Y-%m-%d%H%M%OS"
      ),
      tz = "UTC"
    )
  }

  # Function to convert HHMMSS.SSS to POSIXct
  parse_time_xml <-
    function(dt_line) {
      dt_str <- gsub('.*timestamp="([^"]+)".*', "\\1", dt_line)
      as.POSIXct(dt_str, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    }

  # Track current time from $GPZDA or :::msg yyyymmdd-HHMMSS
  current_time <- NA
  current_date <- NA
  year <- NA
  month <- NA
  day <- NA

  last_data_time <- list(depth = NA, temp = NA, height = NA)
  pending <- list()

  # Line-by-line processing - required to handle data errors
  for(line in nmea_strings) {

    # Parse lines to extract dates/times from .marp files
    if(grepl(pattern = ".* (\\d{8})-\\d{6}Z", x = line)) {

      year <- as.numeric(sub(".* (\\d{4})\\d{4}-\\d{6}Z", "\\1", line))
      month <- as.numeric(sub(".*\\d{4}(\\d{2})\\d{2}-\\d{6}Z", "\\1", line))
      day   <- as.numeric(sub(".*\\d{6}(\\d{2})-\\d{6}Z", "\\1", line))
      time_str <- sub(".*-(\\d{6})Z", "\\1", line)
      current_date <- sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))
      current_time <- parse_time_marp(time_str, current_date)

    } else if(grepl("^\\$GPZDA", line)) {
      parts <- strsplit(line, ",")[[1]]
      time_str <- parts[2]
      day <- parts[3]
      month <- parts[4]
      year <- parts[5]
      current_date <- sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))
      current_time <- parse_time_marp(time_str, current_date)

    } else if(grepl("^\\$GPGGA", line) | grepl("^\\$GPRMC", line)) {
      parts <- strsplit(line, ",")[[1]]
      time_str <- parts[2]
      current_date <- sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))
      current_time <- parse_time_marp(time_str, current_date)

    } else if(grepl("^\\$01TED", line)) {
      val <- as.numeric(sub(",m.*", "", strsplit(line, ",")[[1]][2]))
      if(!is.na(current_time)) {
        last_data_time$depth <- current_time
        pending$depth <- list(value = val, time = current_time)
      }

    } else if(grepl("^\\$01TET", line)) {
      val <- as.numeric(sub(",C.*", "", strsplit(line, ",")[[1]][2]))
      if(!is.na(current_time)) {
        last_data_time$temp <- current_time
        pending$temp <- list(value = val, time = current_time)
      }

    } else if(grepl("^\\$01TEH", line)) {
      val <- as.numeric(sub(",m.*", "", strsplit(line, ",")[[1]][2]))
      if(!is.na(current_time)) {
        last_data_time$height <- current_time
        pending$height <- list(value = val, time = current_time)
      }
    } else if(grepl("^\\$01DST", line)) {
      val <- as.numeric(sub(",m.*", "", strsplit(line, ",")[[1]][4]))
      if(!is.na(current_time)) {
        last_data_time$net_spread <- current_time
        pending$net_spread <- list(value = val, time = current_time)
      } # Handling .xml files
    } else if(grepl("PW", line)) {
      pending$net_spread <- list(value, time = parse_time_xml(line))
      val <- as.numeric(gsub(".*,|\\*.*", "", line))
    } else {
      next
    }

    # When depth and temperature are available, or at least one changes, record a row
    if(!is.na(current_time)) {
      values <- list(
        DATE_TIME = current_time,
        DEPTH = if(!is.null(pending$depth) &&
                   difftime(current_time, pending$depth$time, units = "secs") <= 5) pending$depth$value else NA,
        TEMPERATURE = if(!is.null(pending$temp) &&
                         difftime(current_time, pending$temp$time, units = "secs") <= 5) pending$temp$value else NA,
        NET_HEIGHT = if(!is.null(pending$height) &&
                        difftime(current_time, pending$height$time, units = "secs") <= 1) pending$height$value else NA,
        NET_SPREAD = if(!is.null(pending$net_spread) &&
                        difftime(current_time, pending$net_spread$time, units = "secs") <= 1) pending$net_spread$value else NA
      )
      matched_bt[[length(matched_bt) + 1]] <- values
    }
  }

  # Convert list to data.frame
  matched_bt <- do.call(rbind, lapply(matched_bt, as.data.frame))

  if(is.null(matched_bt)) {
    warning("convert_nmea_btd: No temperature, depth, spread, or height observations. No valid output.")
    return(NULL)
  }

  if(!("NET_HEIGHT" %in% names(matched_bt))) {
    matched_bt$NET_HEIGHT <- NA
  }

  if(!("NET_SPREAD" %in% names(matched_bt))) {
    matched_bt$NET_SPREAD <- NA
  }

  output_btd <- matched_bt[c("DATE_TIME", "DEPTH", "TEMPERATURE")]

  if(!is.null(output_btd)) {

    output_btd <- output_btd[!duplicated(output_btd$DATE_TIME), ]  # Remove duplicates

    output_btd <-
      output_btd[complete.cases(output_btd), ]

    output_btd <- output_btd[!(output_btd$TEMPERATURE == 0 & output_btd$DEPTH == 0), ]

    if(!is.na(min_depth) & !is.na(max_depth)) {
      output_btd <- output_btd[output_btd$DEPTH >= min_depth & output_btd$DEPTH <= max_depth, ]
    }

    if(!is.na(min_temperature) & !is.na(max_temperature)) {
      output_btd <- output_btd[output_btd$TEMPERATURE >= min_temperature & output_btd$TEMPERATURE <= max_temperature, ]
    }

    if(nrow(output_btd) < 3) {
      warning("convert_nmea_btd: No outputs created. Fewer than three valid temperature/depth observations.")
      return(NULL)
    }

    rownames(output_btd) <- NULL

    # Convert DATE_TIME to Alaska time and format for .BTD
    attr(output_btd$DATE_TIME, "tzone") <- "UTC"
    attr(output_btd$DATE_TIME, "tzone") <- "America/Anchorage"

    # Write .BTH file
    output_bth <-
      data.frame(
        VESSEL = VESSEL,
        CRUISE = CRUISE,
        HAUL = HAUL,
        MODEL_NUMBER = MODEL_NUMBER,
        VERSION_NUMBER = VERSION_NUMBER,
        SERIAL_NUMBER = SERIAL_NUMBER,
        HOST_TIME = format(max(output_btd$DATE_TIME, na.rm = TRUE), "%m/%d/%Y %H:%M:%S"),
        LOGGER_TIME = format(max(output_btd$DATE_TIME, na.rm = TRUE), "%m/%d/%Y %H:%M:%S"),
        LOGGING_START = format(min(output_btd$DATE_TIME, na.rm = TRUE), "%m/%d/%Y %H:%M:%S"),
        LOGGING_END = format(max(output_btd$DATE_TIME, na.rm = TRUE), "%m/%d/%Y %H:%M:%S"),
        SAMPLE_PERIOD = as.integer(median(diff(output_btd$DATE_TIME), na.rm = TRUE)),
        NUMBER_CHANNELS = 2,
        NUMBER_SAMPLES = nrow(output_btd),
        MODE = 2
      )

    output_bth[which(is.na(output_bth))] <- ""

    bth_path <- paste0(getwd(), "/HAUL", numbers0(x = HAUL, number_places = 4), ".BTH")

    utils::write.csv(
      x = output_bth,
      file = bth_path,
      quote = FALSE,
      row.names = FALSE
    )

    cat(paste0("convert_nmea_btd: .BTH file saved to ", bth_path, "\n"))

    if(interactive_editing) {

      par(mfrow = c(2,1))
      plot(output_btd$DATE_TIME, output_btd$TEMPERATURE, xlab = "Datetime", ylab = "TEMPERATURE")
      mtext("Raw data.")
      plot(output_btd$DATE_TIME, output_btd$DEPTH, xlab = "Datetime", ylab = "DEPTH")
      mtext("Raw data.")

      dummy <- readline("Plotting raw data. Set plot to actual size (RStudio: View > Actual Size) then press ENTER to begin manual point editing.")

      output_btd <- interactive_point_editing(x = output_btd, x_col = "DATE_TIME", y_col = "DEPTH", tol = 0.5)
      output_btd <- interactive_point_editing(x = output_btd, x_col = "DATE_TIME", y_col = "TEMPERATURE", tol = 0.5)

      par(mfrow = c(2,1))
      plot(output_btd$DATE_TIME, output_btd$TEMPERATURE, xlab = "Datetime", ylab = "TEMPERATURE")
      mtext("Cleaned data.")
      plot(output_btd$DATE_TIME, output_btd$DEPTH, xlab = "Datetime", ylab = "DEPTH")
      mtext("Cleaned data.")

    } else {
      par(mfrow = c(2,1))
      plot(output_btd$DATE_TIME, output_btd$TEMPERATURE, xlab = "Datetime", ylab = "TEMPERATURE")
      mtext("Delete temp outlier rows\nfrom .BTD in a text editor.")
      plot(output_btd$DATE_TIME, output_btd$DEPTH, xlab = "Datetime", ylab = "DEPTH")
      mtext("Delete depth outlier rows\nfrom .BTD in a text editor.")
    }



    # Write .BTD file
    output_btd$DATE_TIME <-
      format_date(
        format(output_btd$DATE_TIME, "%m/%d/%Y %H:%M:%S")
      )

    output_btd <-
      data.frame(
        VESSEL = VESSEL,
        CRUISE = CRUISE,
        HAUL = HAUL,
        SERIAL_NUMBER = SERIAL_NUMBER,
        DATE_TIME = output_btd$DATE_TIME,
        TEMPERATURE = format(output_btd$TEMPERATURE, nsmall = 3),
        DEPTH = format(output_btd$DEPTH, nsmall = 1)
      )

    output_btd[which(is.na(output_btd), arr.ind = TRUE)] <- ""

    btd_path <- paste0(getwd(), "/HAUL", numbers0(x = HAUL, number_places = 4), ".BTD")

    utils::write.csv(
      x = output_btd,
      file = btd_path,
      quote = FALSE,
      row.names = FALSE
    )

    cat(paste0("convert_nmea_btd: .BTD file saved to ", btd_path, "\n"))

  } else {
    output_btd <- NULL
    output_bth <- NULL
    warning("convert_nmea_btd: Fewer than three temperature/depth observations. No valid output.")
  }

  output_hs <- matched_bt[c("DATE_TIME", "NET_HEIGHT", "NET_SPREAD")]

  output_hs <- output_hs[!duplicated(output_hs$DATE_TIME), ]

  output_hs <- output_hs[!is.na(output_hs$NET_HEIGHT) | !is.na(output_hs$NET_SPREAD), ]

  if(nrow(output_hs) > 0) {
    output_hs <-
      data.frame(
        VESSEL = VESSEL,
        CRUISE = CRUISE,
        HAUL = HAUL,
        DATE_TIME = output_hs$DATE_TIME,
        NET_HEIGHT = output_hs$NET_HEIGHT,
        NET_SPREAD = output_hs$NET_SPREAD
      )

    output_hs[which(is.na(output_hs), arr.ind = TRUE)] <- ""

    hs_path <- paste0(getwd(), "/HAUL", numbers0(x = HAUL, number_places = 4), ".hs")

    utils::write.csv(
      x = output_hs,
      file = hs_path,
      quote = FALSE,
      row.names = FALSE
    )

    # plot(output_hs$DATE_TIME, output_hs$NET_HEIGHT, xlab = "Datetime", ylab = "NET_HEIGHT")
    # mtext("Do not edit height.")
    # plot(output_hs$DATE_TIME, output_hs$NET_SPREAD, xlab = "Datetime", ylab = "NET_SPREAD")
    # mtext("Do not edit spread.")

    cat(paste0("convert_nmea_btd: Height-spread (.hs) file saved to ", hs_path, "\n"))

  } else {
    output_hs <- NULL
  }

  return(list(btd = output_btd, bth = output_bth, height_spread = output_hs))

}
