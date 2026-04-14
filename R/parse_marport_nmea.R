#' Parse Marport NMEA Logs into wide format data frames
#'
#' Processes a vector of raw NMEA 0183 strings, specifically targeting Marport
#' sensor data. It groups sentences by time (\code{$GPZDA}), extracts spatial
#' coordinates (\code{$GPGGA}), and extracts Marport sensor values
#' (\code{$MPMSD}) to create a data.frame.
#'
#' @param nmea_strings A character vector containing raw NMEA sentences (e.g.,
#'   starting with \code{$GPZDA}, \code{$GPGGA}, or \code{$MPMSD}).
#'
#' @return A data frame in "wide" format where each row represents a unique
#'   timestamp and location. Columns include:
#'   \itemize{
#'     \item \code{DATE_TIME}: POSIXct UTC timestamp.
#'     \item \code{LATITUDE} / \code{LONGITUDE}: Decimal coordinates.
#'     \item Sensor columns: \code{DEPTH}, \code{TEMPERATURE}, \code{NET_HEIGHT},
#'           \code{DOOR_SPREAD}, and \code{NET_SPREAD} (depending on data availability).
#'   }
#' @examples
#' test_strings <- c(
#'   "$GPZDA,204954.000,24,02,2026,,*5A",
#'   "$GPGGA,204954.000,4741.1526,N,12215.0957,W,1,07,1.7,35.8,M,-17.2,M,,0000*5E",
#'   "$MPMSD,T,PW,18,XST,m,127.64*37",
#'   "$MPMSD,T,PD,23,XST,m,1.13*29",
#'   "$MPMSD,T,HR,11,DTB,m,2.40*2E",
#'   "$MPMSD,T,HR,11,DPT,m,0.65*39",
#'   "$MPMSD,T,HR,11,TMP,c,14.53*0E",
#'   "$GPZDA,204955.000,24,02,2026,,*5A",
#'   "$GPGGA,204955.000,4741.1526,N,12215.0957,W,1,07,1.7,35.8,M,-17.2,M,,0000*5E"
#' )
#'
#' parse_marport_nmea(test_strings)
#'
#' @export

parse_marport_nmea <-
  function(nmea_strings) {

    # Assign data types to individual Marport strings; can be extended with additional string types
    set_data_type <-
      function(line) {
        mappings <- list(
          list(must_have = c("HR,", "DPT", "*"), type = "DEPTH"),
          list(must_have = c("HR,", "TMP", "*"), type = "TEMPERATURE"),
          list(must_have = c("HR", "DTB", "*"), type = "NET_HEIGHT"),
          list(must_have = c("PD", "XST", "*"), type = "DOOR_SPREAD"),
          list(must_have = c("PW", "XST", "*"), type = "NET_SPREAD")
        )

        for(map in mappings) {
          if (all(sapply(map$must_have, grepl, x = line))) {
            return(map$type)
          }
        }

        return(NA)
      }

    is_zda <- grepl("^\\$GPZDA", nmea_strings)

    grouping_factor <- cumsum(is_zda)

    nmea_list <- split(nmea_strings, grouping_factor)

    # Remove any lines before the first $GPZDA
    nmea_list <- nmea_list[names(nmea_list) != "0"]

    string_data_list <-
      lapply(
        X = nmea_list,
        FUN =
          function(group) {

            zda_line <- group[grepl(pattern = "GPZDA", x = group)]

            gga_line <- group[grepl(pattern = "GPGGA", x = group)]

            # Return NULL if there are no date/time or GPS data
            if(length(zda_line) < 1 | length(gga_line) < 1) {
              return(NULL)
            }

            # Parse date/time sentences
            zda <- parse_zda(x = zda_line)

            # Parse GPS sentences
            gga <- parse_gga(x = gga_line)

            # Parse Marport proprietary sentences
            mpmsd_lines <- group[grepl(pattern = "MPMSD", x = group)]
            values <-
              as.numeric(
                gsub(".*,|\\*.*", "", mpmsd_lines)
              )

            # Assing NA if there are no Marport sensor data
            if(length(values) < 1) {
              values <- NA
            }

            data_type <-
              sapply(
                X = mpmsd_lines,
                FUN = function(x) {
                  set_data_type(x)
                }
              )

            data_type <- unname(data_type)

            if(length(data_type) < 1) {
              data_type <- NA
            }

            out <- data.frame(
              DATE_TIME = zda,
              LATITUDE = gga[['latitude']],
              LONGITUDE = gga[['longitude']],
              values = values,
              data_type = data_type
            )

            return(out)

          }
      )

    string_data_long <-
      do.call(what = rbind, args = string_data_list)

    rownames(string_data_long) <- NULL

    output <-
      stats::reshape(
        data = string_data_long,
        idvar = c("DATE_TIME", "LONGITUDE", "LATITUDE"),
        timevar = "data_type",
        v.names = NULL,
        direction = "wide"
      )

    names(output) <- gsub("values\\.", "", names(output))

    rownames(output) <- NULL

    output <- output[, names(output) != "NA"]

    output <- output[order(output$DATE_TIME), ]

    return(output)

  }


#' Parse NMEA GGA sentences
#'
#' Extracts latitude and longitude from a Global Positioning System Fix Data (GGA)
#' sentence and converts coordinates from NMEA format (DDMM.MMMM) to decimal degrees.
#'
#' @param x A character string containing a single NMEA GGA sentence.
#'
#' @return A named numeric vector of length 2 containing \code{latitude} and
#'   \code{longitude} in decimal degrees.
#'
#' @details
#' The function assumes the standard NMEA 0183 structure where:
#' \itemize{
#'   \item \code{x[3]} is latitude in DDMM.MMMM format.
#'   \item \code{x[4]} is the N/S indicator.
#'   \item \code{x[5]} is longitude in DDDMM.MMMM format.
#'   \item \code{x[6]} is the E/W indicator.
#' }
#'
#' @examples
#' gga_str <- "$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47"
#' parse_gga(gga_str)
#'
#' @noRd

parse_gga <- function(x) {

  x <- strsplit(x, split = ",")

  x <- unlist(x)

  lat_val <- as.numeric(x[3])
  lon_val <- as.numeric(x[5])

  lat_dec <- floor(lat_val / 100) + (lat_val %% 100) / 60
  lon_dec <- floor(lon_val / 100) + (lon_val %% 100) / 60

  if (x[4] == "S") lat_dec <- lat_dec * -1
  if (x[6] == "W") lon_dec <- lon_dec * -1

  return(c(latitude = lat_dec, longitude = lon_dec))
}

#' Parse NMEA ZDA Sentences
#'
#' Extracts date and time information from a ZDA (Time & Date) NMEA sentence
#' and converts it into a POSIXct object.
#'
#' @param x A character string containing a single NMEA ZDA sentence.
#'
#' @return A \code{POSIXct} object representing the date and time in UTC,
#'   truncated to the nearest second.
#'
#' @details
#' The function parses the following fields from the comma-separated string:
#' \itemize{
#'   \item Field 2: UTC Time (HHMMSS.SS)
#'   \item Field 3: Day
#'   \item Field 4: Month
#'   \item Field 5: Year
#' }
#'
#' @examples
#' zda_str <- "$GPZDA,201530.00,04,07,2002,00,00*60"
#' parse_zda(zda_str)
#'
#' @noRd

parse_zda <- function(x) {

  x <- strsplit(x, split = ",")

  x <- unlist(x)

  # Extract time and date from split string
  raw_time <- x[2]
  formatted_time <- paste0(
    substr(raw_time, 1, 2), ":",
    substr(raw_time, 3, 4), ":",
    substr(raw_time, 5, 10)
  )

  formatted_date <-
    paste(
      x[5], x[4], x[3], sep = "-"
    )

  formatted_dt <- paste0(formatted_date, " ", formatted_time)

  formatted_dt <- as.POSIXct(formatted_dt, tz = "UTC")

  # Truncate to seconds
  formatted_dt <- trunc(formatted_dt, units = "secs")

  return(formatted_dt)
}
