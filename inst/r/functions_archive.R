# Volumentric Tows

#' Volumetric Tow Calculator
#' @description
#' recall on a boat...
#'     bow
#' port   starbord
#'    stern
#' thus heights in the bin should be taken like this:
#'                     ...bow...
#' bow-port (bp)       bow-center (bc)      bow-starbord (bs)
#' center-port (cp)    center-center (cc)   center-starbord (cs)
#' stern-port (sp)     stern-center (sc)    stern-starbord (ss)
#'                     ...stern...
#' @param volume_method Character: "average" (default) or "contour". If "contour", an inverse-distance weighted interpolation will be applied to the heights across the bin and summed to find volume.
#' If "average", the bin heights will be averaged in 4 area quadrants (as done in old documentation) to find volume.
#' @param density_method Character: "estimated" (default) or "observed". For "observed", add a numeric of the observed (calculated by you on deck) density. For "estimated", create a 2 column data.frame with "taxon" and "prop" (proportion) of each fish caught in the survey: "pollock", "pcod", "Atka mackerel", "pop", "nrf", "Giant grenadier", "flatfish", "snow crab"
#' @param bin_dimentions list of 3 items (length of "bow" side of shape, length of "stern" side of shape, and the "height"s between them) or NULL (default).
#' @param area_of_bin A single numeric with the area, or the bin in cm3 or NULL (default). If null, area will be calculated from the bin_dimensions. Will be used instead of bin_dimentions if not NULL.
#' @param heights_in_bin List of 9 items. See example below for structure. 9 hieghts from the bin should be taken from the stern starbord, center starbord, bow starbord, stern center, center center, bow center, stern port, center port, bow port
#' @param density Dataframe or numeric. For density_method = "observed", add a numeric of the observed (calculated by you on deck) density. For density_method = "estimated", create a 2 column data.frame with "taxon" and "prop" (proportion) of each fish caught in the survey: "pollock", "pcod", "Atka mackerel", "pop", "nrf", "Giant grenadier", "flatfish", "snow crab"
#'
#' @export
#'
#' @examples
#' # Example from EBS 2022 Alaska Knight Haul 144 where a large bag (4650 kg)
#' # was weighed by a load cell but volumetric measurements were also taken for
#' # comparison and study.
#'
#' # 9 heights from the bin at each location described below
#' # see notes above for boat layout.
#' heights_in_bin <- list( # in cm
#'  "ss" = 65, # stern starbord
#'  "cs" = 55, # center starbord
#'  "bs" = 43, # bow starbord
#'  "sc" = 51, # stern center
#'  "cc" = 34, # center center
#'  "bc" = 27, # bow center
#'  "sp" = 20, # stern port
#'  "cp" = 29, # center port
#'  "bp" = 31) # bow port
#'
#' # develop a dataframe of the taxon groups below and proportions found from
#' # sample bag (be sure not to include weights of organisms rescued from the bin!)
#'
#' density_prop <- data.frame(taxon = c("pollock", "pcod", "Atka mackerel",
#'                                      "pop", "nrf", "Giant grenadier",
#'                                      "flatfish", "snow crab"),
#'                            prop = c(0.045011, 0.00533, 0,
#'                                     0.8843, 0, 0,
#'                                     0.022022, 0.0000398))
#' # Prefered/easiest
#'
#' calc_volumetric_tow(volume_method = "contour",
#'                              # Using function to develop an IDW
#'                              # contour over the heights for estimating volume.
#'                density_method = "estimated",
#'                              # Determining density proportions to apply to
#'                              # known densities of fish
#'                bin_dimentions = # Dimensions for the Alaska Knight's
#'                                 # trapezoidal bin, measured in cm
#'                                 list("bow" = 420,
#'                                      "stern" = 355,
#'                                      "height" = 315.5),
#'                              # the height is actually 320, but subtract 4.5
#'                              # for width of board in middle
#'                heights_in_bin = heights_in_bin, # measured in cm
#'                density = density_prop)
#'
#' # Using other methods
#' calc_volumetric_tow(volume_method = "average",
#'                        # Averaging the 4 quadrants of the bin to find volume
#'                density_method = "observed",
#'                        # Directly measured density. See documentation in RACE
#'                        # Survey app for more details.
#'                bin_dimentions = 162,
#'                        # Alaska Knight Vessel code - bin_dimensions stored in
#'                        # the function and this vessel code will look them up
#'                heights_in_bin = heights_in_bin, # Measured in cm
#'                density = 775.8343)
#'                        # if you measured the density directly (this happens
#'                        # to be what the proportions above would solve for)
#'
#' # Using other methods
#' calc_volumetric_tow(volume_method = "average",
#'                        # Averaging the 4 quadrants of the bin to find volume
#'                density_method = "observed",
#'                        # Directly measured density. See documentation in
#'                        # RACE Survey app for more details.
#'                area_of_bin = 122256.2,  # calculated area of the bin.
#'                heights_in_bin = heights_in_bin, # Measured in cm
#'                density = 775.8343)
#'                        # if you measured the density directly (this happens
#'                        # to be what the proportions above would solve for)
#'
#' # F/V Vesteraalen Haul 128 EBS 2022
#' calc_volumetric_tow( # volume_method = "average",
#'   bin_dimentions = 94,
#'   heights_in_bin = list( # in cm
#'     "ss" = 50, # stern starbord
#'     "cs" = 63, # center starbord
#'     "bs" = 56, # bow starbord
#'     "sc" = 50, # stern center
#'     "cc" = 60, # center center
#'     "bc" = 61, # bow center
#'     "sp" = 50, # stern port
#'     "cp" = 50, # center port
#'     "bp" = 41), # bow port,
#'  density = data.frame("taxon" = c("pollock", "pcod", "Atka mackerel",
#'     "pop", "nrf", "Giant grenadier", "flatfish", "snow crab"),
#'     "prop" = c(1, 0, 0, 0, 0, 0, 0, 0)))
calc_volumetric_tow <- function(volume_method = "contour",
                           density_method = "estimated",
                           bin_dimentions = NULL,
                           area_of_bin = NULL,
                           heights_in_bin,
                           density = NULL) {

  if (is.null(area_of_bin)) {
  if (methods::is(object = bin_dimentions, class2 = "list")) {
    bin_dimentions <- bin_dimentions
  } else if (bin_dimentions == 162) { # F/V Alaska Knight
    bin_dimentions <- list("bow" = 420,
                           "stern" = 355,
                           # "port" = 310,
                           # "starbord" = 330,
                           "height" = 315.5) # actually 320, but subtract 4.5 for width of board in middle
  } else if (bin_dimentions == 148) { # F/V Ocean Explorer
    # 29.785 m2
    bin_dimentions <- list("bow" = 440,
                           "stern" = 365,
                           "height" = 740) # actually 320, but subtract 4.5 for width of board in middle
  } else if (bin_dimentions == 94) { # F/V Vesteraalen
    # 29.785 m2
    bin_dimentions <- list("bow" = 320,
                           "stern" = 345,
                           "height" = 325) # actually 320, but subtract 4.5 for width of board in middle
  }
}
  # I. Calculate Catch Volume
  # - Dump the catch into a bin, such that the height (depth) of the catch is maximized (e.g., >3 ft deep).
  # - Level the catch.
  # - Draw diagram of bin area to be used (on back of this form).
  # - Measure the horizontal dimensions (Width and Length) of the bin for the bin area using a tape measure. (for irregular bins, e.g., trapezoids, use the average of unequal Widths and/or Lengths).
  # - Measure the bin Height at multiple positions (perimeter and interior) using a meter stick. Average the Height.
  # - Volume of the bin = Width x Length x Height.
  # An accurate way to estimate the volume of the catch is to imagine the bin as four (or more) rectangles with a corner of each meeting in the middle of the bin. Measure the catch height at each of the four corners of each rectangle. For a bin with four rectangles this is a minimum of 9 catch height estimates (six rectangles is minimum of 12 height measurements). Average the heights from each of the four corners and multiply times the width and length for each of the rectangles to get the volume.

  if (volume_method == "average") {

    if (is.null(area_of_bin)) {

      area_of_bin <- ((bin_dimentions$bow+bin_dimentions$stern)/2)*bin_dimentions$height
    }

    q1 <- (heights_in_bin$bp + heights_in_bin$bc + heights_in_bin$cp + heights_in_bin$cc)/4
    q2 <- (heights_in_bin$cp + heights_in_bin$cc + heights_in_bin$sp + heights_in_bin$cs)/4
    q3 <- (heights_in_bin$bc + heights_in_bin$cc + heights_in_bin$bs + heights_in_bin$sc)/4
    q4 <- (heights_in_bin$cc+ heights_in_bin$sc + heights_in_bin$sc + heights_in_bin$ss)/4

    volume_of_bin <- ( (q1*(area_of_bin/4)) + (q2*(area_of_bin/4)) +
                         (q3*(area_of_bin/4)) + (q4*(area_of_bin/4)) ) * 1e-6

  } else if (volume_method == "contour") {

    bow_stern_diff <- round((bin_dimentions$bow - bin_dimentions$stern)/2, digits = 2)

    df <- heights_in_bin1 <- data.frame("x" = c(bin_dimentions$stern+bow_stern_diff, round(bin_dimentions$stern+(bow_stern_diff/2)), bin_dimentions$bow,
                                                round(bin_dimentions$bow/2), round(bin_dimentions$bow/2), round(bin_dimentions$bow/2),
                                                bow_stern_diff, round(bow_stern_diff/2), 1),
                                        "y" = c(bin_dimentions$h, round(bin_dimentions$h/2), 1,
                                                bin_dimentions$h, round(bin_dimentions$h/2), 1,
                                                bin_dimentions$h, round(bin_dimentions$h/2), 1),
                                        "vals" = (unlist(heights_in_bin)))

    idw.nmax = 4
    grid.cell <- c(10,10)

    bin_polygon <- df
    bin_polygon <- bin_polygon[!grepl(pattern = "m", x = rownames(bin_polygon)),]
    bin_polygon <- bin_polygon[!grepl(pattern = "c", x = rownames(bin_polygon)),]
    bin_polygon <- bin_polygon %>%
      dplyr::arrange(x) %>%
      dplyr::mutate(group = 1) %>%
      sf::st_as_sf(coords = c("x", "y")) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
      sf::st_cast("POLYGON") #%>%
    # plot()

    x <- df
    colnames(x) <- c("LONGITUDE", "LATITUDE", "CPUE_KGHA")
    x <- sf::st_as_sf(x,
                      coords = c(x = "LONGITUDE", y = "LATITUDE"))

    extrap.box <- sf::st_bbox(x)

    idw_fit <- gstat::gstat(formula = CPUE_KGHA ~ 1, locations = x,
                            nmax = idw.nmax)

    stn.predict <- stats::predict(idw_fit, x)

    sp_extrap.raster <- raster::raster(xmn = extrap.box["xmin"],
                                       xmx = extrap.box["xmax"],
                                       ymn = extrap.box["ymin"],
                                       ymx = extrap.box["ymax"],
                                       ncol = (extrap.box["xmax"] - extrap.box["xmin"])/grid.cell[1],
                                       nrow = (extrap.box["ymax"] - extrap.box["ymin"])/grid.cell[2])

    extrap.grid <- stats::predict(idw_fit, methods::as(sp_extrap.raster, "SpatialPoints")) %>%
      sf::st_as_sf() %>%
      stars::st_rasterize()

    extrap.grid.crop <-  sf::st_crop(x = extrap.grid, y = bin_polygon)

    volume_of_bin <- sum(extrap.grid.crop$var1.pred, na.rm = TRUE) * 1e-6 *
      grid.cell[1] * grid.cell[2]

    # ggplot() +
    #   # stars::geom_stars(data = extrap.grid)  +
    #   stars::geom_stars(data = extrap.grid.crop) +
    #   geom_sf(data = bin_polygon, fill = NA)

  }

  # II. Calculate Catch Density

  if (density_method == "estimated") {
    # - Determine the predominant species in the catch, and use following table below to apply a density estimate.

    if (methods::is(object = density, class2 = "data.frame")) {
    # Density (kg/m3)
    density_est <- data.frame(taxon = c("pollock", "pcod", "Atka mackerel",
                                        "pop", "nrf", "Giant grenadier",
                                        "flatfish", "snow crab"),
                              dens = c(950, 900, 900,
                                       800, 840, 980,
                                       945, 654))

    # - If the catch consists of a mix of several of the above species, use the proportion of each species to
    # calculate density, e.g., 60% POP and 40% pollock: (0.6 x 800) + (0.4 x 950) = 860.
    dens <- dplyr::left_join(density %>%
                               dplyr::mutate(taxon = tolower(taxon)),
                             density_est %>%
                               dplyr::mutate(taxon = tolower(taxon)))
    dens$density <- dens$dens * dens$prop

    density_of_bin <- sum(dens$density)
    }

  } else if (density_method == "observed") {

    # Method 2:
    #   -Measure the density directly. If the catch does not fit easily into one of the above single species or species
    # mixes (e.g., sponge tow), measure the density by filling a deep container of known volume with a random
    # portion of the catch, then weigh that subsample. Density = Weight / Volume = kg/m3.
    # The chemical tote, for example, is a good deep container that allows for an accurate measurement of volume.
    # To get the weight, weigh the tote with the catch (using sling & load-cell), discard the catch (or dump it on the
    # sorting table as part of your normal catch subsample), then weigh the empty tote and subtract that weight.
    # ***Remember to measure your main bin volume first before removing a subsample***

    if (methods::is(object = density, class2 = "numeric") & length(density) == 1) {
      density_of_bin <- density
    }

  }
  # III. Total Catch Weight (kg) = Density x Volume
  # Helpful suggestion: For larger catches, dump only a portion of the catch into the bin for the volumetric estimate, then weigh
  # the remaining portion of the catch still in the trawl net with the crane/load-cell (subtracting the weight of the net). Total
  # Catch weight is then the addition of the remaining net catch weight + bin-volume-estimated weight. The catch subsample
  # can be taken from either portion, but the catch composition needs to be random throughout.

  weight_of_bin <- # kg
    density_of_bin * # m3
    volume_of_bin # kg/m3

  return(weight_of_bin)

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
