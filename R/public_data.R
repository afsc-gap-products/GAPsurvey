#' @title Public data from FOSS
#' @description This dataset includes zero-filled (presence and absence) observations and catch-per-unit-effort (CPUE) estimates for all identified species at for index stations by the Resource Assessment and Conservation Engineering Division (RACE) Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC). There are no legal restrictions on access to the data. The data from this dataset are shared on the Fisheries One Stop Stop (FOSS) platform (https://www.fisheries.noaa.gov/foss/). The GitHub repository for the scripts that created this code can be found at https://github.com/afsc-gap-products/gap_public_data. For more information about codes used in the tables, please refer to the survey code books (https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual). These data were last updated April 24, 2023.
#' @usage data('public_data')
#' @author Emily Markowitz (emily.markowitz AT noaa.gov)
#' @format A data frame with 905768 observations on the following 35 variables.
#' \describe{
#'   \item{\code{scientific_name}}{Taxon Scientific Name. The scientific name of the organism associated with the 'common_name' and 'species_code' columns. For a complete taxon list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{species_code}}{Taxon Code. The species code of the organism associated with the 'common_name' and 'scientific_name' columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{srvy}}{Survey. Abbreviated survey names. The column 'srvy' is associated with the 'survey' and 'survey_id' columns. Northern Bering Sea (NBS), Southeastern Bering Sea (EBS), Bering Sea Slope (BSS), Gulf of Alaska (GOA), Aleutian Islands (AI).}
#'   \item{\code{station}}{Station ID. Alpha-numeric designation for the station established in the design of a survey.}
#'   \item{\code{stratum}}{Stratum ID. RACE database statistical area for analyzing data. Strata were designed using bathymetry and other geographic and habitat-related elements. The strata are unique to each survey series. Stratum of value 0 indicates experimental tows.}
#'   \item{\code{surface_temperature_c}}{Surface Temperature (Degrees Celsius). Surface temperature (tenths of a degree Celsius); NA indicates removed or missing values.}
#'   \item{\code{survey}}{Survey Name. Name and description of survey. The column 'survey' is associated with the 'srvy' and 'survey_id' columns.}
#'   \item{\code{survey_definition_id}}{Survey ID. This number uniquely identifies a survey. Name and description of survey. The column 'survey_id' is associated with the 'srvy' and 'survey' columns. For a complete list of surveys, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{taxon_confidence}}{Taxon Confidence Rating. Confidence in the ability of the survey team to correctly identify the taxon to the specified level, based solely on identification skill (e.g., not likelihood of a taxon being caught at that station on a location-by-location basis). Quality codes follow: **'High'**: High confidence and consistency. Taxonomy is stable and reliable at this level, and field identification characteristics are well known and reliable. **'Moderate'**: Moderate confidence. Taxonomy may be questionable at this level, or field identification characteristics may be variable and difficult to assess consistently. **'Low'**: Low confidence. Taxonomy is incompletely known, or reliable field identification characteristics are unknown. Documentation: [Species identification confidence in the eastern Bering Sea shelf survey (1982-2008)](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2009-04.pdf), [Species identification confidence in the eastern Bering Sea slope survey (1976-2010)](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-05.pdf), and [Species identification confidence in the Gulf of Alaska and Aleutian Islands surveys (1980-2011)](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-01.pdf).}
#'   \item{\code{vessel_id}}{Vessel ID. ID number of the vessel used to collect data for that haul. The column 'vessel_id' is associated with the 'vessel_name' column. Note that it is possible for a vessel to have a new name but the same vessel id number. For a complete list of vessel ID codes, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{vessel_name}}{Vessel Name. Name of the vessel used to collect data for that haul. The column 'vessel_name' is associated with the 'vessel_id' column. Note that it is possible for a vessel to have a new name but the same vessel id number. For a complete list of vessel ID codes, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{weight_kg}}{Taxon Weight (kg). Weight (thousandths of a kilogram) of individuals in a haul by taxon.}
#'   \item{\code{worms}}{World Register of Marine Species Taxonomic Serial Number. Species code as identified in the World Register of Marine Species (WoRMS) (https://www.marinespecies.org/).}
#'   \item{\code{year}}{Year. Year the survey was conducted in.}
#'   \item{\code{area_swept_km2}}{Area Swept (km). The area the net covered while the net was fishing (kilometers squared), defined as the distance fished times the net width.}
#'   \item{\code{bottom_temperature_c}}{Bottom Temperature (Degrees Celsius). Bottom temperature (tenths of a degree Celsius); NA indicates removed or missing values.}
#'   \item{\code{common_name}}{Taxon Common Name. The common name of the marine organism associated with the 'scientific_name' and 'species_code' columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{count}}{Taxon Count. Total number of individuals caught in haul by taxon, represented in whole numbers.}
#'   \item{\code{cpue_kgkm2}}{Weight CPUE (kg/km<sup>2</sup>). Catch weight (kilograms) divided by area (squared kilometers) swept by the net.}
#'   \item{\code{cpue_nokm2}}{Number CPUE (no/km<sup>2</sup>). Catch number (in number of organisms) per area (squared kilometers) swept by the net.}
#'   \item{\code{cruise}}{Cruise ID. This is a six-digit number identifying the cruise number of the form: YYYY99 (where YYYY = year of the cruise; 99 = 2-digit number and is sequential; 01 denotes the first cruise that vessel made in this year, 02 is the second, etc.).}
#'   \item{\code{date_time}}{Date and Time of Haul. The date (MM/DD/YYYY) and time (HH:MM) of the beginning of the haul.}
#'   \item{\code{depth_m}}{Depth (m). Bottom depth (tenths of a meter).}
#'   \item{\code{distance_fished_km}}{Distance Fished (km). Distance the net fished (thousandths of kilometers).}
#'   \item{\code{duration_hr}}{Tow Duration (decimal hr). This is the elapsed time between start and end of a haul (decimal hours).}
#'   \item{\code{haul}}{Haul Number. This number uniquely identifies a sampling event (haul) within a cruise. It is a sequential number, in chronological order of occurrence.}
#'   \item{\code{hauljoin}}{Haul ID. This is a unique numeric identifier assigned to each (vessel, cruise, and haul) combination.}
#'   \item{\code{itis}}{ITIS Taxonomic Serial Number. Species code as identified in the Integrated Taxonomic Information System (https://itis.gov/).}
#'   \item{\code{latitude_dd_end}}{End Latitude (decimal degrees). Latitude (one hundred thousandth of a decimal degree) of the end of the haul.}
#'   \item{\code{latitude_dd_start}}{Start Latitude (decimal degrees). Latitude (one hundred thousandth of a decimal degree) of the start of the haul.}
#'   \item{\code{longitude_dd_end}}{End Longitude (decimal degrees). Longitude (one hundred thousandth of a decimal degree) of the end of the haul.}
#'   \item{\code{longitude_dd_start}}{Start Longitude (decimal degrees). Longitude (one hundred thousandth of a decimal degree) of the start of the haul.}
#'   \item{\code{net_height_m}}{Net Height (m). Measured or estimated distance (meters) between footrope and headrope of the trawl.}
#'   \item{\code{net_width_m}}{Net Width (m). Measured or estimated distance (meters) between wingtips of the trawl.}
#'   \item{\code{performance}}{Haul Performance Code. This denotes what, if any, issues arose during the haul. For more information, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}#'   }
#' @source https://github.com/afsc-gap-products/gap_public_data
#' @keywords species code data
#' @examples
#' data(public_data)
#' @details DETAILS
'public_data'
