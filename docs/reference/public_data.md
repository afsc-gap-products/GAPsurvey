# Presence-only public data from FOSS

snapshot table for snapshot GAP_PRODUCTS.FOSS_CATCH

## Usage

``` r
data('public_data')
```

## Format

A data frame with 917401 observations on the following 37 variables.

- `date_time`:

  Date and time. The date (MM/DD/YYYY) and time (HH:MM) of the haul. All
  dates and times are in Alaska time (AKDT) of Anchorage, AK, USA
  (UTC/GMT -8 hours).

- `depth_m`:

  Depth (m). Bottom depth (meters).

- `distance_fished_km`:

  Distance fished (km). Distance the net fished (kilometers).

- `duration_hr`:

  Tow duration (decimal hr). This is the elapsed time between start and
  end of a haul (decimal hours).

- `haul`:

  Haul number. This number uniquely identifies a sampling event (haul)
  within a cruise. It is a sequential number, in chronological order of
  occurrence.

- `hauljoin`:

  Haul ID. This is a unique numeric identifier assigned to each (vessel,
  cruise, and haul) combination.

- `id_rank`:

  Lowest taxonomic rank. Lowest taxonomic rank of a given species entry.

- `itis`:

  Integrated taxonomic information system (ITIS) serial number. Species
  code as identified in the Integrated Taxonomic Information System
  (https://itis.gov/).

- `latitude_dd_end`:

  End latitude (decimal degrees). Latitude (one hundred thousandth of a
  decimal degree) of the end of the haul.

- `latitude_dd_start`:

  Start latitude (decimal degrees). Latitude (one hundred thousandth of
  a decimal degree) of the start of the haul.

- `longitude_dd_end`:

  End longitude (decimal degrees). Longitude (one hundred thousandth of
  a decimal degree) of the end of the haul.

- `longitude_dd_start`:

  Start longitude (decimal degrees). Longitude (one hundred thousandth
  of a decimal degree) of the start of the haul.

- `net_height_m`:

  Net height (m). Measured or estimated distance (meters) between
  footrope and headrope of the trawl.

- `net_width_m`:

  Net width (m). Measured or estimated distance (meters) between
  wingtips of the trawl.

- `performance`:

  Haul performance code. This denotes what, if any, issues arose during
  the haul. For more information, review the \[code
  books\](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

- `scientific_name`:

  Taxon scientific name. The scientific name of the organism associated
  with the common_name and species_code columns. For a complete taxon
  list, review the \[code
  books\](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

- `species_code`:

  Taxon code. The species code of the organism associated with the
  common_name and scientific_name columns. For a complete species list,
  review the \[code
  books\](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

- `srvy`:

  Survey abbreviation. Abbreviated survey names. The column srvy is
  associated with the survey and survey_definition_id columns. Northern
  Bering Sea (NBS), Southeastern Bering Sea (EBS), Bering Sea Slope
  (BSS), Gulf of Alaska (GOA), Aleutian Islands (AI).

- `station`:

  Station ID. Alpha-numeric designation for the station established in
  the design of a survey.

- `stratum`:

  Stratum ID. RACE database statistical area for analyzing data. Strata
  were designed using bathymetry and other geographic and
  habitat-related elements. The strata are unique to each survey region.
  Stratum of value 0 indicates experimental tows.

- `surface_temperature_c`:

  Surface temperature (degrees Celsius). Surface temperature (tenths of
  a degree Celsius); NA indicates removed or missing values.

- `survey`:

  Survey name. Name and description of survey. The column survey is
  associated with the srvy and survey_definition_id columns.

- `survey_definition_id`:

  Survey ID. The survey definition ID key code is an integer that
  uniquely identifies a survey region/survey design. The column
  survey_definition_id is associated with the srvy and survey columns.
  Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS
  and in the \[code
  books\](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

- `taxon_confidence`:

  Taxon confidence rating. Confidence in the ability of the survey team
  to correctly identify the taxon to the specified level, based solely
  on identification skill (e.g., not likelihood of a taxon being caught
  at that station on a location-by-location basis). Quality codes
  follow: \*\*High\*\*: High confidence and consistency. Taxonomy is
  stable and reliable at this level, and field identification
  characteristics are well known and reliable. \*\*Moderate\*\*:
  Moderate confidence. Taxonomy may be questionable at this level, or
  field identification characteristics may be variable and difficult to
  assess consistently. \*\*Low\*\*: Low confidence. Taxonomy is
  incompletely known, or reliable field identification characteristics
  are unknown. Documentation: \[Species identification confidence in the
  eastern Bering Sea shelf survey
  (1982-2008)\](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2009-04.pdf),
  \[Species identification confidence in the eastern Bering Sea slope
  survey
  (1976-2010)\](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-05.pdf),
  and \[Species identification confidence in the Gulf of Alaska and
  Aleutian Islands surveys
  (1980-2011)\](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-01.pdf).

- `vessel_id`:

  Vessel ID. ID number of the vessel used to collect data for that haul.
  The column vessel_id is associated with the vessel_name column. Note
  that it is possible for a vessel to have a new name but the same
  vessel id number. For a complete list of vessel ID key codes, review
  the \[code
  books\](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

- `vessel_name`:

  Vessel name. Name of the vessel used to collect data for that haul.
  The column vessel_name is associated with the vessel_id column. Note
  that it is possible for a vessel to have a new name but the same
  vessel id number. For a complete list of vessel ID key codes, review
  the \[code
  books\](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

- `weight_kg`:

  Sample or taxon weight (kg). Total weight (kilograms).

- `worms`:

  World register of marine species (WoRMS) taxonomic serial number.
  Species code as identified in the World Register of Marine Species
  (WoRMS) (https://www.marinespecies.org/).

- `year`:

  Survey year. Year the observation (survey) was collected.

- `area_swept_km2`:

  Area swept (km). The area the net covered while the net was fishing
  (kilometers squared), defined as the distance fished times the net
  width.

- `bottom_temperature_c`:

  Bottom temperature (degrees Celsius). Bottom temperature (tenths of a
  degree Celsius); NA indicates removed or missing values.

- `common_name`:

  Taxon common name. The common name of the marine organism associated
  with the scientific_name and species_code columns. For a complete
  species list, review the \[code
  books\](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

- `count`:

  Taxon count. Total whole number of individuals caught in haul or
  samples collected.

- `cpue_kgkm2`:

  Weight CPUE (kg/km2). Catch weight (kilograms) per unit effort (area
  swept by the net, units square kilometers).

- `cpue_nokm2`:

  Number CPUE (no/km2). Numerical catch per unit effort (area swept by
  the net, units square kilometers).

- `cruise`:

  Cruise Name. This is a six-digit integer identifying the cruise number
  of the form: YYYY99 (where YYYY = year of the cruise; 99 = 2-digit
  number and is sequential; 01 denotes the first cruise that vessel made
  in this year, 02 is the second, etc.).

- `cruisejoin`:

  Cruise ID. Unique integer ID assigned to each survey, vessel, and year
  combination.

## Source

https://github.com/afsc-gap-products/gap_products and
https://www.fisheries.noaa.gov/foss/f?p=215:28:14951401791129:::::

## Details

The Resource Assessment and Conservation Engineering (RACE) Division
Groundfish Assessment Program (GAP) of the Alaska Fisheries Science
Center (AFSC) conducts fisheries-independent bottom trawl surveys to
assess the populations of demersal fish and crab stocks of Alaska.

## Author

Emily Markowitz (Emily.Markowitz AT noaa.gov)

## Examples

``` r
data(public_data)
```
