# Extract depth and temperature from NMEA strings

Convert Marport Trawl Explorer NMEA strings to BTD and BTH files. Called
internally by convert_bvdr_marp().

## Usage

``` r
convert_nmea_btd(
  nmea_strings = NULL,
  interactive_editing = TRUE,
  min_depth = -0.1,
  max_depth = 800,
  min_temperature = -2,
  max_temperature = 20,
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
  MODEL_NUMBER = "Marport TE",
  VERSION_NUMBER = NA,
  SERIAL_NUMBER = NA,
  ...
)
```

## Arguments

- nmea_strings:

  Character vector of NMEA strings (e.g., from a .bvdr file) or a path
  to a .marp file.

- interactive_editing:

  Should the interactive point removal interface be used to manually
  clean temperature and depth data? If TRUE, must have graphic devices
  set to view plots in actual size (in R Studio: View \> Actual Size or
  Ctrl+0)

- min_depth:

  Optional (default = -0.1). Minimum valid depth (m).

- max_depth:

  Optional (default = 1000). Maximum valid depth (m).

- min_temperature:

  Optional (default = -2). Maximum valid temperature (Celsius).

- max_temperature:

  Optional (default = 20). Maximum valid temperature (Celsius).

- VESSEL:

  Optional. Default = NA. The vessel number (e.g., 162 for AK Knight, 94
  for Vesteraalen). If NA or not called in the function, a prompt will
  appear asking for this data.

- CRUISE:

  Optional. Default = NA. The cruise number, which is usually the year +
  sequential two digit cruise (e.g., 202101). If NA or not called in the
  function, a prompt will appear asking for this data.

- HAUL:

  Optional. Default = NA. The haul number that you are trying to convert
  data for (e.g., 3). If NA or not called in the function, a prompt will
  appear asking for this data.

- MODEL_NUMBER:

  Optional. Default = "Marport TE". The model name/number of the Marport
  sensor (e.g., 123 or 999, you can put in NA or a dummy number here
  instead of the actual model number without any negative
  repercussions). This field may have restrictions on length.

- VERSION_NUMBER:

  Optional. Default = NA. The version number of the Marport sensor
  (e.g., 123 or 999, you can put in NA or a dummy number here instead of
  the actual version number without any negative repercussions).

- SERIAL_NUMBER:

  Optional. Default = NA. The serial number of the Marport sensor (e.g.,
  123 or 999, you can put in NA or a dummy number here instead of the
  actual serial number without any negative repercussions).

- ...:

  additional arguments

## Author

Sean Rohan \<sean.rohan@noaa.gov\>

## Examples

``` r
if (FALSE) { # \dontrun{
# Run this to select Marport (.marp files)
convert_nmea_btd()
} # }
```
