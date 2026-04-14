# Get sunrise and sunset times by day, latitude, and longitude

Get sunrise and sunset times by day, latitude, and longitude

## Usage

``` r
get_sunrise_sunset(
  chosen_date,
  latitude = NULL,
  longitude = NULL,
  survey = NULL,
  station = NULL,
  verbose = FALSE,
  timezone = "US/Alaska"
)
```

## Arguments

- chosen_date:

  Date or charater. Formatted as "YYYY-MM-DD"

- latitude:

  Numeric. Fill in only if survey and station are not entered. latitude
  in either decimal degrees or a character latitude in degrees and
  decimal minutes

- longitude:

  Numeric. Fill in only if survey and station are not entered. Longitude
  in either decimal degrees or a character longitude in degrees and
  decimal minutes

- survey:

  Character. Fill in only if latitude and longitude are not entered. A
  character string of the survey you are interested in reivewing.
  Options are those from public_data\$survey, which are "AI", "GOA",
  "EBS", "NBS", "BSS".

- station:

  Character. Fill in only if latitude and longitude are not entered. A
  character string of the current station name (as a grid cell; e.g.,
  "264-85"). Stations defined in the station_coords dataset.

- verbose:

  Logical. Default = FALSE. If you would like a readout of what the file
  looks like in the console, set to TRUE.

- timezone:

  Character. Default = "US/Alaska." Other options include: "US/Aleutian"

## Value

Time of sunrise and sunset in text. Also shows a pop-up with sunrise and
sunset times.

## Examples

``` r
# Find times based on lat/lon for today's date, where date is a date object
get_sunrise_sunset(chosen_date = Sys.Date(),
                   latitude = 63.3,
                   longitude = -170.5)
#> Using latitude and longitude to calcualte sunrise and sunset. 
#> Sunrise is at 2026-04-14 07:56:00 AKDT
#> Sunset is at 2026-04-13 22:47:00 AKDT

# Find times based on lat/lon for today's date, where date is a character
# and lat/lon in degree decimal-minutes
get_sunrise_sunset(chosen_date = "2023-06-05",
                   latitude = "63 18.0",
                   longitude = "-170 30.0")
#> Using latitude and longitude to calcualte sunrise and sunset. 
#> Sunrise is at 2023-06-05 05:19:00 AKDT
#> Sunset is at 2023-06-05 01:13:00 AKDT

# Find times based on a survey (AI) station's recorded lat/lon for today's date
get_sunrise_sunset(chosen_date = "2025-06-10",
                   survey = "AI",
                   station = "8-55")
#> Using survey station (AI 8-55) centroid location information (lat = 53.371, lon = 170.561) to calculate sunrise and sunset. 
#> Sunrise is at 2025-06-10 08:10:00 AKDT
#> Sunset is at 2025-06-10 01:04:00 AKDT

# Find times based on a survey (GOA) station's recorded lat/lon for today's date
get_sunrise_sunset(chosen_date = Sys.Date(),
                   survey = "GOA",
                   station = "264-18-511")
#> Using survey station (GOA 264-18-511) centroid location information (lat = 52.351, lon = -169.964) to calculate sunrise and sunset. 
#> Sunrise is at 2026-04-14 08:24:00 AKDT
#> Sunset is at 2026-04-13 22:15:00 AKDT

# Find times based on a survey (EBS) station's recorded lat/lon for today's date
get_sunrise_sunset(chosen_date = "2025-08-04",
                   survey = "EBS",
                   station = "P-31")
#> Using survey station (EBS P-31) centroid location information (lat = 60, lon = -177.356) to calculate sunrise and sunset. 
#> Sunrise is at 2025-08-04 07:38:00 AKDT
#> Sunset is at 2025-08-04 00:12:00 AKDT

# Find times based on a survey (NBS) station's recorded lat/lon for today's date
get_sunrise_sunset(chosen_date = "2025-06-04",
                   survey = "NBS",
                   station = "ZZ-01")
#> Using survey station (NBS ZZ-01) centroid location information (lat = 63.334, lon = -168.244) to calculate sunrise and sunset. 
#> Sunrise is at 2025-06-04 05:14:00 AKDT
#> Sunset is at 2025-06-04 01:08:00 AKDT
```
