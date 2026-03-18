# Example got-to script

## Example Script Using R Package `GAPsurvey`

> Last updated April 2024

Open Rstudio script `example_script.R`. This should already be located
on desktop, otherwise find it using the link below.

``` r
system.file("r/example_script.R", package = "GAPsurvey")
#> [1] "C:/Users/emily.markowitz/AppData/Local/Temp/2/RtmpQjqQee/temp_libpath558858e5257c/GAPsurvey/r/example_script.R"
```

### Install R package

Rerun this only when there is a new version of the package to install.
The user may install from `GitHub`:

``` r
devtools::install_github("afsc-gap-products/GAPsurvey")
```

or install from local file `.tar.gz`:

``` r
# example, the user may have a different path
install.packages('C:/Users/User/Downloads/GAPsurvey_2023.04.04.tar.gz',
                 repos=NULL, type='source')
```

### Load libraries

``` r
library(GAPsurvey)
```

Now we can use functions from `GAPSurvey`!

### What have we historically caught at this station?

Learn more about and find examples using…

``` r
?get_catch_haul_history
```

``` r
get_catch_haul_history(
  years = 2021:2023, # optional; if you only want to see a specific year, not the last 10
  species_codes = c(21720, 21740), # optional; pacific cod and walleye pollock ONLY
  survey = "EBS", # for example
  station = "I-13") # for example
#> $catch
#> $catch$`2021`
#>   station     scientific_name     common_name count weight_kg cpue_kgkm2
#> 5    I-13 Gadus chalcogrammus walleye pollock   793   550.359 11529.7821
#> 6    I-13 Gadus macrocephalus     Pacific cod    91    31.140   652.3695
#>   cpue_nokm2
#> 5   16613.01
#> 6    1906.41
#> 
#> $catch$`2022`
#>   station     scientific_name     common_name count weight_kg cpue_kgkm2
#> 3    I-13 Gadus chalcogrammus walleye pollock   135   140.138   3252.886
#> 4    I-13 Gadus macrocephalus     Pacific cod   173   104.016   2414.421
#>   cpue_nokm2
#> 3   3133.623
#> 4   4015.679
#> 
#> $catch$`2023`
#>   station     scientific_name     common_name count weight_kg cpue_kgkm2
#> 1    I-13 Gadus chalcogrammus walleye pollock    70    41.744    898.023
#> 2    I-13 Gadus macrocephalus     Pacific cod    31     9.190    197.701
#>   cpue_nokm2
#> 1  1505.8837
#> 2   666.8913
#> 
#> 
#> $catch_means
#>       scientific_name     common_name station count weight_kg cpue_kgkm2
#> 1 Gadus chalcogrammus walleye pollock    I-13 332.7    244.08    5226.90
#> 2 Gadus macrocephalus     Pacific cod    I-13  98.3     48.12    1088.16
#>   cpue_nokm2 Freq
#> 1    7084.17    3
#> 2    2196.33    3
#> 
#> $haul
#>   year station haul stratum        vessel_name           date_time
#> 1 2021    I-13    7      31      ALASKA KNIGHT 2021-06-02 12:05:29
#> 2 2022    I-13   12      31        VESTERAALEN 2022-06-01 16:20:20
#> 3 2023    I-13   16      31 NORTHWEST EXPLORER 2023-05-30 13:43:02
#>   latitude_dd_start longitude_dd_start bottom_temperature_c
#> 1          57.65493          -160.2535                  4.4
#> 2          57.65502          -160.2754                  3.6
#> 3          57.66505          -160.2725                  3.4
#>   surface_temperature_c depth_m distance_fished_km net_width_m net_height_m
#> 1                   4.7      55              2.954      16.159        1.794
#> 2                   7.6      54              2.854      15.095        2.566
#> 3                   3.4      55              2.879      16.146        2.175
#>   area_swept_km2 duration_hr total_weight_kg
#> 1     0.04773369       0.522            1220
#> 2     0.04308113       0.531            1190
#> 3     0.04648433       0.516             812
```

### What time is sunrise and sunset?

Learn more about and find examples using…

``` r
?get_sunrise_sunset
```

``` r
get_sunrise_sunset(chosen_date = "2025-06-10",
                   survey = "AI",
                   station = "10402-8-55")
#> Using survey station (AI 10402-8-55) centroid location information (lat = 53.371, lon = 170.561) to calculate sunrise and sunset.
#> Sunrise is at 2025-06-10 08:10:00 AKDT
#> Sunset is at 2025-06-10 01:04:00 AKDT

get_sunrise_sunset(chosen_date = Sys.Date(),
                   survey = "GOA",
                   station = "264-264-19-511") 
#> Using survey station (GOA 264-264-19-511) centroid location information (lat = 52.369, lon = -169.988) to calculate sunrise and sunset.
#> Sunrise is at 2025-04-08 08:37:00 AKDT
#> Sunset is at 2025-04-07 22:05:00 AKDT

get_sunrise_sunset(chosen_date = "2025-08-04",
                   survey = "EBS",
                   station = "P-31")
#> Using survey station (EBS P-31) centroid location information (lat = 60, lon = -177.356) to calculate sunrise and sunset.
#> Sunrise is at 2025-08-04 07:38:00 AKDT
#> Sunset is at 2025-08-04 00:12:00 AKDT

get_sunrise_sunset(chosen_date = "2025-06-04",
                   survey = "NBS",
                   station = "ZZ-01")
#> Using survey station (NBS ZZ-01) centroid location information (lat = 63.334, lon = -168.244) to calculate sunrise and sunset.
#> Sunrise is at 2025-06-04 05:14:00 AKDT
#> Sunset is at 2025-06-04 01:08:00 AKDT

get_sunrise_sunset(chosen_date = "2025-08-04",
                   survey = NULL,
                   latitude = 60,
                   longitude = -162)
#> Using latitude and longitude to calcualte sunrise and sunset.
#> Sunrise is at 2025-08-04 06:36:00 AKDT
#> Sunset is at 2025-08-03 23:11:00 AKDT
```

### Convert CTD data to BTD as a backup for SBE39 (aka ‘the BT’)

``` r
?convert_ctd_btd
```

``` r
convert_ctd_btd(
  filepath_hex = system.file(paste0("exdata/convert_ctd_btd/",
                                    "SBE19plus_01908106_2023_06_18_0001.hex"),
                             package = "GAPsurvey"),
  filepath_xmlcon = system.file(paste0("exdata/convert_ctd_btd/",
                                       "SBE19plusV2_8106_ph_DO_leg2.xmlcon"),
                                package = "GAPsurvey"),
  VESSEL = 162,
  CRUISE = 202301,
  HAUL = 97,
  latitude = 59.01693, # Approximate - for depth estimation
  MODEL_NUMBER = "",
  VERSION_NUMBER = "",
  SERIAL_NUMBER = 8106)
```
