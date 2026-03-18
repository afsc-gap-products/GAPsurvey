# Recover position data from Globe .log file

In the event that the MARPORT server GPS fails or is incomplete,
"convert_log_gps()" converts GLOBE LOG files into a format that can be
uploaded into WHEELHOUSE. To get a .log file that is usable in this
function, 1) Go the C:\\ globe\\ logs\\ 2018\\ directory and choose GLG
file with proper date 2) Use GLOBE Files\>Logs\> to convert .GLG
(binary) to a .LOG (.csv) file 3) convert_log_gps()will prompt you for
Vessel code, Cruise no., Haul no. and Date 4) The final prompt will ask
for the location of the GLOBE LOG file 5) convert_log_gps()will create
csv file in the R directory with filename "new.gps" 6) Rename "new.gps"
to HAULXXXX.GPS where XXXX is the haul number 7) Upload HAULXXXX.GPS
into WHEELHOUSE 8) NOTE: The raw GLOBE log data are in GMT time (-8 hrs
or 4PM AKDT prior day to 4PM current day. Hence if haul with missing GPS
spans the 4PM hour (e.g.,3:45-4:30 PM),YOU WILL HAVE TO CONVERT TWO GLG
files (current day and next day)and run convert_log_gps()twice &
manually combine the two GPS files 9) ALSO NOTE: You may have to shut
down GLOBE or wait until after 4pm on following day before all the
incoming NMEA data are written to the GLG file.

## Usage

``` r
convert_log_gps(
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
  DATE = NA,
  path_in,
  path_out = "./",
  filename_add = ""
)
```

## Arguments

- VESSEL:

  Optional. Default = NA. The vessel number (e.g., 94). If NA or not
  called in the function, a prompt will appear asking for this data.

- CRUISE:

  Optional. Default = NA. The cruise number, which is usually the year
  date (e.g., 201901). If NA or not called in the function, a prompt
  will appear asking for this data.

- HAUL:

  Optional. Default = NA. The haul number, aka the iterative number of
  this haul (e.g., 3). If NA or not called in the function, a prompt
  will appear asking for this data.

- DATE:

  Optional. Default = NA. The date in MM/DD/YYYY format (e.g.,
  "06/02/2019"). If NA or not called in the function, a prompt will
  appear asking for this data.

- path_in:

  Optional. Default = "./., or the local working directory but any path
  (as a string) may be entered.

- path_out:

  Optional. The default is the local working directory but may be
  specified with a string.

- filename_add:

  Optional. Default = "new". This string will be added to the name of
  the outputted file. Here, you can additional information that may make
  this file helpful to find later.

## Value

A .GPS file to the path_out directory with DATE/TIME in AKDT.

## Details

Now that you have a .log file, you can RUN the function by putting your
cursor on the "convert_log_gps()" line below & press CTRL+R.

## Examples

``` r
readLines(system.file("exdata/convert_log_gps/06062017.log",
  package = "GAPsurvey"))[1:5] # input file
#> [1] "$GPRMC,235958.000,A,5814.4411,N,15934.8091,W,9.45,11.19,050617,,,D*4E"        
#> [2] "$SDDBS,76.71,f,23.38,M,12.79,F"                                               
#> [3] "$GPGGA,235959.000,5814.4436,N,15934.8081,W,2,08,1.1,10.9,M,16.2,M,2.0,0000*53"
#> [4] "$GPGSA,M,3,25,29,02,31,12,06,14,51,,,,,1.8,1.1,1.4*3A"                        
#> [5] "$SDDBS,76.79,f,23.41,M,12.80,F"                                               
convert_log_gps(
    VESSEL = 94,
    CRUISE = 201901,
    HAUL = 3,
    DATE = "06/06/2017",
    path_in = system.file("exdata/convert_log_gps/06062017.log",
        package = "GAPsurvey"),
    path_out = getwd(),
    filename_add = "newlog")
#> Your new .gps files are saved to Z:/Projects/GAPsurvey_general/GAPsurvey/docs/reference/HAUL0003_newlog.gps
readLines(system.file("exdata/convert_log_gps/HAUL0003_newlog.gps",
  package = "GAPsurvey"))[1:5] # output file
#> [1] "94,201901,3,06/06/2017 15:59:58,5814.4411,-15934.8091"
#> [2] "94,201901,3,06/06/2017 15:59:59,5814.4436,-15934.8081"
#> [3] "94,201901,3,06/06/2017 16:00:00,5814.4462,-15934.8071"
#> [4] "94,201901,3,06/06/2017 16:00:01,5814.4487,-15934.8061"
#> [5] "94,201901,3,06/06/2017 16:00:02,5814.4513,-15934.8052"
```
