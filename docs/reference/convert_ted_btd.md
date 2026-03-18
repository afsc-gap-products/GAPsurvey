# BVDR Conversion to Create BTD data

Converts Marport BVDR data (.ted and .tet files from Marport headrope
sensor) to .BTD format. You must first run the BVDR converter program
(BVDRReader.exe) to convert the Marport .bvdr files into .ted and .tet
files that can be pulled into R. The BVDR program and instructions can
be found in the RACE Survey App (NEW BVDR README.txt). You will have to
create your own .SGT file using the example in the BVDR instruction file
with start and end time (be sure to include a carriage return after your
(second and) final row of data!), because this is not a file that our
current systems creates. Once you have used the BVDR converter to output
the .ted and .tet files you are ready to use the convert_ted_btd()
function here!

## Usage

``` r
convert_ted_btd(
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
  MODEL_NUMBER = NA,
  VERSION_NUMBER = NA,
  SERIAL_NUMBER = NA,
  path_in = "C:/Program Files/Marport Server/Logs/",
  path_out = "./",
  filename_add = "new"
)
```

## Arguments

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

  Optional. Default = NA. The model number of the Marport sensor (e.g.,
  123 or 999, you can put in NA or a dummy number here instead of the
  actual model number without any negative repercussions).

- VERSION_NUMBER:

  Optional. Default = NA. The version number of the Marport sensor
  (e.g., 123 or 999, you can put in NA or a dummy number here instead of
  the actual version number without any negative repercussions).

- SERIAL_NUMBER:

  Optional. Default = NA. The serial number of the Marport sensor (e.g.,
  123 or 999, you can put in NA or a dummy number here instead of the
  actual serial number without any negative repercussions).

- path_in:

  Optional. The default is the location on the catch computer
  ("C:/Program Files/Marport Server/Logs/") but any path can be entered.

- path_out:

  Optional. The default is the local working directory but can be
  specified with a string.

- filename_add:

  Optional. Default = "new". This string will be added to the name of
  the outputed file. Here, you can additional information that may make
  this file helpful to find later.

## Value

.BTH and .BTD files to the path_out directory.

## Examples

``` r
# input files
readLines(system.file("exdata/convert_bvdr_btd/201901_94_0003.ted",
  package = "GAPsurvey"))[1:5]
#> [1] "94,201901,3,6/3/2019 11:47:08,1,51,m"
#> [2] "94,201901,3,6/3/2019 11:47:09,1,51,m"
#> [3] "94,201901,3,6/3/2019 11:47:10,1,51,m"
#> [4] "94,201901,3,6/3/2019 11:47:11,1,51,m"
#> [5] "94,201901,3,6/3/2019 11:47:13,1,51,m"
readLines(system.file("exdata/convert_bvdr_btd/201901_94_0003.tet",
  package = "GAPsurvey"))[1:5]
#> [1] "94,201901,3,6/3/2019 10:49:47,1,8.8,C"
#> [2] "94,201901,3,6/3/2019 10:49:37,1,8.7,C"
#> [3] "94,201901,3,6/3/2019 10:49:45,1,8.7,C"
#> [4] "94,201901,3,6/3/2019 10:49:46,1,8.7,C"
#> [5] "94,201901,3,6/3/2019 10:49:48,1,8.7,C"
readLines(system.file("exdata/convert_bvdr_btd/201901_94_0003.teh",
  package = "GAPsurvey"))[1:5]
#> [1] "94,201901,3,6/3/2019 10:02:27,23,4.9"
#> [2] "94,201901,3,6/3/2019 10:05:58,23,4.9"
#> [3] "94,201901,3,6/3/2019 10:10:33,23,4.9"
#> [4] "94,201901,3,6/3/2019 10:13:29,23,4.8"
#> [5] "94,201901,3,6/3/2019 10:17:47,23,4.8"
#' run function
convert_ted_btd(
   VESSEL = 94,
   CRUISE = 201901,
   HAUL = 3,
   MODEL_NUMBER = 123,
   VERSION_NUMBER = 456,
   SERIAL_NUMBER = 789,
   path_in = system.file("exdata/convert_bvdr_btd/", package = "GAPsurvey"),
   path_out = getwd(),
   filename_add = "newted")
#> Your new Z:/Projects/GAPsurvey_general/GAPsurvey/docs/reference/HAUL0003_newted .BTD and .BTH files are saved.
# output files
readLines(system.file("exdata/convert_bvdr_btd/HAUL0003_newted.BTD",
  package = "GAPsurvey"))[1:5]
#> [1] "VESSEL,CRUISE,HAUL,SERIAL_NUMBER,DATE_TIME,TEMPERATURE,DEPTH,"
#> [2] "94,201901,3,789,6/3/2019 10:49:20,8.5,0,"                     
#> [3] "94,201901,3,789,6/3/2019 10:49:23,8.5,,"                      
#> [4] "94,201901,3,789,6/3/2019 10:49:25,8.5,,"                      
#> [5] "94,201901,3,789,6/3/2019 10:49:26,8.5,,"                      
readLines(system.file("exdata/convert_bvdr_btd/HAUL0003_newted.BTH",
  package = "GAPsurvey"))[1:5]
#> [1] "VESSEL,CRUISE,HAUL,MODEL_NUMBER,VERSION_NUMBER,SERIAL_NUMBER,HOST_TIME,LOGGER_TIME,LOGGING_START,LOGGING_END,SAMPLE_PERIOD,NUMBER_CHANNELS,NUMBER_SAMPLES,MODE"
#> [2] "94,201901,3,123,456,789,06/03/19 11:59:59,06/03/19 11:59:59,06/03/19 10:49:20,06/03/19 11:59:59,3,2,0,2"                                                       
#> [3] NA                                                                                                                                                              
#> [4] NA                                                                                                                                                              
#> [5] NA                                                                                                                                                              
```
