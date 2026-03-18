# Station centroid locations for each station from akgfmaps

Station centroid coordinates for each station for all surveys, as
defined by the akgfmaps package.

## Usage

``` r
data('station_coords')
```

## Format

A data frame with 32858 observations on the following 7 variables.

- `design_year`:

  Design year. Year ID associated with a given value AREA_ID. This field
  describes the changes in the survey design over time.

- `latitude_dd`:

  Latitude (decimal degrees). Latitude (one hundred thousandth of a
  decimal degree).

- `longitude_dd`:

  Longitude (decimal degrees). Longitude (one hundred thousandth of a
  decimal degree).

- `srvy`:

  Survey abbreviation. Abbreviated survey names. The column srvy is
  associated with the survey and survey_definition_id columns. Northern
  Bering Sea (NBS), Southeastern Bering Sea (EBS), Bering Sea Slope
  (BSS), Gulf of Alaska (GOA), Aleutian Islands (AI).

- `station`:

  Station ID. Alpha-numeric designation for the station established in
  the design of a survey.

- `survey_definition_id`:

  Survey ID. The survey definition ID key code is an integer that
  uniquely identifies a survey region/survey design. The column
  survey_definition_id is associated with the srvy and survey columns.
  Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS
  and in the \[code
  books\](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

- `geometry`:

  Spatial geometry. Spatial geometry information (like points, lines, or
  polygons) a feature.

## Source

https://github.com/afsc-gap-products/akgfmaps

## Details

Find code to create this table in ./inst/run.R

## Author

Sean Rohan (sean.rohan AT noaa.gov)

## Examples

``` r
data(station_coords)
```
