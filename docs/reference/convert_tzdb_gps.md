# Read data from a TimeZero database and convert to a .gps file

This function retrieves time and position data from a TimeZero database
and converts it to a .gps file format.

## Usage

``` r
convert_tzdb_gps(
  path_tzdb = NULL,
  output_file = NULL,
  vessel,
  cruise,
  haul,
  start = NULL,
  end = NULL
)
```

## Arguments

- path_tzdb:

  A specific path to the TimeZero database file (.tzdb).

- output_file:

  Output file path as a character vector.

- vessel:

  RACE vessel number

- cruise:

  RACE cruise number

- haul:

  RACE haul number

- start:

  Optional start time as character vector POSIXct object for filtering
  data from the TimeZero database.

- end:

  Optional end time as character vector or POSIXct object for filtering
  ddata from the TimeZero database.

## Value

A data frame containing the columns "Date", "X", and "Y" from the
Timezero database.

## Author

Alex Dowlin and Sean Rohan

## Examples
