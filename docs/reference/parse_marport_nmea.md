# Parse Marport NMEA Logs into wide format data frames

Processes a vector of raw NMEA 0183 strings, specifically targeting
Marport sensor data. It groups sentences by time (`$GPZDA`), extracts
spatial coordinates (`$GPGGA`), and extracts Marport sensor values
(`$MPMSD`) to create a data.frame.

## Usage

``` r
parse_marport_nmea(nmea_strings)
```

## Arguments

- nmea_strings:

  A character vector containing raw NMEA sentences (e.g., starting with
  `$GPZDA`, `$GPGGA`, or `$MPMSD`).

## Value

A data frame in "wide" format where each row represents a unique
timestamp and location. Columns include:

- `DATE_TIME`: POSIXct UTC timestamp.

- `LATITUDE` / `LONGITUDE`: Decimal coordinates.

- Sensor columns: `DEPTH`, `TEMPERATURE`, `NET_HEIGHT`, `DOOR_SPREAD`,
  and `NET_SPREAD` (depending on data availability).

## Examples

``` r
test_strings <- c(
  "$GPZDA,204954.000,24,02,2026,,*5A",
  "$GPGGA,204954.000,4741.1526,N,12215.0957,W,1,07,1.7,35.8,M,-17.2,M,,0000*5E",
  "$MPMSD,T,PW,18,XST,m,127.64*37",
  "$MPMSD,T,PD,23,XST,m,1.13*29",
  "$MPMSD,T,HR,11,DTB,m,2.40*2E",
  "$MPMSD,T,HR,11,DPT,m,0.65*39",
  "$MPMSD,T,HR,11,TMP,c,14.53*0E",
  "$GPZDA,204955.000,24,02,2026,,*5A",
  "$GPGGA,204955.000,4741.1526,N,12215.0957,W,1,07,1.7,35.8,M,-17.2,M,,0000*5E"
)

parse_marport_nmea(test_strings)
#> Warning: there are records with missing times, which will be dropped.
#>             DATE_TIME   LATITUDE   LONGITUDE NET_SPREAD DOOR_SPREAD NET_HEIGHT
#> 1 2026-02-24 20:49:54 47.6858767 -122.251595     127.64        1.13        2.4
#> 2 2026-02-24 20:49:55 47.6858767 -122.251595         NA          NA         NA
#>   DEPTH TEMPERATURE
#> 1  0.65       14.53
#> 2    NA          NA
```
