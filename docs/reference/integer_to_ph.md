# Convert SBE integer to pH

Convert SBE integer to pH

## Usage

``` r
integer_to_ph(
  ph_integer,
  ph_offset,
  ph_slope,
  temperature,
  sig_figs = 3,
  par0 = 13107
)
```

## Arguments

- ph_integer:

  pH voltage integer

- ph_offset:

  pH calibration parameter offset

- ph_slope:

  pH calibration parameter slope

- temperature:

  temperature in degrees C

- sig_figs:

  number of significant digits to use for conductivity (default = 3)

- par0:

  ph_integer conversion constant

## Author

Sean Rohan
