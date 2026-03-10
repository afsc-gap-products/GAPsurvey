# Convert SBE integer to pressure

Convert SBE integer to pressure

## Usage

``` r
integer_to_pressure(
  pressure_integer,
  tvoltage_integer,
  ptempa0,
  ptempa1,
  ptempa2,
  ptca0,
  ptca1,
  ptca2,
  ptcb0,
  ptcb1,
  ptcb2,
  pa0,
  pa1,
  pa2,
  par0 = 13107,
  sig_figs = 3,
  convert_to_dbar = TRUE
)
```

## Arguments

- pressure_integer:

  Pressure voltage integer

- tvoltage_integer:

  Temperature voltage integer

- ptempa0:

  Pressure calibration parameter ptempa0

- ptempa1:

  Pressure calibration parameter ptempa1

- ptempa2:

  Pressure calibration parameter ptempa2

- ptca0:

  Pressure calibration parameter ptca0

- ptca1:

  Pressure calibration parameter ptca1

- ptca2:

  Pressure calibration parameter ptca2

- ptcb0:

  Pressure calibration parameter ptcb0

- ptcb1:

  Pressure calibration parameter ptcb1

- ptcb2:

  Pressure calibration parameter ptcb2

- pa0:

  Pressure calibration parameter pa0

- pa1:

  Pressure calibration parameter pa1

- pa2:

  Pressure calibration parameter pa2

- par0:

  tvoltage_integer conversion constant

- sig_figs:

  number of significant digits to use for temperature (default = 3)

- convert_to_dbar:

  Should pressure be returned in or decibars (TRUE) or pounds per square
  inch without offset (FALSE)

## Author

Sean Rohan
