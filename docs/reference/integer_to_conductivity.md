# Convert SBE integer to conductivity

Convert SBE integer to conductivity

## Usage

``` r
integer_to_conductivity(
  conductivity_integer,
  temperature,
  pressure,
  condg,
  condh,
  condi,
  condj,
  cpcor,
  ctcor,
  par0 = 256,
  par1 = 1000,
  sig_figs = 6
)
```

## Arguments

- conductivity_integer:

  Conductivity voltage integer

- temperature:

  Temperature in degrees C

- pressure:

  Presssure in degrees C

- condg:

  Conductivity calibration parameter condg

- condh:

  Conductivity calibration parameter condh

- condi:

  Conductivity calibration parameter condi

- condj:

  Conductivity calibration parameter condj

- cpcor:

  Conductivity calibration parameter cpcor

- ctcor:

  Conductivity calibration parameter ctcor

- par0:

  Constant to convert integer to voltage

- par1:

  Constant to convert integer to voltage

- sig_figs:

  number of significant digits to use for conductivity (default = 6)

## Author

Sean Rohan
