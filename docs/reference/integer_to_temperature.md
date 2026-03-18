# Convert SBE integer to ITS-90 temperature

Convert SBE integer to ITS-90 temperature

## Usage

``` r
integer_to_temperature(
  temperature_integer,
  sig_figs = NULL,
  a0,
  a1,
  a2,
  a3,
  offset = 0,
  par0 = 524288,
  par1 = 1.6e+07,
  par2 = 2.9e+09,
  par3 = 102400000,
  par4 = 20480,
  par5 = 2e+05
)
```

## Arguments

- temperature_integer:

  Temperature voltage integer

- sig_figs:

  number of significant digits to use for temperature

- a0:

  Temperature calibration parameter a0

- a1:

  Temperature Calibration parameter a1

- a2:

  Temperature calibration parameter a2

- a3:

  Temperature calibration parameter a3

- offset:

  Temperature calibration parameter offset

- par0:

  Temperature calibration parameter par0

- par1:

  Temperature calibration parameter par1

- par2:

  Temperature calibration parameter par2

- par3:

  Temperature calibration parameter par3

- par4:

  Temperature calibration parameter par4

- par5:

  Temperature calibration parameter par5

## Author

Sean Rohan
