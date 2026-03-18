# Tau correction for dissolved oxygen voltage

Tau correction following Edwards et al. (2010).

## Usage

``` r
tau_par(temperature, pressure, tau20, d0, d1, d2)
```

## Arguments

- temperature:

  Temperature in degrees C

- pressure:

  Pressure in dbar

- tau20:

  Tau correction calibration parameter Tau20.

- d0:

  Tau correction calibration parameter D0.

- d1:

  Tau correction calibration parameter D1.

- d2:

  Tau correction calibration parameter D2.

## References

Edwards, B., Murphy, D., Janzen, C., Larson, A.N., 2010. Calibration,
response, and hysteresis in deep-sea dissolved oxygen measurements. J.
Atmos. Ocean. Technol. 27, 920–931.
https://doi.org/10.1175/2009JTECHO693.1

## Author

Sean Rohan
