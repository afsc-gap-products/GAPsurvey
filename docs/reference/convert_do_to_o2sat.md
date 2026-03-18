# Calculate oxygen saturation (percent) from dissolved oxygen (ml/l)

Dissolved oxygen divided by oxygen saturation calculated following
Garcia and Gordon (1992)

## Usage

``` r
convert_do_to_o2sat(oxygen, temperature, salinity)
```

## Arguments

- oxygen:

  Dissolved oxygen in ml/l

- temperature:

  Temperature (IPTS-68, degrees Celsius).

- salinity:

  Salinity (PSU, PSS-78).

## References

Garcia, H.E., Gordon, L.I., 1992. Oxygen solubility in seawater: Better
fitting equations. Limnol. Oceanogr. 37, 1307–1312.
https://doi.org/10.4319/lo.1992.37.6.1307

## Author

Sean Rohan
