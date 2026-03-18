# Extract variable from xmlcon file text

Extract variable from xmlcon file text

## Usage

``` r
get_calibration_parameter(
  header,
  cal_par,
  start_block = NULL,
  end_block = NULL,
  make_numeric = TRUE
)
```

## Arguments

- header:

  Character vector of lines from an xmlcon file

- cal_par:

  Character vector of the xmlcon tag

- start_block:

  starting index for lines of the header to search

- end_block:

  ending index for lines of the leader to search

- make_numeric:

  Logical. Should the tag value be forced to a numeric?

## Author

Sean Rohan
