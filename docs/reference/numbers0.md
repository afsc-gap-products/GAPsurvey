# Make numbers the same length preceeded by 0s

Make numbers the same length preceeded by 0s

## Usage

``` r
numbers0(x, number_places = NA)
```

## Arguments

- x:

  a single or vector of values that need to be converted from something
  like 1 to "001"

- number_places:

  default = NA. If equal to NA, the function will take use the longest
  length of a value provided in x (example 1). If equal to a number, it
  will make sure that every number is the same length of number_places
  (example 2) or larger (if a value of x has more places than
  number_places(example 3)).

## Value

A string of the values in x preceeded by "0"s

## Examples

``` r
# example 1
numbers0(x = c(1,11,111))
#> [1] "001" "011" "111"
# example 2
numbers0(x = c(1,11,111), number_places = 4)
#> [1] "0001" "0011" "0111"
# example 3
numbers0(x = c(1,11,111), number_places = 2)
#> [1] "01"  "11"  "111"
```
