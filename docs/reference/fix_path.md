# Make sure file path is complete

Function adds '/' or '\\ to the end of directories and recognizes when
there are file extensions at the end of strings.

## Usage

``` r
fix_path(path)
```

## Arguments

- path:

  A string with the complete path of the directory or file.

## Value

A fixed path string.

## Examples

``` r
fix_path("sdfg/sdfg/sdfg/dfg.dd")
#> [1] "sdfg/sdfg/sdfg/dfg.dd"
fix_path("sdfg/sdfg/sdfg")
#> [1] "sdfg/sdfg/sdfg/"
fix_path("sdfg/sdfg/sdfg/")
#> [1] "sdfg/sdfg/sdfg/"
```
