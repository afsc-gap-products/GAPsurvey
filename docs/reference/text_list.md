# Takes a string of words and combines them into a sentance that lists them.

This function alows you to take a string of words and combine them into
a sentance list. For example, 'apples', 'oranges', 'pears' would become
'apples, oranges, and pears'. This function uses oxford commas.

## Usage

``` r
text_list(x, oxford = TRUE, sep = ",")
```

## Arguments

- x:

  Character strings you want in your string.

- oxford:

  T/F: would you like to use an oxford comma? Default = TRUE

- sep:

  string. default = "," but ";" might be what you need!

## Examples

``` r
text_list(c(1,2,"hello",4,"world",6))
#> [1] "1, 2, hello, 4, world, and 6"
```
