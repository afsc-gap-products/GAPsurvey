# Convert SBE19plus V2 hex files to cnv

This function decodes hexadecimal-formatted Sea-Bird CTD files to cnv
files.

## Usage

``` r
hex_to_cnv(
  hex_path,
  output_path,
  xmlcon_path = NULL,
  sample_interval = 0.25,
  output_channels = NULL,
  output_sig_digits = NULL
)
```

## Arguments

- hex_path:

  Path to a .hex file

- output_path:

  Path to the output file location for a .cnv file

- xmlcon_path:

  Optional. Path to config file. Must be provided if .hex file does not
  contain configuration file parameters.

- sample_interval:

  Sampling interval for scans; 0.25 for a typical SBE19plus V2
  deployment.

- output_channels:

  Optional. Named vector of output channels and their names. Do not use
  unless outputs are are not the defaults.

- output_sig_digits:

  Optional. Significant digits after the decimal place for output
  channels. Only change if a subset of channels. Do not use unless
  outputs are are not the defaults.

## Author

Sean Rohan
