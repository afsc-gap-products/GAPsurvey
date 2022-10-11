<!-- README.md is generated from README.Rmd. Please edit that file -->

# GAPsurvey <a href={https://afsc-gap-products.github.io/GAPsurvey}><img src="man/figures/logo.png" align="right" width=139 height=139 alt="logo with an image of a NOAA Fisheries report" />

> This code is always in development

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov; @EmilyMarkowitz-NOAA)

**Megsie Siple** (Margaret.Siple AT noaa.gov; @MargaretSiple-NOAA)

**Liz Dawson** (Liz.Dawson AT noaa.gov; @liz-dawson-NOAA)

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

### *At-sea data management tools for RACE GAP surveys*

[![](https://img.shields.io/badge/devel%20version-2022.10.01-blue.svg)](https://github.com/afsc-gap-products/GAPsurvey)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![](https://img.shields.io/github/last-commit/afsc-gap-products/GAPsurvey.svg)](https://github.com/afsc-gap-products/GAPsurvey/commits/main)

## Make sure the necessary packages are installed

    library(devtools)

    devtools::install_github("afsc-gap-products/GAPsurvey")
    library(GAPsurvey)
    # Alternatively
    remotes::install_github("afsc-gap-products/GAPsurvey@main")

## Metadata

This package was last produced using:

    #> R version 4.2.0 (2022-04-22 ucrt)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows 10 x64 (build 19044)
    #> 
    #> Matrix products: default
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
    #> [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
    #> [5] LC_TIME=English_United States.utf8    
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] pkgdown_2.0.6 usethis_2.1.6 here_1.0.1    badger_0.2.1  ggplot2_3.3.6
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] Rcpp_1.0.9          gert_1.9.1          gitcreds_0.1.2      prettyunits_1.1.1  
    #>  [5] ps_1.7.1            assertthat_0.2.1    rprojroot_2.0.3     digest_0.6.29      
    #>  [9] utf8_1.2.2          mime_0.12           R6_2.5.1            sys_3.4            
    #> [13] evaluate_0.17       httr_1.4.4          pillar_1.8.1        yulab.utils_0.0.5  
    #> [17] rlang_1.0.6         curl_4.3.3          rstudioapi_0.14     miniUI_0.1.1.1     
    #> [21] callr_3.7.2         urlchecker_1.0.1    rmarkdown_2.17      desc_1.4.2         
    #> [25] devtools_2.4.4      stringr_1.4.1       htmlwidgets_1.5.4   dlstats_0.1.5      
    #> [29] munsell_0.5.0       shiny_1.7.2         compiler_4.2.0      httpuv_1.6.6       
    #> [33] xfun_0.33           askpass_1.1         pkgconfig_2.0.3     pkgbuild_1.3.1     
    #> [37] htmltools_0.5.3     openssl_2.0.3       tidyselect_1.2.0    tibble_3.1.8       
    #> [41] fansi_1.0.3         crayon_1.5.2        dplyr_1.0.10        withr_2.5.0        
    #> [45] later_1.3.0         grid_4.2.0          jsonlite_1.8.2      xtable_1.8-4       
    #> [49] gtable_0.3.1        lifecycle_1.0.3     DBI_1.1.3           magrittr_2.0.3     
    #> [53] credentials_1.3.2   scales_1.2.1        cli_3.4.1           stringi_1.7.8      
    #> [57] cachem_1.0.6        fs_1.5.2            promises_1.2.0.1    remotes_2.4.2      
    #> [61] ellipsis_0.3.2      rvcheck_0.2.1       generics_0.1.3      vctrs_0.4.2        
    #> [65] gh_1.3.1            RColorBrewer_1.1-3  tools_4.2.0         glue_1.6.2         
    #> [69] purrr_0.3.5         processx_3.7.0      pkgload_1.3.0       fastmap_1.1.0      
    #> [73] yaml_2.3.5          colorspace_2.0-3    BiocManager_1.30.18 sessioninfo_1.2.2  
    #> [77] memoise_2.0.1       knitr_1.40          profvis_0.3.7

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
