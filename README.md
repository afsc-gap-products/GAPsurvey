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

[![](https://img.shields.io/badge/devel%20version-2023.04.01-blue.svg)](https://github.com/afsc-gap-products/GAPsurvey)
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

    #> R version 4.2.3 (2023-03-15 ucrt)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows 10 x64 (build 19045)
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
    #> [1] badger_0.2.3  ggplot2_3.4.2 pkgdown_2.0.7 usethis_2.1.6 here_1.0.1   
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] pkgload_1.3.2       jsonlite_1.8.4      shiny_1.7.4         askpass_1.1         BiocManager_1.30.20
    #>  [6] rvcheck_0.2.1       yulab.utils_0.0.6   yaml_2.3.7          remotes_2.4.2       sessioninfo_1.2.2  
    #> [11] akgfmaps_2.3.1      pillar_1.9.0        glue_1.6.2          digest_0.6.31       RColorBrewer_1.1-3 
    #> [16] promises_1.2.0.1    snakecase_0.11.0    colorspace_2.1-0    htmltools_0.5.5     httpuv_1.6.9       
    #> [21] pkgconfig_2.0.3     devtools_2.4.5      gitcreds_0.1.2      purrr_1.0.1         xtable_1.8-4       
    #> [26] scales_1.2.1        processx_3.8.1      later_1.3.0         openssl_2.0.6       timechange_0.2.0   
    #> [31] tibble_3.2.1        proxy_0.4-27        generics_0.1.3      ellipsis_0.3.2      cachem_1.0.7       
    #> [36] withr_2.5.0         janitor_2.2.0       credentials_1.3.2   cli_3.6.1           magrittr_2.0.3     
    #> [41] crayon_1.5.2        mime_0.12           memoise_2.0.1       evaluate_0.20       ps_1.7.5           
    #> [46] fs_1.6.2            fansi_1.0.4         class_7.3-21        pkgbuild_1.4.0      httr2_0.2.2        
    #> [51] gh_1.4.0            profvis_0.3.7       tools_4.2.3         prettyunits_1.1.1   gert_1.9.2         
    #> [56] lifecycle_1.0.3     stringr_1.5.0       munsell_0.5.0       callr_3.7.3         compiler_4.2.3     
    #> [61] e1071_1.7-13        rlang_1.1.0         classInt_0.4-9      units_0.8-2         grid_4.2.3         
    #> [66] sys_3.4.1           rstudioapi_0.14     rappdirs_0.3.3      htmlwidgets_1.6.2   miniUI_0.1.1.1     
    #> [71] rmarkdown_2.21      gtable_0.3.3        curl_5.0.0          DBI_1.1.3           R6_2.5.1           
    #> [76] lubridate_1.9.2     knitr_1.42          dplyr_1.1.2         fastmap_1.1.1       utf8_1.2.3         
    #> [81] rprojroot_2.0.3     desc_1.4.2          KernSmooth_2.23-20  dlstats_0.1.6       stringi_1.7.12     
    #> [86] Rcpp_1.0.10         vctrs_0.6.2         sf_1.0-12           tidyselect_1.2.0    xfun_0.39          
    #> [91] urlchecker_1.0.1

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
