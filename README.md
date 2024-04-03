<!-- README.md is generated from README.Rmd. Please edit that file -->

# GAPsurvey <a href={https://afsc-gap-products.github.io/GAPsurvey}><img src="man/figures/logo.png" align="right" width=139 height=139 alt="logo with an image of a NOAA Fisheries report" />

> This code is always in development

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov; @EmilyMarkowitz-NOAA)

**Sean Rohan** (Sean.Rohan AT noaa.gov; @MargaretSiple-NOAA)

**Megsie Siple** (Margaret.Siple AT noaa.gov; @MargaretSiple-NOAA)

**Liz Dawson** (Liz.Dawson AT noaa.gov; @liz-dawson-NOAA)

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

### *At-sea data management tools for RACE GAP surveys*

[![](https://img.shields.io/badge/devel%20version-2024.04.01-blue.svg)](https://github.com/afsc-gap-products/GAPsurvey)
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

    #> R version 4.3.1 (2023-06-16 ucrt)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows 10 x64 (build 19045)
    #> 
    #> Matrix products: default
    #> 
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
    #> [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
    #> 
    #> time zone: America/Los_Angeles
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] remotes_2.5.0   badger_0.2.3    roxygen2_7.3.1  devtools_2.4.5  usethis_2.2.3   here_1.0.1      akgfmaps_3.5.0  terra_1.7-71   
    #>  [9] stars_0.6-4     abind_1.4-5     sf_1.0-16       gstat_2.1-1     ggplot2_3.5.0   classInt_0.4-10 RODBC_1.3-23    dplyr_1.1.4    
    #> [17] readr_2.1.5     magrittr_2.0.3 
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] DBI_1.2.2           httr2_1.0.1         s2_1.1.6            rlang_1.1.3         snakecase_0.11.1    e1071_1.7-14        compiler_4.3.1     
    #>  [8] callr_3.7.6         vctrs_0.6.5         stringr_1.5.1       profvis_0.3.8       crayon_1.5.2        pkgconfig_2.0.3     wk_0.9.1           
    #> [15] fastmap_1.1.1       ellipsis_0.3.2      utf8_1.2.4          promises_1.2.1      rmarkdown_2.26      sessioninfo_1.2.2   tzdb_0.4.0         
    #> [22] ps_1.7.6            purrr_1.0.2         xfun_0.43           gert_2.0.1          rvcheck_0.2.1       cachem_1.0.8        jsonlite_1.8.8     
    #> [29] later_1.3.2         parallel_4.3.1      R6_2.5.1            stringi_1.8.3       RColorBrewer_1.1-3  pkgload_1.3.4       lubridate_1.9.3    
    #> [36] Rcpp_1.0.12         knitr_1.45          zoo_1.8-12          gitcreds_0.1.2      dlstats_0.1.7       FNN_1.1.4           httpuv_1.6.15      
    #> [43] timechange_0.3.0    tidyselect_1.2.1    rstudioapi_0.16.0   yaml_2.3.8          codetools_0.2-19    miniUI_0.1.1.1      processx_3.8.4     
    #> [50] curl_5.2.1          pkgbuild_1.4.4      lattice_0.22-6      tibble_3.2.1        intervals_0.15.4    shiny_1.8.1.1       withr_3.0.0        
    #> [57] askpass_1.2.0       evaluate_0.23       desc_1.4.3          units_0.8-5         proxy_0.4-27        urlchecker_1.0.1    xts_0.13.2         
    #> [64] xml2_1.3.6          pillar_1.9.0        BiocManager_1.30.22 KernSmooth_2.23-22  generics_0.1.3      rprojroot_2.0.4     credentials_2.0.1  
    #> [71] sp_2.1-3            spacetime_1.3-1     hms_1.1.3           munsell_0.5.1       scales_1.3.0        xtable_1.8-4        class_7.3-22       
    #> [78] glue_1.7.0          janitor_2.2.0       tools_4.3.1         sys_3.4.2           fs_1.6.3            grid_4.3.1          gh_1.4.1           
    #> [85] colorspace_2.1-0    cli_3.6.2           rappdirs_0.3.3      fansi_1.0.6         gtable_0.3.4        yulab.utils_0.1.4   digest_0.6.35      
    #> [92] htmlwidgets_1.6.4   memoise_2.0.1       htmltools_0.5.8     pkgdown_2.0.7       lifecycle_1.0.4     mime_0.12           openssl_2.1.1

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
