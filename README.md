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
    #> [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
    #> 
    #> time zone: America/Los_Angeles
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats4    stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] badger_0.2.3    readr_2.1.5     magrittr_2.0.3  akgfmaps_3.5.0  terra_1.7-71    stars_0.6-4     abind_1.4-5     gstat_2.1-1     classInt_0.4-10 gapctd_2.1.2    lubridate_1.9.3 plotly_4.10.4   sf_1.0-16       interp_1.1-6    bbmle_1.0.25.1  oce_1.8-2       gsw_1.1-1       ggplot2_3.5.0  
    #> [19] tidyr_1.3.1     dplyr_1.1.4     plyr_1.8.9      RODBC_1.3-23    roxygen2_7.3.1  devtools_2.4.5  usethis_2.2.3   here_1.0.1     
    #> 
    #> loaded via a namespace (and not attached):
    #>   [1] RColorBrewer_1.1-3  sys_3.4.2           rstudioapi_0.16.0   dlstats_0.1.7       jsonlite_1.8.8      rmarkdown_2.26      fs_1.6.3            vctrs_0.6.5         memoise_2.0.1       askpass_1.2.0       gh_1.4.1            janitor_2.2.0       htmltools_0.5.8     curl_5.2.1         
    #>  [15] KernSmooth_2.23-22  htmlwidgets_1.6.4   desc_1.4.3          httr2_1.0.1         zoo_1.8-12          cachem_1.0.8        mime_0.12           lifecycle_1.0.4     pkgconfig_2.0.3     Matrix_1.6-5        R6_2.5.1            fastmap_1.1.1       shiny_1.8.1.1       snakecase_0.11.1   
    #>  [29] digest_0.6.35       numDeriv_2016.8-1.1 colorspace_2.1-0    rprojroot_2.0.4     pkgload_1.3.4       fansi_1.0.6         timechange_0.3.0    httr_1.4.7          compiler_4.3.1      proxy_0.4-27        intervals_0.15.4    remotes_2.5.0       withr_3.0.0         DBI_1.2.2          
    #>  [43] pkgbuild_1.4.4      MASS_7.3-60.0.1     openssl_2.1.1       rappdirs_0.3.3      sessioninfo_1.2.2   tools_4.3.1         units_0.8-5         httpuv_1.6.15       glue_1.7.0          promises_1.2.1      grid_4.3.1          generics_0.1.3      gtable_0.3.4        tzdb_0.4.0         
    #>  [57] class_7.3-22        data.table_1.15.4   hms_1.1.3           sp_2.1-3            xml2_1.3.6          utf8_1.2.4          pillar_1.9.0        stringr_1.5.1       yulab.utils_0.1.4   later_1.3.2         lattice_0.22-6      FNN_1.1.4           deldir_2.0-4        tidyselect_1.2.1   
    #>  [71] rvcheck_0.2.1       miniUI_0.1.1.1      knitr_1.45          gitcreds_0.1.2      xfun_0.43           credentials_2.0.1   stringi_1.8.3       lazyeval_0.2.2      yaml_2.3.8          evaluate_0.23       codetools_0.2-19    tibble_3.2.1        BiocManager_1.30.22 cli_3.6.2          
    #>  [85] xtable_1.8-4        munsell_0.5.1       spacetime_1.3-1     Rcpp_1.0.12         gert_2.0.1          bdsmatrix_1.3-7     parallel_4.3.1      ellipsis_0.3.2      profvis_0.3.8       urlchecker_1.0.1    viridisLite_0.4.2   mvtnorm_1.2-4       scales_1.3.0        xts_0.13.2         
    #>  [99] e1071_1.7-14        purrr_1.0.2         crayon_1.5.2        rlang_1.1.3

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
