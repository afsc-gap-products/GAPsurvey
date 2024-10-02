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

    #> Warning: `git_branch_default()` was deprecated in usethis 2.1.0.
    #> ℹ Please use `git_default_branch()` instead.
    #> ℹ The deprecated feature was likely used in the badger package.
    #>   Please report the issue at <https://github.com/GuangchuangYu/badger/issues>.
    #> This warning is displayed once every 8 hours.
    #> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

[![](https://img.shields.io/badge/devel%20version-2024.04.05-blue.svg)](https://github.com/afsc-gap-products/GAPsurvey)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![](https://img.shields.io/github/last-commit/afsc-gap-products/GAPsurvey.svg)](https://github.com/afsc-gap-products/GAPsurvey/commits/main)

## Make sure the necessary packages are installed

    library(devtools)

    devtools::install_github("afsc-gap-products/GAPsurvey")
    # Or
    remotes::install_github("afsc-gap-products/GAPsurvey@main")

    library(GAPsurvey)

## Cite this data

Use the below [bibtext
citations](%22https://afsc-gap-products.github.io/GAPsurvey/blob/main/code/CITATION.bib%22)
for citing the package created and maintained in this repo. Add “note =
{Accessed: mm/dd/yyyy}” to append the day this data was accessed.

    #> @misc{GAPsurvey,
    #>   author = {{NOAA Fisheries Alaska Fisheries Science Center, Goundfish Assessment Program}},
    #>   year = {2024},
    #>   title = {AFSC Goundfish Assessment Program at-Sea data management tools for RACE GAP surveys},
    #>   howpublished = {https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys},
    #>   publisher = {{U.S. Dep. Commer.}},
    #>   copyright = {Public Domain}
    #> }

## Metadata

This package was last produced using:

    #> R version 4.4.1 (2024-06-14 ucrt)
    #> Platform: x86_64-w64-mingw32/x64
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
    #> [1] stats4    stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] badger_0.2.4    gapindex_2.2.0  pkgdown_2.1.0   gapctd_2.1.6    lubridate_1.9.3 plotly_4.10.4   interp_1.1-6   
    #>  [8] bbmle_1.0.25.1  oce_1.8-3       gsw_1.2-0       tidyr_1.3.1     plyr_1.8.9      remotes_2.5.0   roxygen2_7.3.2 
    #> [15] devtools_2.4.5  usethis_3.0.0   here_1.0.1      akgfmaps_3.5.3  terra_1.7-78    stars_0.6-6     abind_1.4-8    
    #> [22] sf_1.0-17       gstat_2.1-2     ggplot2_3.5.1   classInt_0.4-10 RODBC_1.3-23    dplyr_1.1.4     readr_2.1.5    
    #> [29] magrittr_2.0.3 
    #> 
    #> loaded via a namespace (and not attached):
    #>   [1] RColorBrewer_1.1-3  sys_3.4.2           rstudioapi_0.16.0   dlstats_0.1.7       jsonlite_1.8.8     
    #>   [6] rmarkdown_2.28      fs_1.6.4            vctrs_0.6.5         memoise_2.0.1       askpass_1.2.0      
    #>  [11] gh_1.4.1            janitor_2.2.0       htmltools_0.5.8.1   curl_5.2.2          KernSmooth_2.23-24 
    #>  [16] htmlwidgets_1.6.4   desc_1.4.3          httr2_1.0.4         zoo_1.8-12          cachem_1.1.0       
    #>  [21] mime_0.12           lifecycle_1.0.4     pkgconfig_2.0.3     Matrix_1.7-0        R6_2.5.1           
    #>  [26] fastmap_1.2.0       shiny_1.9.1         snakecase_0.11.1    digest_0.6.37       numDeriv_2016.8-1.1
    #>  [31] colorspace_2.1-1    ps_1.8.0            rprojroot_2.0.4     pkgload_1.4.0       fansi_1.0.6        
    #>  [36] timechange_0.3.0    httr_1.4.7          compiler_4.4.1      proxy_0.4-27        intervals_0.15.5   
    #>  [41] withr_3.0.1         DBI_1.2.3           pkgbuild_1.4.4      MASS_7.3-60.2       openssl_2.2.1      
    #>  [46] rappdirs_0.3.3      sessioninfo_1.2.2   tools_4.4.1         units_0.8-5         httpuv_1.6.15      
    #>  [51] glue_1.7.0          callr_3.7.6         promises_1.3.0      grid_4.4.1          generics_0.1.3     
    #>  [56] gtable_0.3.5        tzdb_0.4.0          class_7.3-22        data.table_1.16.0   hms_1.1.3          
    #>  [61] sp_2.1-4            xml2_1.3.6          utf8_1.2.4          pillar_1.9.0        stringr_1.5.1      
    #>  [66] yulab.utils_0.1.7   later_1.3.2         lattice_0.22-6      FNN_1.1.4           deldir_2.0-4       
    #>  [71] tidyselect_1.2.1    rvcheck_0.2.1       miniUI_0.1.1.1      knitr_1.48          gitcreds_0.1.2     
    #>  [76] xfun_0.47           credentials_2.0.1   stringi_1.8.4       lazyeval_0.2.2      yaml_2.3.10        
    #>  [81] evaluate_0.24.0     codetools_0.2-20    tibble_3.2.1        BiocManager_1.30.25 cli_3.6.3          
    #>  [86] xtable_1.8-4        munsell_0.5.1       processx_3.8.4      spacetime_1.3-2     Rcpp_1.0.13        
    #>  [91] gert_2.1.1          bdsmatrix_1.3-7     parallel_4.4.1      ellipsis_0.3.2      profvis_0.3.8      
    #>  [96] urlchecker_1.0.1    viridisLite_0.4.2   mvtnorm_1.3-1       scales_1.3.0        xts_0.14.0         
    #> [101] e1071_1.7-14        crayon_1.5.3        purrr_1.0.2         rlang_1.1.4

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
