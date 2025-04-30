<!-- README.md is generated from README.Rmd. Please edit that file -->

# GAPsurvey <a href={https://afsc-gap-products.github.io/GAPsurvey}><img src="man/figures/logo.png" align="right" width=139 height=139 alt="logo."/>

*At-sea data management tools for RACE GAP surveys*

> This code is always in development

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov; @EmilyMarkowitz-NOAA)

**Sean Rohan** (Sean.Rohan AT noaa.gov; @sean-rohan-NOAA)

**Margaret Siple** (Margaret Siple AT noaa.gov; @MargaretSiple-NOAA)

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

## Table of contents

> -   [*Make sure the necessary packages are
>     installed*](#make-sure-the-necessary-packages-are-installed)
> -   [*example, the user may have a different
>     path*](#example,-the-user-may-have-a-different-path)
>     -   [*User Resources*](#user-resources)
>     -   [*Cite this data*](#cite-this-data)
> -   [*Relevant publications*](#relevant-publications)
> -   [*Suggestions and Comments*](#suggestions-and-comments)
>     -   [*R Version Metadata*](#r-version-metadata)
>     -   [*NOAA README*](#noaa-readme)
>     -   [*NOAA License*](#noaa-license)

## Make sure the necessary packages are installed

    library(devtools)

    devtools::install_github("afsc-gap-products/GAPsurvey")
     # Or
    remotes::install_github("afsc-gap-products/GAPsurvey@main")

    library(GAPsurvey)

or install from local file `.tar.gz`:

    # example, the user may have a different path
    install.packages('C:/Users/User/Downloads/GAPsurvey_2025.04.30.tar.gz',
                     repos=NULL, type='source')
    library(GAPsurvey)

## User Resources

-   [GitHub
    repository](https://github.com/afsc-gap-products/gap_products).

-   [Access Tips and Documentation for All Production
    Data](https://afsc-gap-products.github.io/gap_products/)

-   [Fisheries One Stop Shop
    (FOSS)](https://www.fisheries.noaa.gov/foss)

-   [Groundfish Assessment Program Bottom Trawl
    Surveys](https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys)

-   [AFSC’s Resource Assessment and Conservation Engineering
    Division](https://www.fisheries.noaa.gov/about/resource-assessment-and-conservation-engineering-division)

-   [Survey code
    books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual)

-   [Publications and Data
    Reports](https://repository.library.noaa.gov/)

-   [Research Surveys conducted at
    AFSC](https://www.fisheries.noaa.gov/alaska/ecosystems/alaska-fish-research-surveys)

## Cite this data

Use the below [bibtext
citations](%22https://afsc-gap-products.github.io/GAPsurvey/blob/main/code/CITATION.bib%22)
for citing the package created and maintained in this repo. Add “note =
{Accessed: mm/dd/yyyy}” to append the day this data was accessed.

    cat(readLines(con = here::here("inst/CITATION.bib")), sep = "\n") 
    #> @misc{GAPsurvey,
    #>   author = {{NOAA Fisheries Alaska Fisheries Science Center, Goundfish Assessment Program}},
    #>   year = {2024},
    #>   title = {AFSC Goundfish Assessment Program at-Sea data management tools for RACE GAP surveys},
    #>   howpublished = {https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys},
    #>   publisher = {{U.S. Dep. Commer.}},
    #>   copyright = {Public Domain}
    #> }

# Relevant publications

    source("https://raw.githubusercontent.com/afsc-gap-products/citations/main/cite/current_data_tm.r") # srvy_cite 

**Learn more about these surveys** \[@2023NEBS; @2023NEBS; @GOA2023;
@AI2022; @RN979; @SAPcrab2024\].

# Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/afsc-gap-products/GAPsurvey/pulls), [submit
an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/afsc-gap-products/GAPsurvey/issues).

## R Version Metadata

    sessionInfo()
    #> R version 4.4.3 (2025-02-28 ucrt)
    #> Platform: x86_64-w64-mingw32/x64
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
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] ggplot2_3.5.2
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] tidyselect_1.2.1   dplyr_1.1.4        farver_2.1.2       fastmap_1.2.0      gapindex_3.0.2     janitor_2.2.1      akgfmaps_4.0.4     promises_1.3.2     digest_0.6.37      timechange_0.3.0   mime_0.13         
    #> [12] lifecycle_1.0.4    sf_1.0-20          ellipsis_0.3.2     terra_1.8-42       magrittr_2.0.3     compiler_4.4.3     rlang_1.1.5        RODBC_1.3-26       tools_4.4.3        yaml_2.3.10        data.table_1.17.0 
    #> [23] knitr_1.50         FNN_1.1.4.1        htmlwidgets_1.6.4  sp_2.2-0           pkgbuild_1.4.7     classInt_0.4-11    here_1.0.1         RColorBrewer_1.1-3 pkgload_1.4.0      abind_1.4-8        KernSmooth_2.23-26
    #> [34] miniUI_0.1.2       withr_3.0.2        purrr_1.0.4        grid_4.4.3         urlchecker_1.0.1   profvis_0.4.0      xts_0.14.1         readtext_0.91      xtable_1.8-4       e1071_1.7-16       scales_1.4.0      
    #> [45] cli_3.6.3          rmarkdown_2.29     intervals_0.15.5   generics_0.1.3     remotes_2.5.0      rstudioapi_0.17.1  httr_1.4.7         sessioninfo_1.2.3  DBI_1.2.3          cachem_1.1.0       proxy_0.4-27      
    #> [56] stringr_1.5.1      stars_0.6-8        parallel_4.4.3     vctrs_0.6.5        devtools_2.4.5     units_0.8-7        glue_1.8.0         pkgdown_2.1.2      codetools_0.2-20   gstat_2.1-3        lubridate_1.9.4   
    #> [67] stringi_1.8.7      gtable_0.3.6       later_1.4.2        tibble_3.2.1       pillar_1.10.2      htmltools_0.5.8.1  R6_2.6.1           rprojroot_2.0.4    evaluate_1.0.3     shiny_1.10.0       lattice_0.22-6    
    #> [78] memoise_2.0.1      snakecase_0.11.1   httpuv_1.6.16      class_7.3-23       Rcpp_1.0.14        spacetime_1.3-3    xfun_0.52          fs_1.6.6           zoo_1.8-14         usethis_3.1.0      pkgconfig_2.0.3

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

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
