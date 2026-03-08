<!-- README.md is generated from README.Rmd. Please edit that file -->

# GAPsurvey <a href={https://afsc-gap-products.github.io/GAPsurvey}><img src="man/figures/logo.png" align="right" width=139 height=139 alt="logo."/>

*At-sea data management tools for RACE GAP surveys*

> This code is always in development

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov; EmilyMarkowitz-NOAA)

**Sean Rohan** (Sean.Rohan AT noaa.gov; sean-rohan-NOAA)

**Margaret Siple** (Margaret Siple AT noaa.gov; MargaretSiple-NOAA)

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

## Table of contents

> - [*Make sure the necessary packages are
>   installed*](#make-sure-the-necessary-packages-are-installed)
> - [*example, the user may have a different
>   path*](#example,-the-user-may-have-a-different-path)
>   - [*User Resources*](#user-resources)
>   - [*Cite this data*](#cite-this-data)
> - [*Relevant publications*](#relevant-publications)
> - [*Suggestions and Comments*](#suggestions-and-comments)
>   - [*R Version Metadata*](#r-version-metadata)
>   - [*NOAA README*](#noaa-readme)
>   - [*NOAA License*](#noaa-license)

## Make sure the necessary packages are installed

``` r
library(devtools)

devtools::install_github("afsc-gap-products/GAPsurvey")
 # Or
remotes::install_github("afsc-gap-products/GAPsurvey@main")

library(GAPsurvey)
```

or install from local file `.tar.gz`:

``` r
# example, the user may have a different path
install.packages('C:/Users/User/Downloads/GAPsurvey_2025.06.07.tar.gz',
                 repos=NULL, type='source')
library(GAPsurvey)
```

## User Resources

- [GitHub
  repository](https://github.com/afsc-gap-products/gap_products).

- [Access Tips and Documentation for All Production
  Data](https://afsc-gap-products.github.io/gap_products/)

- [Fisheries One Stop Shop (FOSS)](https://www.fisheries.noaa.gov/foss)

- [Groundfish Assessment Program Bottom Trawl
  Surveys](https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys)

- [AFSC’s Resource Assessment and Conservation Engineering
  Division](https://www.fisheries.noaa.gov/about/resource-assessment-and-conservation-engineering-division)

- [Survey code
  books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual)

- [Publications and Data Reports](https://repository.library.noaa.gov/)

- [Research Surveys conducted at
  AFSC](https://www.fisheries.noaa.gov/alaska/ecosystems/alaska-fish-research-surveys)

## Cite this data

Use the below [bibtext
citations](%22https://afsc-gap-products.github.io/GAPsurvey/blob/main/code/CITATION.bib%22)
for citing the package created and maintained in this repo. Add “note =
{Accessed: mm/dd/yyyy}” to append the day this data was accessed.

``` r
cat(readLines(con = here::here("inst/CITATION.bib")), sep = "\n") 
#> @misc{GAPsurvey,
#>   author = {{NOAA Fisheries Alaska Fisheries Science Center, Goundfish Assessment Program}},
#>   year = {2024},
#>   title = {AFSC Goundfish Assessment Program at-Sea data management tools for RACE GAP surveys},
#>   howpublished = {https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys},
#>   publisher = {{U.S. Dep. Commer.}},
#>   copyright = {Public Domain}
#> }
```

# Relevant publications

``` r
source("https://raw.githubusercontent.com/afsc-gap-products/citations/main/cite/current_data_tm.r") # srvy_cite 
```

**Learn more about these surveys** (Hoff, 2016; Markowitz et al., 2024;
Markowitz et al., 2025; Siple et al., 2024; Von Szalay et al., 2023;
Zacher et al., 2024).

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-RN979" class="csl-entry">

Hoff, G. R. (2016). *Results of the 2016 eastern Bering Sea upper
continental slope survey of groundfishes and invertebrate resources*
(NOAA Tech. Memo. NOAA-AFSC-339). U.S. Dep. Commer.
<https://doi.org/10.7289/V5/TM-AFSC-339>

</div>

<div id="ref-2023NEBS" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Wassermann, S., Anderson, C. B., Rohan,
S. K., Charriere, B. K., and Stevenson, D. E. (2024). *Results of the
2023 eastern and northern Bering Sea continental shelf bottom trawl
survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-AFSC-487; p. 242). U.S. Dep. Commer.
<https://doi.org/10.25923/2mry-yx09>

</div>

<div id="ref-2024EBS" class="csl-entry">

Markowitz, E. H., Wassermann, S., Rohan, S. K., Charriere, B. K.,
Anderson, C. B., and Stevenson, D. E. (2025). *Results of the 2024
eastern and northern Bering Sea continental shelf bottom trawl survey of
groundfish and invertebrate fauna* (NOAA Tech. Memo. NMFS-AFSC-499; p.
203). U.S. Dep. Commer. <https://doi.org/10.25923/8qa3-x785>

</div>

<div id="ref-GOA2023" class="csl-entry">

Siple, M. C., Szalay, P. G. von, Raring, N. W., Dowlin, A. N., and
Riggle, B. C. (2024). *Data report: 2023 gulf of alaska bottom trawl
survey* (NOAA Tech. Memo. AFSC processed report; 2024-09). U.S. Dep.
Commer. <https://doi.org/10.25923/gbb1-x748>

</div>

<div id="ref-AI2022" class="csl-entry">

Von Szalay, P. G., Raring, N. W., Siple, M. C., Dowlin, A. N., Riggle,
B. C., and Laman, E. A. and. (2023). *Data report: 2022 Aleutian Islands
bottom trawl survey* (AFSC Processed Rep. 2023-07; p. 230). U.S. Dep.
Commer. <https://doi.org/10.25923/85cy-g225>

</div>

<div id="ref-SAPcrab2024" class="csl-entry">

Zacher, L. S., Richar, J. I., Fedewa, E. J., Ryznar, E. R., and Litzow,
M. A. (2024). *The 2024 eastern Bering Sea continental shelf trawl
survey: Results for commercial crab species* \[NOAA Tech. Memo.\].
(NFMS-AFSC-491), 237. <https://doi.org/10.25923/q0fw-z324>

</div>

</div>

# Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/afsc-gap-products/GAPsurvey/pulls), [submit
an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/afsc-gap-products/GAPsurvey/issues).

## R Version Metadata

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows 11 x64 (build 22631)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: America/Los_Angeles
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats4    stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] fontawesome_0.5.3   ggspatial_1.1.10    pkgdown_2.2.0       roxygen2_7.3.3      RODBC_1.3-26.1     
#>  [6] sp_2.2-1            httr_1.4.8          jsonlite_2.0.0      gapindex_3.0.3      gapctd_2.1.8       
#> [11] plotly_4.12.0       interp_1.1-6        bbmle_1.0.25.1      oce_1.8-3           gsw_1.2-0          
#> [16] coldpool_3.5-3      stringr_1.6.0       reshape2_1.4.5      lubridate_1.9.5     fields_17.1        
#> [21] RColorBrewer_1.1-3  spam_2.11-3         gstat_2.1-5         ggthemes_5.2.0      akgfmaps_4.2.1     
#> [26] terra_1.8-93        stars_0.7-1         abind_1.4-8         sf_1.1-0            here_1.0.2         
#> [31] data.table_1.18.2.1 janitor_2.2.1       tibble_3.3.1        ggplot2_4.0.2       readr_2.2.0        
#> [36] viridis_0.6.5       viridisLite_0.4.3   readxl_1.4.5        tidyr_1.3.2         magrittr_2.0.4     
#> [41] dplyr_1.2.0         plyr_1.8.9          remotes_2.5.0       devtools_2.4.6      usethis_3.2.1      
#> 
#> loaded via a namespace (and not attached):
#>  [1] DBI_1.3.0           deldir_2.0-4        gridExtra_2.3       rlang_1.1.7         snakecase_0.11.1   
#>  [6] otel_0.2.0          e1071_1.7-17        compiler_4.5.2      getPass_0.2-4       vctrs_0.7.1        
#> [11] maps_3.4.3          pkgconfig_2.0.3     fastmap_1.2.0       ellipsis_0.3.2      rmarkdown_2.30     
#> [16] sessioninfo_1.2.3   tzdb_0.5.0          purrr_1.2.1         xfun_0.56           cachem_1.1.0       
#> [21] parallel_4.5.2      R6_2.6.1            stringi_1.8.7       pkgload_1.5.0       cellranger_1.1.0   
#> [26] numDeriv_2016.8-1.1 knitr_1.51          Rcpp_1.1.1          zoo_1.8-15          readtext_0.92.1    
#> [31] FNN_1.1.4.1         Matrix_1.7-4        timechange_0.4.0    tidyselect_1.2.1    yaml_2.3.12        
#> [36] rstudioapi_0.18.0   codetools_0.2-20    pkgbuild_1.4.8      lattice_0.22-9      intervals_0.15.5   
#> [41] withr_3.0.2         S7_0.2.1            evaluate_1.0.5      units_1.0-0         proxy_0.4-29       
#> [46] xml2_1.5.2          xts_0.14.2          pillar_1.11.1       KernSmooth_2.23-26  generics_0.1.4     
#> [51] rprojroot_2.1.1     spacetime_1.3-3     hms_1.1.4           scales_1.4.0        class_7.3-23       
#> [56] glue_1.8.0          lazyeval_0.2.2      tools_4.5.2         fs_1.6.7            mvtnorm_1.3-3      
#> [61] dotCall64_1.2       grid_4.5.2          bdsmatrix_1.3-7     raster_3.6-32       cli_3.6.5          
#> [66] gtable_0.3.6        digest_0.6.39       classInt_0.4-11     htmlwidgets_1.6.4   farver_2.1.2       
#> [71] memoise_2.0.1       htmltools_0.5.9     lifecycle_1.0.5     MASS_7.3-65
```

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

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
