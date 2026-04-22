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
> - [*User Resources*](#user-resources)
> - [*Updating the Package*](#updating-the-package)
>   - [*Step 1: Update the Version Number in DESCRIPTION
>     file*](#step-1:-update-the-version-number-in-description-file)
>   - [*Step 2: Once a year: Re-download source
>     data*](#step-2:-once-a-year:-re-download-source-data)
>   - [*Step 3: If you make needed edits to
>     README.rmd*](#step-3:-if-you-make-needed-edits-to-readme.rmd)
>   - [*Step 4: If you add or remove
>     functions*](#step-4:-if-you-add-or-remove-functions)
>   - [*Step 5: Commit and Push to Main
>     Branch*](#step-5:-commit-and-push-to-main-branch)
>   - [*Step 6: Automated Workflows*](#step-6:-automated-workflows)
>     - [*A. R-CMD-check Workflow
>       (.github/workflows/R-CMD-check.yaml)*](#a.-r-cmd-check-workflow-(.github/workflows/r-cmd-check.yaml))
>     - [*B. pkgdown Workflow
>       (.github/workflows/pkgdown.yml)*](#b.-pkgdown-workflow-(.github/workflows/pkgdown.yml))
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
install.packages('C:/Users/User/Downloads/GAPsurvey.tar.gz',
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

# Updating the Package

This repository uses automated GitHub Actions workflows to streamline
package updates and documentation builds. Follow these steps to release
a new version once you have made your content edits to the package code.

## Step 1: Update the Version Number in DESCRIPTION file

The only manual change you need to make is updating the version number
in the
[`DESCRIPTION`](https://github.com/afsc-gap-products/GAPsurvey/blob/main/DESCRIPTION)
file at the repository root. The version uses a date-based format
(YYYY.MM.DD). For example: `Version: 2026.04.21`

## Step 2: Once a year: Re-download source data

Once a year re-download data to go into the package using this
[data_download.R
script](https://github.com/afsc-gap-products/GAPsurvey/blob/main/inst/r/data_download.R).

## Step 3: If you make needed edits to README.rmd

Changes should be minimal, but all edits to this README file must be
made in the
[RMD](https://github.com/afsc-gap-products/GAPsurvey/blob/main/inst/r/README.Rmd)
version.

## Step 4: If you add or remove functions

If you add or remove functions that are supposed to be used by the user,
update the function list in the
[`_pkgdown.yml`](https://github.com/afsc-gap-products/GAPsurvey/blob/main/_pkgdown.yml).

## Step 5: Commit and Push to Main Branch

Once you’ve updated the `DESCRIPTION` file, commit your changes and push
to the main branch.

## Step 6: Automated Workflows

Two GitHub Actions workflows will automatically trigger on your push to
main:

### A. R-CMD-check Workflow (.github/workflows/R-CMD-check.yaml)

This workflow:

✅ Runs R package checks on Windows to ensure code quality

✅ Installs all package dependencies (devtools, knitr, rmarkdown, here)

✅ Builds the package into a .tar.gz file with the version number from
DESCRIPTION (e.g., GAPsurvey-2026.04.21.tar.gz)

✅ Converts the vignette GAPsurvey-script.Rmd into an R script
(inst/r/GAPsurvey-script.R)

✅ Renders inst/r/README.Rmd into README.md

✅ Commits and pushes the updated generated files back to the repository

✅ Uploads the built .tar.gz package as a workflow artifact (available
for download in GitHub Actions)

Check status: Visit GitHub Actions to view workflow runs and download
built packages.

### B. pkgdown Workflow (.github/workflows/pkgdown.yml)

This workflow:

✅ Builds the package documentation site using pkgdown

✅ Deploys the documentation to GitHub Pages

✅ Automatically updates when you push to main

View documentation: <https://afsc-gap-products.github.io/GAPsurvey>

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
#> R version 4.5.3 (2026-03-11 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows Server 2022 x64 (build 26100)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8 
#> [2] LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: UTC
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39       R6_2.6.1            fastmap_1.2.0      
#>  [4] xfun_0.57           glue_1.8.1          knitr_1.51         
#>  [7] htmltools_0.5.9     rmarkdown_2.31      lifecycle_1.0.5    
#> [10] cli_3.6.6           readtext_0.92.1     vctrs_0.7.3        
#> [13] data.table_1.18.2.1 compiler_4.5.3      rprojroot_2.1.1    
#> [16] here_1.0.2          httr_1.4.8          tools_4.5.3        
#> [19] pillar_1.11.1       evaluate_1.0.5      yaml_2.3.12        
#> [22] otel_0.2.0          rlang_1.2.0         stringi_1.8.7
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
