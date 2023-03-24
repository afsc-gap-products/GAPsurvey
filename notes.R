

# Load new FOSS data -----------------------------------------------------------

# This has a specific username and password because I DONT want people to have access to this!
source("https://raw.githubusercontent.com/afsc-gap-products/metadata/main/code/functions_oracle.R")

locations <- c("Z:/Projects/ConnectToOracle.R",
               "C:/Users/emily.markowitz/Work/Projects/ConnectToOracle.R")
for (i in 1:length(locations)){
  if (file.exists(locations[i])){
    source(locations[i])
  }
}

locations<-c(
  "RACEBASE_FOSS.JOIN_FOSS_CPUE_CATCH",
  "RACEBASE_FOSS.JOIN_FOSS_CPUE_HAUL",
  "GAP_PRODUCTS.OLD_TAXONOMICS_WORMS"
)

oracle_dl(
  locations = locations,
  channel = channel,
  dir_out = paste0("../notforgit/"))

library(magrittr)
library(readr)
library(dplyr)

a<-list.files(path = paste0("../notforgit/"), pattern = ".csv")
for (i in 1:length(a)){
  b <- read_csv(file = paste0("../notforgit/", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
}

public_data <- racebase_foss_join_foss_cpue_catch0 %>%
  dplyr::filter(
    !(count %in% c(NA, 0) & # this will remove 0-filled values
        weight_kg %in% c(NA, 0)) ) %>% # which shouldn't happen, but good to double check
  dplyr::left_join(x = .,
                   y = racebase_foss_join_foss_cpue_haul0,
                   by = "hauljoin")
save(public_data, file = "./data/public_data.rda")


species_data <- GAP_PRODUCTS_OLD_TAXONOMICS_WORMS0 %>%
  dplyr::filter(is.na(reason)) %>%
  dplyr::select(species_code,
                common_name,
                scientific_name = accepted_name,
                scientific_name_old = survey_name)
save(species_data, file = "./data/species_data.rda")

# Document Package ---------------------------------------------------
.rs.restartR()

# options(rmarkdown.html_vignette.check_title = FALSE)
Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.3.1/bin;', Sys.getenv('PATH')))
library(here)
library(devtools)
library(usethis)
library(roxygen2)
library(RODBC)
devtools::document()
setwd("..")
install("GAPsurvey")
3
setwd(here::here())
# devtools::check()

# Catch is Windows 10, Wheelhouse is Windows 7

## Create Documentation GitHub-Pages -------------------------------------------

.rs.restartR()
# devtools::install_github("rstudio/fontawesome", force = T)
# library(fontawesome)
library(here)
library(usethis)
library(pkgdown)
rmarkdown::render(input = "README.Rmd",
                  output_dir = "./",
                  output_file = "README.md")


# devtools::install_github("r-lib/pkgdown")
# pkgdown::build_favicons()
# devtools::build_vignettes()
# usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")

pkgdown::build_site(pkg = here::here())
# usethis::use_github_action("pkgdown")

# Save Package tar.gz
date0 <- "2023.04.01"
file.remove(paste0(dirname(here::here()), "/GAPsurvey_",date0,".tar.gz"))
file.remove(paste0((here::here()), "/GAPsurvey_",date0,".tar.gz"))
devtools::build()
file.copy(from = paste0(dirname(here::here()), "/GAPsurvey_",date0,".tar.gz"),
          to = paste0(here::here(), "/GAPsurvey_",date0,".tar.gz"),
          overwrite = TRUE)
