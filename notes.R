# Good to know links:

# The computer we usefor the catch comptuer
# https://www.onlogic.com/ml350g-10/?cpsrc=StandardShoppingB&kw=&gclid=Cj0KCQjwo-aCBhC-ARIsAAkNQivOtLBIFqE1ZJ2Qch0B8HfpDnG0dYjGhwn88wNUINasV2rjACuXHrcaAi5PEALw_wcB

# test data:
# https://drive.google.com/drive/folders/1EyXHy1AO6jWppUi0qI85YLhCcanIo-x4?usp=sharing
# https://drive.google.com/drive/folders/13ahk5y7ygEcTSfDWXPEQn42vP2QUJV_C?usp=sharing

# 2021 GAP Survey CTD Team
# https://drive.google.com/drive/folders/1XqWqrvaq1ZAhrZZzxy9NfSlgEDeJRn9G?usp=sharing


# Things to fix in 2022
# 1. LOGtoGPS() fix 4pm double file problem
# 2. make CTD .hex and .xml files inputs into CTDtoBTD
# 

############## QUESTIONS ####################

# inst/documentation folder - what do we need here if anything?

########### Document Package ############
.rs.restartR()

# options(rmarkdown.html_vignette.check_title = FALSE)
# Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.0.1/bin;', Sys.getenv('PATH')))
library(here)
library(devtools)
library(roxygen2)
devtools::document()
setwd("..")
install("GAPsurvey")
3
setwd(here::here())
# devtools::check()
file.remove(paste0(dirname(here::here()), "/GAPsurvey_2.3.00.tar.gz"))
file.remove(paste0((here::here()), "/GAPsurvey_2.3.00.tar.gz"))
devtools::build()
file.copy(from = paste0(dirname(here::here()), "/GAPsurvey_2.3.00.tar.gz"), 
          to = paste0(here::here(), "/GAPsurvey_2.3.00.tar.gz"),
          overwrite = TRUE)

########### Create Documentation GitHub-Pages ############

.rs.restartR()

# git rm -r --cached .
# git reset --hard HEAD^^

# devtools::install_github("rstudio/fontawesome", force = T)
# library(fontawesome)
# library(here)
# library(usethis)
# library(pkgdown)
# options(rmarkdown.html_vignette.check_title = FALSE)

########### Submit to CRAN ############

# devtools::check() # add the console output to cran-commentes.md
# devtools::check_rhub() # check that your email is 'validated.' You'll need a token emailed to you
# devtools::check_win_devel()
# devtools::release()

##########build a vignette#########

# usethis::use_vignette("testfunct")


# loading qpdf:
# https://stackoverflow.com/questions/41570633/how-to-build-qpdf-on-windows
# https://stackoverflow.com/questions/15035150/r-cmd-check-as-cran-warning
# https://sourceforge.net/projects/qpdf/files/qpdf/10.0.1/
# Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.0.1/bin;', Sys.getenv('PATH')))
# Sys.which(Sys.getenv("R_QPDF", "qpdf"))
