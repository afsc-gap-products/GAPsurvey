# Good to know links:

# The computer we usefor the catch comptuer
# https://www.onlogic.com/ml350g-10/?cpsrc=StandardShoppingB&kw=&gclid=Cj0KCQjwo-aCBhC-ARIsAAkNQivOtLBIFqE1ZJ2Qch0B8HfpDnG0dYjGhwn88wNUINasV2rjACuXHrcaAi5PEALw_wcB

# test data:
# https://drive.google.com/drive/folders/1EyXHy1AO6jWppUi0qI85YLhCcanIo-x4?usp=sharing
# https://drive.google.com/drive/folders/13ahk5y7ygEcTSfDWXPEQn42vP2QUJV_C?usp=sharing

# 2021 GAP Survey CTD Team
# https://drive.google.com/drive/folders/1XqWqrvaq1ZAhrZZzxy9NfSlgEDeJRn9G?usp=sharing


# Data
# C:\Users\emily.markowitz\Documents\Projects\GAPsurvey_general\GAPsurvey\inst\exdata\catch\GOA
# G:\GOA\GOA 2019\DATA_2019\Ocean Explorer

# example of basket weight thing
# G:\ALEUTIAN\AI 2018\Data\Sea Storm\Leg 2\CATCH\OriginalData

# Things to fix in 2022
# 1. LOGtoGPS() fix 4pm double file problem
# 2. make CTD .hex and .xml files inputs into CTDtoBTD
# 
# gitlab vs github
# 
# Thoughts on GAPsurvey being on GitLab vs. GitHub? I would advocate for GitHub, but it's up to what you think is best. I don't think anything in the package is sensitive. It has also been so useful to share links to code with people in the group to make sure everything works like it should and see where improvements can be made. The only thing that might be debatably sensitive would be the empty data_ent.mbd file? I can also remove that file from the package if that is a concern. 
# In all honesty I'm not worried about this being public.  The data that's even in it is raw and the package doesn't have much utility outside of our program.   But, I need some education here, I thought we could share between our users using GitLab?
# Totally agree. You're right about GitLab, but the platform can seem a bit exclusionary to people unfamiliar with the platform. Its not actually that hard to access, but looks harder than it is. I imagine that if I had shared the GitLab link today for the package, probably only 5 or so people would have known how to access it/thought they could access it. 
# The issue mostly comes down to educating people about the platform, but that is still an issue. 
# Ok.  Thanks.  That was what I remembered but the rules have been changing on this stuff fast.  The next question is, what GitLab account?  I know at one point we were debating the creation of one for GAP...but then we sort of need an administrator.
# ah that's a bit more complicated of an answer. Right now it is on Jason's GitLab and my GitHub (should be updated on both, but i've needed some IT help to connect to GitLab). There isnt currently a GAP Git* account and I think there needs to be a bit more discussion about how to do that. 
# maybe that was less complicated to explain than I thought haha
# Maybe we need to have this be a topic in a group meeting.  Especially for something like this package which has multiple authorship.  Especially since updating and maintaining packages seems to typically fall on the owner of the storage hub.
# I think you're exactly right - Creating the physical account is easy, but no one wants to be the only person at the helm and we need to come up with a plan to work on it together. 
                                                                                                # In this case, I've made Liz, cecilia, megsie, duane, and jason collaborators on the GAPsurvey github page, so if they want to, they will be able to make changes as needed and will get "credit" for making that change, but its on my page as opposed to a more centrailized location
# History is off. Messages may disappear before people see them. Conversations with no messages may disappear.



install.packages("C:/Users/liz.dawson/Desktop/AKKNIGHT2021CATCHHAULS1-7/RODBC_1.3-16.zip", repos = NULL, type = "source")
install.packages("C:/Users/liz.dawson/Work/R_Code/gapsurvey_20210607/GAPsurvey_2.4.00.tar.gz", repos = NULL, type = "source")
dsnTablet <- "C:/Users/liz.dawson/Desktop/AKKNIGHT2021CATCHHAULS1-7"
dsnDataEnt <- "C:/Users/liz.dawson/Desktop/AKKNIGHT2021CATCHHAULS1-7/data_ent.mdb"



############## QUESTIONS ####################

# inst/documentation folder - what do we need here if anything?

########### Document Package ############
.rs.restartR()

# options(rmarkdown.html_vignette.check_title = FALSE)
Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.3.1/bin;', Sys.getenv('PATH')))
library(here)
library(devtools)
library(roxygen2)
devtools::document()
setwd("..")
install("GAPsurvey")
3
setwd(here::here())
# devtools::check()

########### Create Documentation GitHub-Pages ############

.rs.restartR()
# devtools::install_github("rstudio/fontawesome", force = T)
# library(fontawesome)
library(here)
library(usethis)
library(pkgdown)
# options(rmarkdown.html_vignette.check_title = FALSE)

# git rm -r --cached .

# devtools::install_github("r-lib/pkgdown")
# pkgdown::build_favicons()
# devtools::build_vignettes()
usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")
pkgdown::build_site(pkg = here::here())
usethis::use_github_action("pkgdown")

# Save Package tar.gz

# devtools::check()
file.remove(paste0(dirname(here::here()), "/GAPsurvey_2021.08.01.tar.gz"))
file.remove(paste0((here::here()), "/GAPsurvey_2021.08.01.tar.gz"))
devtools::build()
file.copy(from = paste0(dirname(here::here()), "/GAPsurvey_2021.08.01.tar.gz"), 
          to = paste0(here::here(), "/GAPsurvey_2021.08.01.tar.gz"),
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


CTDtoBTD(
  VESSEL = 94,
  CRUISE = 2001,
  HAUL = 4,
  MODEL_NUMBER = 123,
  VERSION_NUMBER = 456,
  SERIAL_NUMBER = 789,
  path_in = system.file("exdata/ctd2btd/SBE19plus_01908103_2021_06_01_94_0004_raw.cnv", package = "GAPsurvey"),   
  path_out = getwd(),
  filename_add = "newctd", 
  quiet = TRUE)


LOGtoGPS(
  VESSEL = 94,
  CRUISE = 202101,
  HAUL = 37,
  DATE = "06/07/2021",
  path_in = "C:/Users/emily.markowitz/Documents/Projects/GAPsurvey_general/GAPsurvey/inst/exdata/log2gps/Haul0037.log",
  path_out = getwd(),
  filename_add = "newlog",
  quiet = TRUE)
