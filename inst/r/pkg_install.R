
#' Package install
#' 
#' @param PKG name of package
#' this script has additional capability to work on google workstations. 
#' You can use the library install bash script. 
#' @keywords internal
#' @example 
#' # lapply(PKG = c("dplyr", "ggplot2"), pkg_install)
pkg_install <- function(PKG){
  
  if(base::grepl("/home/user/", base::getwd())){
    base::system("chmod a+x ubuntu_libraries.sh")
    base::system("./ubuntu_libraries.sh")
  }
  
  if(!require(PKG, character.only = TRUE)) {
    if (PKG == 'coldpool') {
      devtools::install_github("afsc-gap-products/coldpool")
    } else if (PKG == "akgfmapas") {
      devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
    } else if (PKG == 'gapctd') {
      remotes::install_github("afsc-gap-products/gapctd")
    } else if (PKG == 'navmaps') {
      remotes::install_github("afsc-gap-products/navmaps")
    } else if (PKG == 'gapindex') {
      remotes::install_github("afsc-gap-products/gapindex")
    } else {
      utils::install.packages(PKG)
    }
    require(PKG, character.only = TRUE)}
}

