#' @title Subsetted species data
#' @description Taxonomic classification information for the current list of SPECIES_CODE values. This table was created by the Resource Assessment and Conservation Engineering Division (RACE) Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC). The GitHub repository for the scripts that created this code can be found at (https://github.com/afsc-gap-products/gap_products). There are no legal restrictions on access to the data. Last updated on 12 September 2024.
#' @usage data('species_data')
#' @author Sarah Friedman (sarah.friedman AT noaa.gov)
#' @format A data frame with 2718 observations on the following 3 variables.
#' \describe{
#'   \item{\code{scientific_name}}{Taxon scientific name. The scientific name of the organism associated with the common_name and species_code columns. For a complete taxon list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{species_code}}{Taxon code. The species code of the organism associated with the common_name and scientific_name columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{common_name}}{Taxon common name. The common name of the marine organism associated with the scientific_name and species_code columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}#' }
#' @source https://github.com/afsc-gap-products/gap_products
#' @keywords species code data
#' @examples
#' data(species_data)
#' @details The Resource Assessment and Conservation Engineering (RACE) Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess the populations of demersal fish and crab stocks of Alaska.

'species_data'
