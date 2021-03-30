library(GAPsurvey)

dsnDataEnt <- "C:/TEMPEDIT/CATCH"

PolySpecies <- selectDataEnt(dsnDataEnt, "select distinct a.species_code, a.poly_species_code, b.species_name, b.common_name
                            from LENGTH_WEIGHT_PARAMETERS as a, species_list as b
                            where a.SPECIES_CODE = b.SPECIES_CODE")

names(PolySpecies) <- toupper(names(PolySpecies))

devtools::use_data(PolySpecies, overwrite=T)
