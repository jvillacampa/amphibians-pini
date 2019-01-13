# Code to host functions and packages.

##########################################################.
## Packages ----
##########################################################.
lapply(c("dplyr", "readxl", "readr", "iNEXT", "tidyr"), library, character.only = TRUE)

##########################################################.
## Functions ----
##########################################################.
# small function to create files for iNEXT
make_band_files <- function(site_chosen) {
  abun_band_transect %>% filter(site == site_chosen) %>% select(-site) %>% 
    spread (transect, n) %>% mutate_all(funs(replace(., is.na(.), 0))) %>% 
    select(-species) %>% as.matrix()
}

# END