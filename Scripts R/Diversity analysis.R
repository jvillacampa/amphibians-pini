# Code to run diversity analysis

##########################################################.
## Data ----
##########################################################.
# bringing packages and functions
source("Scripts R/Functions and packages.R")

amph_data <- readRDS("Datos/amph_data_basefile.rds") # basefile

# Preparing files needed for iNEXT diversity calculations
abun_band <- amph_data %>% mutate(site = paste0("Band", site)) %>% 
  group_by(site, species) %>% count() %>% spread (site, n) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% ungroup() %>% 
  select(-species) %>% as.list()

# Don't need it
# Creating file with matrix for each band with a column per transect(sampling unit)
# abun_band_transect <- amph_data %>% mutate(site = paste0("Band", site)) %>% 
#   group_by(transect, site, species) %>% count() %>% ungroup()
# 
# abun_sampling_unit <- list(band500 = make_band_files("Band500"), band700 = make_band_files("Band700"),
#              band900 = make_band_files("Band900"), band1100 = make_band_files("Band1100"))

abun_transect <- amph_data %>% 
  group_by(transect, species) %>% count() %>% spread (transect, n) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% ungroup() %>% 
  select(-species) %>% as.list()

##########################################################.
## Diversity analisys ----
##########################################################.
# running diversity analysis. Endpoint double minimum sample size (Colwell et al.)
band_div_results <- iNEXT(abun_band, q=c(0,1,2), datatype="abundance",
                          endpoint = 132)

transect_div_results <- iNEXT(abun_transect, q=c(0,1,2), datatype="abundance")


ggiNEXT(band_div_results, type=1, facet.var="order", color.var="site")
ggiNEXT(transect_div_results, type=1, facet.var="order", color.var="site")

