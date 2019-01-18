# Code to run diversity analysis

##########################################################.
## Data ----
##########################################################.
# bringing packages and functions
source("Scripts R/Functions and packages.R")

amph_data <- readRDS("Datos/amph_data_basefile.rds") # basefile

##########################################################.
# Preparing files needed for iNEXT diversity calculations
# Abundance by band: total and by functional category
abun_band <- amph_data %>% mutate(group = paste0("B", site)) %>% make_inext_file()

abun_band_repro <- amph_data %>% mutate(group = paste0("B", site, reproduction)) %>% 
  make_inext_file()

abun_band_weight <- amph_data %>% mutate(group = paste0("B", site, weight_group)) %>% 
  make_inext_file()

abun_band_habitat <- amph_data %>% mutate(group = paste0("B", site, habitat)) %>% 
  make_inext_file()

##########################################################.
# Abundance by transect: total and by functional category
abun_transect <- amph_data %>% mutate(group = transect) %>% make_inext_file()

abun_trans_repro <- amph_data %>% mutate(group = paste0("B", site, "_", transect, reproduction)) %>% 
  make_inext_file()

abun_trans_weight <- amph_data %>% mutate(group = paste0("B", site, "_", transect, weight_group)) %>% 
  make_inext_file()

abun_trans_habitat <- amph_data %>% mutate(group = paste0("B", site, "_", transect, habitat)) %>% 
  make_inext_file()

##########################################################.
## Diversity analisys ----
##########################################################.
# running diversity analysis. Endpoint double minimum sample size (Colwell et al.)
##########################################################.
# Diversity by band: total and by functional category
band_div_total <- iNEXT(abun_band, q=c(0,1,2), datatype="abundance",
                          endpoint = 132)

band_div_repro <- iNEXT(abun_band_repro, q=c(0,1,2), datatype="abundance",
                          endpoint = 132)

band_div_repro <- iNEXT(abun_band_repro, q=c(0,1,2), datatype="abundance",
                        endpoint = 132)

band_div_weight <- iNEXT(abun_band_weight, q=c(0,1,2), datatype="abundance",
                        endpoint = 132)

band_div_habitat <- iNEXT(abun_band_habitat, q=c(0,1,2), datatype="abundance",
                        endpoint = 132)

##########################################################.
# Diversity by transect: total and by functional category
transect_div_total <- iNEXT(abun_transect, q=c(0,1,2), datatype="abundance")
transect_div_repro <- iNEXT(abun_transect_repro, q=c(0,1,2), datatype="abundance")
transect_div_weight <- iNEXT(abun_transect_weight, q=c(0,1,2), datatype="abundance")
transect_div_habitat <- iNEXT(abun_transect_habitat, q=c(0,1,2), datatype="abundance")

ggiNEXT(band_div_results, type=1, facet.var="order", color.var="site")
ggiNEXT(transect_div_results, type=1, facet.var="order", color.var="site")

## END