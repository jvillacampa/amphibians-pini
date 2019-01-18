# Code to host functions and packages.

##########################################################.
## Packages ----
##########################################################.
lapply(c("dplyr", "readxl", "readr", "iNEXT", "tidyr"), library, character.only = TRUE)

##########################################################.
## Functions ----
##########################################################.
# small function to create files for iNEXT
make_inext_file <- function(data_abun) {
  data_abun %>% group_by(group, species) %>% count() %>% spread (group, n) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% ungroup() %>% 
    select(-species) %>% as.list()
}

# Function to calculate kruskall wallis for abundance between bands
calculate_kruskal <- function(vars_group, type, selection) {
  
  # Calculating abundance for the grouping variales
  abun_data <- amph_data %>% mutate(group = paste0("B", site, "_", transect)) %>% 
    group_by_at(vars_group) %>% count() %>% spread (group, n) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% ungroup() 
  
  # SUbsetting dataset for the group needed
  if (type == "species") {
    abun_data <- abun_data %>% filter(species == selection) %>% 
      gather(key = species, value = abundance) %>% rename(group = species)     
  } else if (type == "habitat") {
    abun_data <- abun_data %>% filter(habitat == selection) %>% 
      gather(key = habitat, value = abundance) %>% rename(group = habitat) 
  } else if (type == "reproduction") {
    abun_data <- abun_data %>% filter(reproduction == selection) %>% 
      gather(key = reproduction, value = abundance) %>% rename(group = reproduction) 
  } else if (type == "weight") {
    abun_data <- abun_data %>% filter(weight_group == selection) %>% 
      gather(key = weight_group, value = abundance) %>% rename(group = weight_group) 
  }
  
  abun_data <- abun_data %>% mutate(band = as.factor(substr(group, 2, 5))) %>% 
    select(-group) 
  # Calculating Kruskal-Wallis test
  kruskal.test(abundance ~ band, data = abun_data) 
  
}

# END