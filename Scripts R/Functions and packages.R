# Code to host functions and packages.

##########################################################.
## Packages ----
##########################################################.
lapply(c("dplyr", "readxl", "readr", "iNEXT", "tidyr", "ggplot2", "Hmisc", 
         "tibble", "vegan", "ggpubr"), library, character.only = TRUE)

##########################################################.
## Data ----
##########################################################.
# Loading basefiles
amph_data <- readRDS("Datos/prepared_data/amph_data_basefile.rds") # basefile amphibians
veg_mapping <- readRDS("Datos/prepared_data/vegmap_basefile.rds") #vegetation mapping data

band_pal <- c("#d7191c", "#fdae61", "#3690c0", "#034e7b") #palette for altitudinal bands

##########################################################.
## Functions ----
##########################################################.
# small function to create files for iNEXT
make_inext_file <- function(data_abun) {
  data_abun %>% group_by(group, species) %>% count() %>% spread (group, n) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% ungroup() %>% 
    select(-species) %>% as.list()
}

##########################################################.
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
  result <- kruskal.test(abundance ~ band, data = abun_data) 
  kruskal.test(abundance ~ band, data = abun_data)
  
  data.frame(pvalue = result$p.value, species = selection)
  
}

##########################################################.
# Function to combine together results from inext. It's also used in save_model_file()
combine_inext <- function(results_list, type) {
  if (type == "band") {
    rbind(
      results_list[["iNextEst"]][["B500"]] %>% mutate(altura = "450 - 550"),
      results_list[["iNextEst"]][["B700"]] %>% mutate(altura = "650 - 750"),
      results_list[["iNextEst"]][["B900"]] %>% mutate(altura = "850 - 950"),
      results_list[["iNextEst"]][["B1100"]] %>% mutate(altura = "1050 - 1150")) %>% 
      mutate(altura = factor(altura, levels = c("450 - 550", "650 - 750", 
                                                "850 - 950", "1050 - 1150"))) %>% 
      setNames(tolower(names(.))) 
    
  } else if (type == "transect") {
    do.call("rbind", results_list[["iNextEst"]]) %>% 
      mutate(transect = substr(row.names(.), 1, unlist(gregexpr("\\.", row.names(.))) - 1)) %>% 
      setNames(tolower(names(.))) 
  }

}

##########################################################.
# Function to format diversity results and save them for modelling.
save_model_file <- function(results_list, filename) {
  
    results_data <- results_list[["AsyEst"]] %>% 
      setNames(tolower(names(.))) %>% rename(transect = site) %>% 
       mutate(diversity = recode(diversity, "Species richness" = "richness" , 
                                "Shannon diversity" = "shannon", "Simpson diversity" ="simpson" )) %>% 
      select(transect, observed, diversity) %>% 
      spread(diversity, observed) # moving from long to wide format
    
    # Adding information on what band each transect is
    trans_band_lookup <- amph_data %>% select(transect, site) %>% unique()
    
    results_data <- left_join(trans_band_lookup, results_data, by = "transect") %>% 
      mutate(site = as.factor(site))
    
    # Create dataframe with abundance by transect and merge with diversity one
    abun_data <- combine_inext(results_list, "transect") %>% 
      filter(method == "observed" & order == 0) %>% rename(abundance = m) %>% 
      select(transect, abundance) %>% unique() #to avoid duplicates
    
      results_data <- left_join(results_data, abun_data, by= "transect") %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) #converting Nas in 0
  # Saving the file
  saveRDS(results_data, paste0("Datos/prepared_data/", filename, ".rds"))
  
}

# END