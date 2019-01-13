# code to read raw data, clean it and calculate diversity

##########################################################.
##Packages----
lapply(c("dplyr", "readxl", "readr", "iNEXT", "tidyr"), library, character.only = TRUE)

##########################################################.
## Data ----
##########################################################.
amph_data <- read_excel("Datos/Raw data/Amphibian_survey_data.xlsx", 
                   sheet = "Data", range = "A1:R575") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  # selecting only amphibians in ves, exlcuding audio records
  filter(observation_type == "VES" & class == "Amphibia" & 
           (notes != "audio" | is.na(notes))) %>%
  #adenomera sp1 same as a.andreae
  mutate(species = recode(species, "Adenomera sp1" = "Adenomera andreae"),
         transect = tolower(transect)) # some in capital letters
  

#bringing functional group information
funct_lookup <- read_csv2("Datos/functional_groups_lookup.csv")
  
amph_data <- left_join(amph_data, funct_lookup, by = c("species")) 

saveRDS(amph_data, "Datos/amph_data_basefile.rds")
amph_dat <- readRDS(amph_data, "Datos/amph_data_basefile.rds")

amph_abun <- amph_data %>% mutate(site = paste0("Band", site)) %>% 
  group_by(site, species) %>% count() %>% spread (site, n) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% ungroup() %>% 
  select(-species) %>% as.list()

iNEXT(amph_abun, q=0, datatype="abundance")
