# Code to read raw data and clean it

# bringing packages and functions
source("Scripts R/Functions and packages.R")

##########################################################.
## Creating directory structure ----
##########################################################.
# Creating folders that will be used through the analysis.
dir.create("Datos/prepared_data") # for prepared basefiles
dir.create("Results/") # for results

##########################################################.
## Basefile data ----
##########################################################.
#Creating basefile from raw data
amph_data <- read_excel("Datos/raw_data/amphibian_survey_data.xlsx", 
                   sheet = "Data", range = "A1:R575") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  # selecting only amphibians in ves, exlcuding audio records - not standardly surveyed
  filter(observation_type == "VES" & class == "Amphibia" & 
           (notes != "audio" | is.na(notes))) %>%
  #adenomera sp1 same as a.andreae, and UID sps to publication names 
  mutate(species = recode(species, "Adenomera sp1" = "Adenomera andreae", 
                          "Noblella sp1" = "Noblella spA", "Pristimantis sp3" = "Pristimantis spA"),
         transect = tolower(transect)) # some in capital letters
  
#bringing functional group information
funct_lookup <- read_csv2("Datos/raw_data/functional_groups_lookup.csv")

#merging with main dataset  
amph_data <- left_join(amph_data, funct_lookup, by = c("species")) 

saveRDS(amph_data, "Datos/prepared_data/amph_data_basefile.rds")

##########################################################.
## Vegetation mapping data ----
##########################################################.
veg_mapping <- read_csv2("Datos/raw_data/vegmap_transect_data.csv") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  mutate_if(is.numeric, funs(scale(.))) #scaling variables

saveRDS(veg_mapping, "Datos/prepared_data/vegmap_basefile.rds")


# END