# Code to read raw data and clean it

# bringing packages and functions
source("Scripts R/Functions and packages.R")
##########################################################.
## Basefile data ----
##########################################################.
#Creating basefile from raw data
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

#merging with main dataset  
amph_data <- left_join(amph_data, funct_lookup, by = c("species")) 

saveRDS(amph_data, "Datos/amph_data_basefile.rds")

##########################################################.
## Vegetation mapping data ----
##########################################################.
veg_mapping <- read_excel("Datos/Raw data/Vegmap summary data.xlsx", 
                        sheet = "Varamb.noest", range = "A1:L41") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  mutate_if(is.numeric, funs(scale(.))) #scaling variables

saveRDS(veg_mapping, "Datos/vegmap_basefile.rds")


# END