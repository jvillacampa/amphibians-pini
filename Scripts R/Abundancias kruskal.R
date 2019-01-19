# Code to calculate abundances differences

##########################################################.
## Data ----
##########################################################.
# bringing packages, basefile data and functions
source("Scripts R/Functions and packages.R")

##########################################################.
## Calculating Kruskal test ----
##########################################################.
# Only for species with 10 or more records
amph_data %>% group_by(species) %>% count() %>% filter(n>9)

kruskal_results <- rbind(
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                  selection = "Adenomera andreae"), 
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                    selection = "Ameerega macero"), 
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                    selection = "Bolitoglossa altamazonica"), 
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                    selection = "Pristimantis carvalhoi"), 
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                    selection = "Pristimantis cf. diadematus"), 
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                    selection = "Pristimantis danae"),
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                    selection = "Pristimantis ockendeni"),
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                    selection = "Pristimantis reichlei"), 
  calculate_kruskal(vars_group = c("species", "group"), type = "species", 
                  selection = "Pristimantis sp3"), 
  calculate_kruskal(vars_group = c("reproduction", "group"), type = "reproduction", 
                    selection = "Bodies of water"), 
  calculate_kruskal(vars_group = c("reproduction", "group"), type = "reproduction", 
                    selection = "Other"), 
  calculate_kruskal(vars_group = c("weight_group", "group"), type = "weight", 
                    selection = "< 2.5g"), 
  calculate_kruskal(vars_group = c("weight_group", "group"), type = "weight", 
                    selection = "2.5 - 10g"), 
  calculate_kruskal(vars_group = c("weight_group", "group"), type = "weight", 
                    selection = "> 10g"), 
  calculate_kruskal(vars_group = c("habitat", "group"), type = "habitat", 
                       selection = "Terrestrial"), 
  calculate_kruskal(vars_group = c("habitat", "group"), type = "habitat", 
                    selection = "Arboreal"), 
  calculate_kruskal(vars_group = c("habitat", "group"), type = "habitat", 
                    selection = "Semiarboreal") 
) 

kruskal_results <- kruskal_results %>% 
  mutate(pvalue = round(pvalue, 3), 
         pvalue = case_when(pvalue <= 0.001 ~ paste0("< 0.001*"),
                            between(pvalue, 0.001, 0.05) ~ paste0(pvalue, "*"),
                            pvalue > 0.05 ~ paste0(pvalue)))
##########################################################.
## Abundance by transects ----
##########################################################.
# Files needed for modelling
abun_trans_tot <- amph_data %>% group_by(transect) %>% count() %>% ungroup()
saveRDS(abun_trans_tot, paste0("Datos/prepared_data/modabun_tot.rds"))

abun_trans_repro <- amph_data %>% group_by(transect) %>% count() %>% ungroup()
saveRDS(abun_trans_repro, paste0("Datos/prepared_data/modabun_repro.rds"))

abun_trans_weight <- amph_data %>% group_by(transect) %>% count() %>% ungroup()
saveRDS(abun_trans, paste0("Datos/prepared_data/modabun_weight.rds"))

abun_trans_habitat <- amph_data %>% group_by(transect) %>% count() %>% ungroup()
saveRDS(abun_trans, paste0("Datos/prepared_data/modabun_habitat.rds"))

##########################################################.
## Creating table ----
##########################################################.
# Creating table needed with abundances by band and kruskal p values
abun_table <- amph_data %>% group_by(species, site) %>% count() %>% 
  spread(site, n) %>% ungroup()

abun_table <- left_join(abun_table, kruskal_results, by = "species") %>% 
  mutate_all(funs(replace(., is.na(.), "-"))) 


## End