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
## Creating tables ----
##########################################################.
# Creating table needed with species abundances by band and kruskal p values
abun_spec_table <- amph_data %>% group_by(species, site) %>% count() %>% 
  spread(site, n) %>% ungroup() # converting each band in a column

abun_spec_table <- left_join(abun_spec_table, kruskal_results, by = "species") %>% 
  mutate_all(funs(replace(., is.na(.), "-"))) #joining with kruskal pvalue

write_csv2(abun_spec_table, "Results/abundance/abun_spec_table.csv")

# Creating table needed with group abundances by band and kruskal p values
abun_func_table <- amph_data %>% # making long format to then aggregate
  gather(func_group, func_cat, habitat, reproduction, weight_group) %>% 
  group_by(func_group, func_cat, site) %>% count() %>% 
  spread(site, n) %>% ungroup() # converting each band in a column

abun_func_table <- left_join(abun_func_table, kruskal_results, by = c("func_cat" = "species")) %>% 
  mutate_all(funs(replace(., is.na(.), "-"))) #joining with kruskal pvalue

write_csv2(abun_func_table, "Results/abundance/abun_func_table.csv")


## End