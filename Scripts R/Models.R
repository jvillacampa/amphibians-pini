# Code to run models on diversity and abundance.

# Part 1 - Overall Diversity/abundance explained by environmental variables
# Part 2 - Band Diversity/abundance explained by environmental variables
# Part 3 - Diversity/abundance explained by altitude
# Part 4 - Code to create shannon diversity graphs, figure 2

##########################################################.
## Data ----
##########################################################.
# bringing packages and functions
source("Scripts R/Functions and packages.R")

# Files created in diversity analysis script.
div_mod_tot <- readRDS("Datos/prepared_data/modeldata_tot.rds")
# Reproduction groups
div_mod_water <- readRDS("Datos/prepared_data/modeldata_water.rds")
div_mod_otherrep <- readRDS("Datos/prepared_data/modeldata_otherrep.rds")
# Weight groups
div_mod_less2 <- readRDS("Datos/prepared_data/modeldata_less2.rds")
div_mod_2to10 <- readRDS("Datos/prepared_data/modeldata_2to10.rds")
div_mod_more10 <- readRDS("Datos/prepared_data/modeldata_more10.rds")
# Habitat
div_mod_arboreal <- readRDS("Datos/prepared_data/modeldata_arboreal.rds")
div_mod_terrest <- readRDS("Datos/prepared_data/modeldata_terrest.rds")
div_mod_semiarb <- readRDS("Datos/prepared_data/modeldata_semiarb.rds")

##########################################################..
## Abundance models ----
##########################################################..
##########################################################..
# Totalmodels
# Two lower bands much higher than two upper bands.
mod_abun_tot_500 <- glm(abundance ~ site, data = div_mod_tot, family = "poisson")
summary(mod_abun_tot_500) # no significant, almost with 1100
plot(mod_abun_tot_500) #checking residuals
hist(mod_abun_tot_500$residuals, breaks = 10) #checking residuals
mod_abun_tot_700 <- glm(abundance ~ relevel(site, ref = "700"), 
                        data = div_mod_tot, family = "poisson")
summary(mod_abun_tot_700) # significant between 700 and 900, 1100
mod_abun_tot_900 <- glm(abundance ~ relevel(site, ref = "900"), 
                        data = div_mod_tot, family = "poisson")
summary(mod_abun_tot_900) # with 700
##########################################################..
# Reproduction groups
# Water
# Due to high amount of overdispersion and zero inflation no model works well here.
# Other reproductive habitats
mod_abun_otherrep_500 <- glm(abundance ~ site, data = div_mod_otherrep, family = "poisson")
summary(mod_abun_otherrep_500) # almost with 700
plot(mod_abun_otherrep_500)
hist(mod_abun_otherrep_500$residuals, breaks = 5)
mod_abun_otherrep_700 <- glm(abundance ~ relevel(site, ref = "700"), 
                            data = div_mod_otherrep, family = "poisson")
summary(mod_abun_otherrep_700) # significant between 700 and 1100, almost 500
mod_abun_otherrep_900 <- glm(abundance ~ relevel(site, ref = "900"), 
                            data = div_mod_otherrep, family = "poisson")
summary(mod_abun_otherrep_900) # no signifcant
##########################################################..
# Weight groups
# Less than 2.5
mod_abun_less2_500 <- glm(abundance ~ site, data = div_mod_less2, family = "poisson")
summary(mod_abun_less2_500) # significan between 500 and 700 and 1100
plot(mod_abun_less2_500)
hist(mod_abun_less2_500$residuals, breaks = 5)
mod_abun_less2_700 <- glm(abundance ~ relevel(site, ref = "700"), 
                          data = div_mod_less2, family = "poisson")
summary(mod_abun_less2_700) # significan between 700 and 500 and 1100
mod_abun_less2_900 <- glm(abundance ~ relevel(site, ref = "900"), 
                          data = div_mod_less2, family = "poisson")
summary(mod_abun_less2_900) # significan between 900 and 1100
# 2.5 to 10
mod_abun_2to10_500 <- glm(abundance ~ site, data = div_mod_2to10, family = "poisson")
summary(mod_abun_2to10_500) # almost significan between 500 and 1100
plot(mod_abun_2to10_500)
hist(mod_abun_2to10_500$residuals, breaks = 5)
mod_abun_2to10_700 <- glm(abundance ~ relevel(site, ref = "700"), 
                          data = div_mod_2to10, family = "poisson")
summary(mod_abun_2to10_700) # almost significan between 700 and 1100
mod_abun_2to10_900 <- glm(abundance ~ relevel(site, ref = "900"), 
                          data = div_mod_2to10, family = "poisson")
summary(mod_abun_2to10_900) # significant between 900 and 1100

# More 10
# No models here because sample size too small.

##########################################################..
# Habitat
# Arboreal
# No models here because sample size too small.

# Terrestrial
mod_abun_terrest_500 <- glm(abundance ~ site, data = div_mod_terrest, family = "poisson")
summary(mod_abun_terrest_500) # significant between 500 and 900 and 1100
plot(mod_abun_terrest_500)
hist(mod_abun_terrest_500$residuals, breaks = 5)
mod_abun_terrest_700 <- glm(abundance ~ relevel(site, ref = "700"), 
                           data = div_mod_terrest, family = "poisson")
summary(mod_abun_terrest_700) # significant between 700 and 900 and 1100
mod_abun_terrest_900 <- glm(abundance ~ relevel(site, ref = "900"), 
                           data = div_mod_terrest, family = "poisson")
summary(mod_abun_terrest_900) # significant between 900 and 500 and 700
# Semiarboreal
mod_abun_semiarb_500 <- glm(abundance ~ site, data = div_mod_semiarb, family = "poisson")
summary(mod_abun_semiarb_500) # significant with 700
plot(mod_abun_semiarb_500)
hist(mod_abun_semiarb_500$residuals, breaks = 5)
mod_abun_semiarb_700 <- glm(abundance ~ relevel(site, ref = "700"), 
                            data = div_mod_semiarb, family = "poisson")
summary(mod_abun_semiarb_700) # significant between 700 and 1100 and 500
mod_abun_semiarb_900 <- glm(abundance ~ relevel(site, ref = "900"), 
                            data = div_mod_semiarb, family = "poisson")
summary(mod_abun_semiarb_900) # no sign

##########################################################..
##  Observed richness models ----
##########################################################..
##########################################################.
# Total models
# Clear decrease with altitude
mod_rich_tot_500b <- glm(richness ~ site, data = div_mod_tot, family = "poisson")
plot(mod_rich_tot_500b) #checking residuals
hist(mod_rich_tot_500b$residuals, breaks = 10) #checking residuals
summary(mod_rich_tot_500b) # significant between 500 and 1100 and almost 900
mod_rich_tot_700 <- glm(richness ~ relevel(site, ref = "700"), 
                        data = div_mod_tot, family = "poisson")
summary(mod_rich_tot_700) # significant between 700 and  1100
mod_rich_tot_900 <- glm(richness ~ relevel(site, ref = "900"), family = "poisson",
                       data = div_mod_tot)
summary(mod_rich_tot_900) # almost with 500
##########################################################.
# Reproduction groups
# Water
# Due to high amount of overdispersion and zero inflation no model works well here.
# Other reproductive habitats
mod_rich_otherrep_500 <- glm(richness ~ site, data = div_mod_otherrep, family = "poisson")
plot(mod_rich_less2_500)
hist(mod_rich_less2_500$residuals, breaks = 5)
summary(mod_rich_otherrep_500) # significant with 1100
mod_rich_otherrep_700 <- glm(richness ~ relevel(site, ref = "700"), 
                             data = div_mod_otherrep, family = "poisson")
summary(mod_rich_otherrep_700) # significant with 1100
mod_rich_otherrep_900 <- glm(richness ~ relevel(site, ref = "900"), 
                             data = div_mod_otherrep, family = "poisson")
summary(mod_rich_otherrep_900) # no significant
##########################################################.
# Weight groups
# Less than 2.5
mod_rich_less2_500 <- glm(richness ~ site, data = div_mod_less2, family = "poisson")
summary(mod_rich_less2_500) # significant between 500 and 1100 
plot(mod_rich_less2_500)
hist(mod_rich_less2_500$residuals, breaks = 5)
mod_rich_less2_700 <- glm(richness ~ relevel(site, ref = "700"), 
                          data = div_mod_less2, family = "poisson")
summary(mod_rich_less2_700) # significant between 700 and 1100
mod_rich_less2_900 <- glm(richness ~ relevel(site, ref = "900"), 
                          data = div_mod_less2, family = "poisson")
summary(mod_rich_less2_900) # significant between 900 and 1100
# 2.5 to 10
mod_rich_2to10_500 <- glm(richness ~ site, data = div_mod_2to10)
summary(mod_rich_2to10_500) # no significant
plot(mod_rich_2to10_500)
hist(mod_rich_2to10_500$residuals, breaks = 5)
mod_rich_2to10_700 <- glm(richness ~ relevel(site, ref = "700"), 
                          data = div_mod_2to10, family = "poisson")
summary(mod_rich_2to10_700) # no significant
mod_rich_2to10_900 <- glm(richness ~ relevel(site, ref = "900"), 
                          data = div_mod_2to10, family = "poisson")
summary(mod_rich_2to10_900) # no significant

# More 10
# No models here because sample size too small.

##########################################################.
# Habitat
# Arboreal
# No models here because sample size too small.

# Terrestrial
mod_rich_terrest_500 <- glm(richness ~ site, data = div_mod_terrest, family = "poisson")
summary(mod_rich_terrest_500) # significant between 500 and 900 
plot(mod_rich_terrest_500)
hist(mod_rich_terrest_500$residuals, breaks = 5)
mod_rich_terrest_700 <- glm(richness ~ relevel(site, ref = "700"), 
                           data = div_mod_terrest, family = "poisson")
summary(mod_rich_terrest_700) # significant between 700 and 900
mod_rich_terrest_900 <- glm(richness ~ relevel(site, ref = "900"), 
                           data = div_mod_terrest, family = "poisson")
summary(mod_rich_terrest_900) # significant between 900 and 500 and 700
# Semiarboreal
mod_rich_semiarb_500 <- glm(richness ~ site, data = div_mod_semiarb, family = "poisson")
summary(mod_rich_semiarb_500) # sign with 1100
plot(mod_rich_semiarb_500)
hist(mod_rich_semiarb_500$residuals, breaks = 5)
mod_rich_semiarb_700 <- glm(richness ~ relevel(site, ref = "700"), 
                           data = div_mod_semiarb, family = "poisson")
summary(mod_rich_semiarb_700) # significant between 700 and 1100
mod_rich_semiarb_900 <- glm(richness ~ relevel(site, ref = "900"), 
                           data = div_mod_semiarb, family = "poisson")
summary(mod_rich_semiarb_900) # almost sign with 1100

##########################################################..
##  Shannon models ----
##########################################################..
##########################################################..
##########################################################.
# Total models
# Clear decrease with altitude
# Not doing models for functional groups as the groups size is small and not clear
# what I will obtain from that.
mod_shan_tot_500 <- lm(log(shannon) ~ site, data = div_mod_tot)
summary(mod_shan_tot_500) # significant between 500 and (900 and 1100)
plot(mod_shan_tot_500)
hist(mod_shan_tot_500$residuals, breaks = 10)
mod_shan_tot_700 <- lm(log(shannon) ~ relevel(site, ref = "700"), data = div_mod_tot)
summary(mod_shan_tot_700) # significant between 700 and 1100, almost 900
mod_shan_tot_900 <- lm(log(shannon) ~ relevel(site, ref = "900"), data = div_mod_tot)
summary(mod_shan_tot_900) # significant between 900 and 500 and almost 700

##########################################################..
##  Simpson models ----
##########################################################..
##########################################################.
# Total models
# Clear decrease with altitude
# Not doing models for functional groups as the groups size is small and not clear
# what I will obtain from that.
mod_simp_tot_500 <- lm(log(simpson) ~ site, data = div_mod_tot)
summary(mod_simp_tot_500) # significant between 500 and 900 and 1100, almost 700
plot(mod_simp_tot_500)
hist(mod_simp_tot_500$residuals, breaks = 10)
mod_simp_tot_700 <- lm(log(simpson) ~ relevel(site, ref = "700"), data = div_mod_tot)
summary(mod_simp_tot_700) # significant between 700 and 1100, almost 900 and 500
mod_simp_tot_900 <- lm(log(simpson) ~ relevel(site, ref = "900"), data = div_mod_tot)
summary(mod_simp_tot_900) # significant between 900 and 500, almost 700

##########################################################.
## Creating summary table ----
##########################################################.

model_table <- data.frame(model = "Simpson´s diversity ~ altitudinal band",
                          reference = c("450-550 vs 650-750", "450-550 vs 850-950",
                                        "450-550 vs 1050-1150", "650-750 vs 850-950",
                                        "650-750 vs 1050-1150", "850-950 vs 1050-1150")) %>% 
  mutate(coefficient = case_when(reference == "450-550 vs 650-750" ~ mod_simp_tot_500$coefficient["site700"],
                                 reference == "450-550 vs 850-950" ~ mod_simp_tot_500$coefficient["site900"],
                                 reference == "450-550 vs 1050-1150" ~ mod_simp_tot_500$coefficient["site1100"],
                                 reference == "650-750 vs 850-950" ~ mod_simp_tot_700$coefficient[3],
                                 reference == "650-750 vs 1050-1150" ~ mod_simp_tot_700$coefficient[4],
                                 reference == "850-950 vs 1050-1150" ~ mod_simp_tot_900$coefficient[4]),
         pvalue = case_when(reference == "450-550 vs 650-750" ~ summary(mod_simp_tot_500)$coefficients[2,4] ,
                            reference == "450-550 vs 850-950" ~ summary(mod_simp_tot_500)$coefficients[3,4] ,
                            reference == "450-550 vs 1050-1150" ~ summary(mod_simp_tot_500)$coefficients[4,4] ,
                            reference == "650-750 vs 850-950" ~ summary(mod_simp_tot_700)$coefficients[3,4] ,
                            reference == "650-750 vs 1050-1150" ~ summary(mod_simp_tot_700)$coefficients[4,4] ,
                            reference == "850-950 vs 1050-1150" ~ summary(mod_simp_tot_900)$coefficients[4,4] ))

# summary(mod_simp_tot_900)$coefficients[,4] 
# relevel(site, ref = "700")900
# test <- 
# model_table$coefficient[reference == "450-550 vs 650-750"] <- mod_simp_tot_700$coefficient["site700"]
# 
# glance(mod_simp_tot_500)
# 
# "450-550 vs 650-750" summary(mod_simp_tot_500)$coefficients[,4]["site700"] 

## END
