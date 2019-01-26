# Code to run models on diversity and abundance.

# Part 1 - Overall Diversity/abundance explained by environmental variables
# Part 2 - Band Diversity/abundance explained by environmental variables
# Part 3 - Diversity/abundance explained by altitude
# Part 4 - Code to create shannon diversity graphs, figure 2

##########################################################..
## Data ----
##########################################################..
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

##########################################################..
## Old method ----
##########################################################..
Varamb <- read.delim("Datos/modelos glm/Varamb.txt", dec=",")
#No estandarizadas
Varamb <- read.delim("Datos/modelos glm/Varamb.noest.txt", dec=",")
#solo var ambientales para multicolinearidad
Varamb.sol <- read.delim("Datos/modelos glm/Varambsolo.txt", dec=",")

#por banda
Varamb500 <- read.delim("Datos/modelos glm/Varamb500.txt", dec=",")
Varamb700 <- read.delim("Datos/modelos glm/Varamb700.txt", dec=",")
Varamb900 <- read.delim("Datos/modelos glm/Varamb900.txt", dec=",")
Varamb1100 <- read.delim("Datos/modelos glm/Varamb1100.txt", dec=",")

#for plot in part 3
Diversidad.noacua <- read.delim("Datos/diversidad/Diversidad.noacua.txt", dec=",")
Diversidad <- read.delim("Datos/diversidad/Diversidad.txt", dec=",")
h<-1:4 #para generar el eje x para los graficos.

##########################################################..
## Overall Diversity/abundance explained by environmental variables ----
##########################################################..

#para mirar multicolinearidad
round(cor(Varamb.sol),3)

#Primero modelo con todas las variables y relacionando con diversidad, asumo normalidad:gausiano
div1<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente, family="gaussian", data=Varamb)
summary(div1)

#Devianza nula 9,15, residual 4,13 AIC 44,723
#Despu?s continuo quitando variables, probablemente se pueden quitar todas las no significativas de golpe
#pero yo voy una a una por si acaso fuera cobdosel
div2<-glm(Diversidad~Hojarasca+AltDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente, family="gaussian", data=Varamb)
summary(div2)
#fuera helechos
div3<-glm(Diversidad~Hojarasca+AltDosel+Densidad+Epifitas+?rboles+Necromasa+Pendiente, family="gaussian", data=Varamb)
summary(div3)
#fuera densidad
div4<-glm(Diversidad~Hojarasca+AltDosel+Epifitas+?rboles+Necromasa+Pendiente, family="gaussian", data=Varamb)
summary(div4)
#fuera necromasa
div5<-glm(Diversidad~Hojarasca+AltDosel+Epifitas+?rboles+Pendiente, family="gaussian", data=Varamb)
summary(div5)
#fuera pendiente
div6<-glm(Diversidad~Hojarasca+AltDosel+Epifitas+?rboles, family="gaussian", data=Varamb)
summary(div6)
#fuera arboles
div7<-glm(Diversidad~Hojarasca+AltDosel+Epifitas, family="gaussian", data=Varamb)
summary(div7)
#este es el modelo final AIC 42,24 devianza residual 5,24 nula 9,15

#Ahora con abundancia
#Modelo completo Null deviance: 791.77  on 39  degrees of freedom
#Residual deviance: 484.13  on 30  degrees of freedom
#AIC: 235.25
abun1<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente, family="gaussian", data=Varamb)
summary(abun1)
#fuera necromasa
abun2<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb)
summary(abun2)
#fuera cobdosel
abun3<-glm(Abundancia~Hojarasca+AltDosel+Densidad+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb)
summary(abun3)
#fuera arboles
abun4<-glm(Abundancia~Hojarasca+AltDosel+Densidad+Epifitas+Helechos+Pendiente, family="gaussian", data=Varamb)
summary(abun4)
#fuera helechos
abun5<-glm(Abundancia~Hojarasca+AltDosel+Densidad+Epifitas+Pendiente, family="gaussian", data=Varamb)
summary(abun5)
#fuera densidad y modelo final
abun6<-glm(Abundancia~Hojarasca+AltDosel+Epifitas+Pendiente, family="gaussian", data=Varamb)
summary(abun6)

#Ahora para riqueza
riq1<-glm(Riqueza~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente, family="gaussian", data=Varamb)
summary(riq1)
#fuera cob dosel
riq2<-glm(Riqueza~Hojarasca+AltDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente, family="gaussian", data=Varamb)
summary(riq2)
#fuera Pendiente
riq3<-glm(Riqueza~Hojarasca+AltDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos, family="gaussian", data=Varamb)
summary(riq3)
#fuera necromasa
riq4<-glm(Riqueza~Hojarasca+AltDosel+Densidad+Epifitas+?rboles+Helechos, family="gaussian", data=Varamb)
summary(riq4)
#fuera densidad
riq5<-glm(Riqueza~Hojarasca+AltDosel+Epifitas+?rboles+Helechos, family="gaussian", data=Varamb)
summary(riq5)
#fuera helechos
riq6<-glm(Riqueza~Hojarasca+AltDosel+Epifitas+?rboles, family="gaussian", data=Varamb)
summary(riq6)
#fuera arboles y final
riq7<-glm(Riqueza~Hojarasca+AltDosel+Epifitas, family="gaussian", data=Varamb)
summary(riq7)

riq8<-glm(Riqueza~Hojarasca+Epifitas, family="gaussian", data=Varamb)
summary(riq8)

#Modelos controlando efecto banda
#diversidad
divb<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente+Banda, family="gaussian", data=Varamb)
summary(divb)
divb<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Banda, family="gaussian", data=Varamb)
summary(divb)
divb<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Banda, family="gaussian", data=Varamb)
summary(divb)
divb<-glm(Diversidad~Hojarasca+AltDosel+Densidad+Epifitas+?rboles+Necromasa+Banda, family="gaussian", data=Varamb)
summary(divb)
divb<-glm(Diversidad~Hojarasca+AltDosel+Densidad+?rboles+Necromasa+Banda, family="gaussian", data=Varamb)
summary(divb)
divb<-glm(Diversidad~Hojarasca+AltDosel+Densidad+Necromasa+Banda, family="gaussian", data=Varamb)
summary(divb)
divb<-glm(Diversidad~Hojarasca+AltDosel+Densidad+Banda, family="gaussian", data=Varamb)
summary(divb)
divb<-glm(Diversidad~Hojarasca+AltDosel+Banda, family="gaussian", data=Varamb)
summary(divb)


#riqueza
riqb<-glm(Riqueza~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente+Banda, family="gaussian", data=Varamb)
summary(riqb)
riqb<-glm(Riqueza~Hojarasca+AltDosel+CobDosel+Densidad+?rboles+Necromasa+Helechos+Pendiente+Banda, family="gaussian", data=Varamb)
summary(riqb)
riqb<-glm(Riqueza~Hojarasca+AltDosel+CobDosel+Densidad+?rboles+Necromasa+Pendiente+Banda, family="gaussian", data=Varamb)
summary(riqb)
riqb<-glm(Riqueza~Hojarasca+AltDosel+CobDosel+Densidad+?rboles+Pendiente+Banda, family="gaussian", data=Varamb)
summary(riqb)
riqb<-glm(Riqueza~Hojarasca+AltDosel+Densidad+?rboles+Pendiente+Banda, family="gaussian", data=Varamb)
summary(riqb)
riqb<-glm(Riqueza~Hojarasca+AltDosel+?rboles+Pendiente+Banda, family="gaussian", data=Varamb)
summary(riqb)
riqb<-glm(Riqueza~Hojarasca+AltDosel+Pendiente+Banda, family="gaussian", data=Varamb)
summary(riqb)
riqb<-glm(Riqueza~Hojarasca+AltDosel+Banda, family="gaussian", data=Varamb)
summary(riqb)

#abundancia
abunb<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente+Banda, family="gaussian", data=Varamb)
summary(abunb)
abunb<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Pendiente+Banda, family="gaussian", data=Varamb)
summary(abunb)
abunb<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Pendiente+Banda, family="gaussian", data=Varamb)
summary(abunb)
abunb<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+Pendiente+Banda, family="gaussian", data=Varamb)
summary(abunb)
abunb<-glm(Abundancia~Hojarasca+AltDosel+Densidad+Epifitas+Pendiente+Banda, family="gaussian", data=Varamb)
summary(abunb)
abunb<-glm(Abundancia~Hojarasca+AltDosel+Epifitas+Pendiente+Banda, family="gaussian", data=Varamb)
summary(abunb)
abunb<-glm(Abundancia~Hojarasca+AltDosel+Pendiente+Banda, family="gaussian", data=Varamb)
summary(abunb)
abunb<-glm(Abundancia~Hojarasca+Pendiente+Banda, family="gaussian", data=Varamb)
summary(abunb)

#Ahora los modelos teniendo en cuenta la banda como covariable
#500

divb1<-glm(Diversidad~(Hojarasca+AltDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos+Pendiente)*Banda, family="gaussian", data=Varamb)
summary(divb1)

##########################################################..
## Band Diversity/abundance explained by environmental variables ----
##########################################################..

#Por banda
#500
#para mirar multicolinearidad
z<-data.frame(AltDosel,CobDosel,Hojarasca,Densidad, Epifitas,?rboles,Necromasa, Helechos,Pendiente)
round(cor(z),3)

#Pruebo eliminando necromasa por el an?lisis de multicolinearidad
div51a<-glm(Diversidad~Hojarasca+AltDosel+Densidad+Epifitas+?rboles+CobDosel+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(div51a)
#Pruebo eliminando pendiente
div51b<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Necromasa+Helechos, family="gaussian", data=Varamb500)
summary(div51b)
#Prosigo con el a, sigo eliminando ?rboles
div52<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(div52)
#Sigo eliminando pendiente
div53<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+Helechos, family="gaussian", data=Varamb500)
summary(div53)
#Sigo eliminando helechos
div54<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas, family="gaussian", data=Varamb500)
summary(div54)
#Sigo eliminando epifitas
div55<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Densidad, family="gaussian", data=Varamb500)
summary(div55)
#Sigo eliminando cobdosel
div56<-glm(Diversidad~Hojarasca+Densidad+AltDosel, family="gaussian", data=Varamb500)
summary(div56)
#Sigo eliminando densidad
div57<-glm(Diversidad~Hojarasca+AltDosel, family="gaussian", data=Varamb500)
summary(div57)

#Ahora con abundancia
#pruebo con necromasa fuera
abun51a<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(abun51a)
#pruebo con densidad fuera
abun51b<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Necromasa+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(abun51b)
#Con ambos el siguiente en ser eliminado ser?a el otro
#modelo sin ninguno
abun52<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(abun52)
#fuera altura dosel
abun53<-glm(Abundancia~Hojarasca+CobDosel+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(abun53)
#fuera ?rboles
abun54<-glm(Abundancia~Hojarasca+CobDosel+Epifitas+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(abun54)
#fuera cobdosel
abun55<-glm(Abundancia~Hojarasca+Epifitas+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(abun55)
#fuera pendiente
abun56<-glm(Abundancia~Hojarasca+Epifitas+Helechos, family="gaussian", data=Varamb500)
summary(abun56)
#fuera epifitas
abun57<-glm(Abundancia~Hojarasca+Helechos, family="gaussian", data=Varamb500)
summary(abun57)
#fuera hojarasca
abun58<-glm(Abundancia~Helechos, family="gaussian", data=Varamb500)
summary(abun58)

#Ahora para riqueza
#pruebo con necromasa fuera
riq51a<-glm(Riqueza~Hojarasca+AltDosel+CobDosel+Densidad+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(riq51a)
#pruebo con alt dosel fuera
riq51b<-glm(Riqueza~Hojarasca+Necromasa+CobDosel+Densidad+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(riq51b)
#Con ambos el siguiente en ser eliminado ser?a el otro
#modelo sin ninguno
riq51b<-glm(Riqueza~Hojarasca+CobDosel+Densidad+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(riq51b)
#fuera densidad
riq51b<-glm(Riqueza~Hojarasca+CobDosel+Epifitas+?rboles+Helechos+Pendiente, family="gaussian", data=Varamb500)
summary(riq51b)

riq51b<-glm(Riqueza~Hojarasca+CobDosel+Epifitas+?rboles+Helechos, family="gaussian", data=Varamb500)
summary(riq51b)

riq51b<-glm(Riqueza~Hojarasca+Epifitas+?rboles+Helechos, family="gaussian", data=Varamb500)
summary(riq51b)

riq51b<-glm(Riqueza~Hojarasca+Epifitas+Helechos, family="gaussian", data=Varamb500)
summary(riq51b)

riq51b<-glm(Riqueza~Hojarasca+Epifitas, family="gaussian", data=Varamb500)
summary(riq51b)

riq51b<-glm(Riqueza~Hojarasca, family="gaussian", data=Varamb500)
summary(riq51b)



#Banda 700

#para mirar multicolinearidad
z<-data.frame(AltDosel,CobDosel,Hojarasca,Densidad, Epifitas,?rboles,Necromasa, Helechos,Pendiente)
round(cor(z),3)
#Pruebo eliminando epifitas por el an?lisis de multicolinearidad
div51a<-glm(Diversidad~Hojarasca+AltDosel+Densidad+Necromasa+?rboles+CobDosel+Helechos+Pendiente, 
            family="gaussian", data=Varamb700)
summary(div51a)
#quito necromasa
div51a<-glm(Diversidad~Hojarasca+AltDosel+Densidad+?rboles+CobDosel+Helechos+Pendiente, 
            family="gaussian", data=Varamb700)
summary(div51a)
#todo significativo pero problemas de colinearidad.
#se quita densidad por ello
div51a<-glm(Diversidad~Hojarasca+AltDosel+?rboles+CobDosel+Helechos+Pendiente, 
            family="gaussian", data=Varamb700)
summary(div51a)

div51a<-glm(Diversidad~Hojarasca+AltDosel+CobDosel+Helechos+Pendiente, 
            family="gaussian", data=Varamb700)
summary(div51a)

div51a<-glm(Diversidad~Hojarasca+AltDosel+Helechos+Pendiente, family="gaussian", data=Varamb700)
summary(div51a)
#todo significativo pero problemas de colinearidad. Entre Hojarasca y pendiente
#sin hojarasca
div51a<-glm(Diversidad~AltDosel+Helechos+Pendiente, family="gaussian")
summary(div51a)

div51a<-glm(Diversidad~AltDosel+Pendiente, family="gaussian")
summary(div51a)

#sin pendiente
div51b<-glm(Diversidad~Hojarasca+AltDosel+Helechos, family="gaussian")
summary(div51b)

div51b<-glm(Diversidad~AltDosel+Helechos, family="gaussian")
summary(div51b)

div51b<-glm(Diversidad~AltDosel, family="gaussian")
summary(div51b)
#con los dos se llega a la misma soluci?n


#Ahora con abundancia
#pruebo con epifitas fuera
abun71a<-glm(Abundancia~Hojarasca+AltDosel+CobDosel+Densidad+Necromasa+?rboles+Helechos+Pendiente, 
             family="gaussian", data=Varamb700)
summary(abun71a)

abun71a<-glm(Abundancia~AltDosel+CobDosel+Densidad+Necromasa+?rboles+Helechos+Pendiente, 
             family="gaussian", data=Varamb700)
summary(abun71a)

abun71a<-glm(Abundancia~AltDosel+CobDosel+Densidad+Necromasa+?rboles+Helechos, family="gaussian", data=Varamb700)
summary(abun71a)

abun71a<-glm(Abundancia~AltDosel+CobDosel+Densidad+?rboles+Helechos, family="gaussian", data=Varamb700)
summary(abun71a)

abun71a<-glm(Abundancia~AltDosel+CobDosel+Densidad+Helechos, family="gaussian", data=Varamb700)
summary(abun71a)

##########################################################..
## Code to create shannon diversity graphs, figure 2 ----
##########################################################..

png(file="mapas-imagenes/Figure2.png",
    width=12,height=6,units="in",res=600)  # specify the png file size.

#Primer gráfico de diversidad general
par(mfrow=c(1,2))
par(mar=c(5.1,4.1,4.1,0)) #ajustando los margenes
errbar(h, Diversidad$Diversidad, yplus=Diversidad$Max, yminus=Diversidad$Min, 
       xlab="", ylab="Shannon's diversity (H')", main="agaga",
       ylim=c(1,2.5), xlim=c(1,4.1), cex.axis=0.9, cex.lab=1.2, cap=0.04, xaxt="n")
axis(side=1,labels=c("450-550","650-750","850-950","1050-1150      "), at=c(1,2,3,4), cex.axis=0.9)
title(main="Overall diversity")

#gráfico de no acuaticos(desarrollo directo)
par(mar=c(5.1,0,4.1,4.1)) #ajustando los margenes
errbar(h, Diversidad.noacua$Diversidad, yplus=Diversidad.noacua$Max, yminus=Diversidad.noacua$Min, 
       xlab="", ylab="", ylim=c(1,2.5), xlim=c(1,4.1), 
       cex.axis=1.3, cex.lab=0.9,cap=0.04, xaxt="n", yaxt="n")
axis(side=1,labels=c("      450-550","650-750","850-950","1050-1150"), at=c(1,2,3,4), cex.axis=0.9)
title(main="Excluding pond/stream breeders")
mtext('Altitudinal band', side = 1, outer = FALSE, line = 3, adj=-0.15, cex=1.2)
dev.off() # close device (stop writing to png so file is created)

####################################.
#con ggplot, no funciona muy bien
ggplot(data=Diversidad, aes(x=Banda, y=Diversidad, margins=TRUE))+
  geom_point(cex=3)+
  geom_errorbar(data=Diversidad, position=position_dodge(.9),width=0.3, aes(x=Banda, y=Diversidad, ymin=Min, ymax=Max))+
  theme(
    plot.background = element_blank()#to remove background plot
    ,panel.grid.major = element_blank()#to remove grid lines
    ,panel.grid.minor = element_blank()#to remove grid lines
    ,panel.border = element_blank()#to remove border line
    #,margins=TRUE
  ) +
  theme(panel.background = element_rect(fill = 'white', color="white"))+
  theme(axis.text.x=element_text(colour="black"))+ #color for labels
  theme(axis.text.y=element_text(colour="black")) #color for labels
theme(axis.line = element_line(color = 'black')) #axis lines
#no funciona theme(border.line = element_line(color = 'black')) #border lines


## END
