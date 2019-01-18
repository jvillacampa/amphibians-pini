# Code to calculate abundances differences

##########################################################.
## Data ----
##########################################################.
# bringing packages and functions
source("Scripts R/Functions and packages.R")

amph_data <- readRDS("Datos/amph_data_basefile.rds") # basefile

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
  calculate_kruskal(vars_group = c("habitat", "group"), type = "habitat", 
                       selection = "Terrestrial") 
)


##########################################################.
## Old method ----
##########################################################.

#Archivos de especies
Baltamazonica <- read.delim("Datos/abundancias/Baltamazonica.txt")
Danae <- read.delim("Datos/abundancias/Danae.txt")
Carvalhoi <- read.delim("Datos/abundancias/Carvalhoi.txt")
Diadematus <- read.delim("Datos/abundancias/Diadematus.txt")
Macero <- read.delim("Datos/abundancias/Macero.txt")
Ockendeni <- read.delim("Datos/abundancias/Ockendeni.txt")
Psp3 <- read.delim("Datos/abundancias/Psp3.txt")
Reichlei <- read.delim("Datos/abundancias/Reichlei.txt")

#Archivos de grupos
Terrestres <- read.delim("Datos/abundancias/Terrestres.txt")
Semiarboreos <- read.delim("Datos/abundancias/Semiarboreos.txt")
Arboreos <- read.delim("Datos/abundancias/Arboreos.txt")
Biomasa1 <- read.delim("Datos/abundancias/Biomasa1.txt")
Biomasa2 <- read.delim("Datos/abundancias/Biomasa2.txt")
Biomasa3 <- read.delim("Datos/abundancias/Biomasa3.txt")
Bufonidae <- read.delim("Datos/abundancias/Bufonidae.txt")
Craugastoridae <- read.delim("Datos/abundancias/Craugastoridae.txt")
Dendrobatidae <- read.delim("Datos/abundancias/Dendrobatidae.txt")
Hylidae <- read.delim("Datos/abundancias/Hylidae.txt")
Leptodactylidae <- read.delim("Datos/abundancias/Leptodactylidae.txt")
Agua <- read.delim("Datos/abundancias/Agua.txt")
Otros <- read.delim("Datos/abundancias/Otros.txt")
Total.abun <- read.delim("Datos/abundancias/Total.abun.txt")


#Tests
kruskal.test(Abun ~ Altura, data=Danae)
kruskal.test(Abun ~ Altura, data=Baltamazonica)
kruskal.test(Abun ~ Altura, data=Andreae)
kruskal.test(Abun ~ Altura, data=Carvalhoi)
kruskal.test(Abun ~ Altura, data=Diadematus)
kruskal.test(Abun ~ Altura, data=Macero)
kruskal.test(Abun ~ Altura, data=Ockendeni)
kruskal.test(Abun ~ Altura, data=Psp3)
kruskal.test(Abun ~ Altura, data=Reichlei)

#Tests de grupos
kruskal.test(Abun ~ Altura, data=Biomasa1)
kruskal.test(Abun ~ Altura, data=Biomasa2)
kruskal.test(Abun ~ Altura, data=Biomasa3)
kruskal.test(Abun ~ Altura, data=Arboreos)
kruskal.test(Abun ~ Altura, data=Semiarboreos)
kruskal.test(Abun ~ Altura, data=Terrestres)
kruskal.test(Abun ~ Altura, data=Bufonidae)
kruskal.test(Abun ~ Altura, data=Craugastoridae)
kruskal.test(Abun ~ Altura, data=Dendrobatidae)
kruskal.test(Abun ~ Altura, data=Hylidae)
kruskal.test(Abun ~ Altura, data=Leptodactylidae)
kruskal.test(Abun ~ Altura, data=Agua)
kruskal.test(Abun ~ Altura, data=Otros)


kruskal.test(Abun ~ Altura, data=Total.abun)


## End