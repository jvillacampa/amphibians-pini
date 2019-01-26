# Code to run diversity analysis

##########################################################.
## Data ----
##########################################################.
# bringing packages and functions
source("Scripts R/Functions and packages.R")

##########################################################.
# Preparing files needed for iNEXT alpha diversity calculations
# Abundance by band: total and by functional category
inext_band <- amph_data %>% mutate(group = paste0("B", site)) %>% make_inext_file()
# Reproduction groups
inext_band_water <- amph_data %>% mutate(group = paste0("B", site, reproduction)) %>% 
  filter(reproduction == "Bodies of water") %>% make_inext_file()
inext_band_otherrep <- amph_data %>% mutate(group = paste0("B", site, reproduction)) %>% 
  filter(reproduction == "Other") %>% make_inext_file()
# Weight groups
inext_band_less2 <- amph_data %>% mutate(group = paste0("B", site, weight_group)) %>% 
  filter(weight_group == "< 2.5g") %>% make_inext_file()
inext_band_2to10 <- amph_data %>% mutate(group = paste0("B", site, weight_group)) %>% 
  filter(weight_group == "2.5 - 10g") %>% make_inext_file()
inext_band_more10 <- amph_data %>% mutate(group = paste0("B", site, weight_group)) %>% 
  filter(weight_group == "> 10g") %>% make_inext_file()
# Habitat
inext_band_arboreal <- amph_data %>% mutate(group = paste0("B", site, habitat)) %>% 
  filter(habitat == "Arboreal") %>% make_inext_file()
inext_band_terrest <- amph_data %>% mutate(group = paste0("B", site, habitat)) %>% 
  filter(habitat == "Terrestrial") %>% make_inext_file()
inext_band_semiarb <- amph_data %>% mutate(group = paste0("B", site, habitat)) %>% 
  filter(habitat == "Semiarboreal") %>% make_inext_file()

##########################################################.
# Abundance by transect: total and by functional category
inext_transect <- amph_data %>% mutate(group = transect) %>% make_inext_file()
# Reproduction groups
inext_trans_water <- amph_data %>% mutate(group = transect) %>% 
  filter(reproduction == "Bodies of water") %>% make_inext_file()
inext_trans_otherrep <- amph_data %>% mutate(group = transect) %>% 
  filter(reproduction == "Other") %>% make_inext_file()
# Weight groups
inext_trans_less2 <- amph_data %>% mutate(group = transect) %>% 
  filter(weight_group == "< 2.5g") %>% make_inext_file()
inext_trans_2to10 <- amph_data %>% mutate(group = transect) %>% 
  filter(weight_group == "2.5 - 10g") %>% make_inext_file()
inext_trans_more10 <- amph_data %>% mutate(group = transect) %>% 
  filter(weight_group == "> 10g") %>% make_inext_file()
# Habitat
inext_trans_arboreal <- amph_data %>% mutate(group = transect) %>% 
  filter(habitat == "Arboreal") %>% make_inext_file()
inext_trans_terrest <- amph_data %>% mutate(group = transect) %>% 
  filter(habitat == "Terrestrial") %>% make_inext_file()
inext_trans_semiarb <- amph_data %>% mutate(group = transect) %>% 
  filter(habitat == "Semiarboreal") %>% make_inext_file()

##########################################################.
# Data for beta diversity similarity analysis
simil_basedata <- amph_data %>% group_by(species, site) %>% count() %>% 
  spread(species, n)  %>% mutate_all(funs(replace(., is.na(.), 0))) %>% 
  ungroup() %>% column_to_rownames(., "site") 

##########################################################.
## Alpha diversity analisys ----
##########################################################.
# running alpha diversity analysis. 
# Endpoint double minimum sample size (Colwell et al.)
# q0 q1 q2 = richness shannon simpson
##########################################################.
# Diversity by band: total and by functional category
band_div_total <- iNEXT(inext_band, q=c(0,1,2), datatype="abundance", endpoint = 132)
band_div_repro <- iNEXT(inext_band_repro, q=c(0,1,2), datatype="abundance")
band_div_weight <- iNEXT(inext_band_weight, q=c(0,1,2), datatype="abundance")
band_div_habitat <- iNEXT(inext_band_habitat, q=c(0,1,2), datatype="abundance")

##########################################################.
# Diversity by transect: total and by functional category
transect_div_total <- iNEXT(inext_transect, q=c(0,1,2), datatype="abundance")
# Reproduction groups
transect_div_water <- iNEXT(inext_trans_water, q=c(0,1,2), datatype="abundance")
transect_div_otherrep <- iNEXT(inext_trans_otherrep, q=c(0,1,2), datatype="abundance")
# Weight groups
transect_div_less2 <- iNEXT(inext_trans_less2, q=c(0,1,2), datatype="abundance")
transect_div_2to10 <- iNEXT(inext_trans_2to10, q=c(0,1,2), datatype="abundance")
transect_div_more10 <- iNEXT(inext_trans_more10, q=c(0,1,2), datatype="abundance")
# Habitat
transect_div_arboreal <- iNEXT(inext_trans_arboreal, q=c(0,1,2), datatype="abundance")
transect_div_terrest <- iNEXT(inext_trans_terrest, q=c(0,1,2), datatype="abundance")
transect_div_semiarb <- iNEXT(inext_trans_semiarb, q=c(0,1,2), datatype="abundance")

##########################################################.
## Preparing files for modelling 
# Obtaining files with diversity by transect for total and functional categories
save_model_file(transect_div_total, "modeldata_tot")
# Reproduction groups
save_model_file(transect_div_water, "modeldata_water")
save_model_file(transect_div_otherrep, "modeldata_otherrep")
# Weight groups
save_model_file(transect_div_less2, "modeldata_less2")
save_model_file(transect_div_2to10, "modeldata_2to10")
save_model_file(transect_div_more10, "modeldata_more10")
# Habitat
save_model_file(transect_div_arboreal, "modeldata_arboreal")
save_model_file(transect_div_terrest, "modeldata_terrest")
save_model_file(transect_div_semiarb, "modeldata_semiarb")

##########################################################.
## Beta diversity ----
##########################################################.
# From package vegan: Chao-Jaccard Estimated Abundance Similarity
# Produces dissimilarity, so converting into similarity
simil_chao <- vegdist(simil_basedata, method = "chao") %>% -1 %>% abs() %>%
  round(.,2) %>% as.matrix() %>% as.data.frame() #needed to be saved as csv

write_csv2(simil_chao, "Results/diversity/simil_chao.csv")

##########################################################.
## Plotting rarefaction ----
##########################################################.
# Preparing data
rarefaction_data <- combine_inext(band_div_total, "band") %>% 
  filter(order == 0) #selecting species richness

# used to create the continuous line
raref_non_extrap <- rarefaction_data %>% filter(method == "interpolated")
# used to create the extrapolation point
raref_extrappoint <- rarefaction_data %>% filter(method == "observed")

#export in format requested by journal
raref_pal <- c("#d7191c", "#fdae61", "#3690c0", "#034e7b") #palette

#this starts the saving the plot command
jpeg(file="rarefaction_plot_jan19.jpeg",width=190,height=100,
     units="mm",res=1000, pointsize=12) 

# code for the plot
ggplot(rarefaction_data, aes(x = m, y = qd, group = altura)) +
  geom_ribbon(aes(ymin = qd.lcl, ymax = qd.ucl, color=altura, 
                  fill = altura), alpha = 0.2, show.legend=F, linetype=0) +
  geom_line(aes(color=altura),lwd=1.5, linetype = "dotted") +
  geom_line(data = raref_non_extrap, aes(y = qd, color=altura),lwd=1.5) +
  geom_point(data = raref_extrappoint, aes(y = qd, color = altura, size = 2),
             show.legend = F) +
  scale_color_manual(values=raref_pal, name = "Altitudinal band") + #color scales line
  scale_fill_manual(values=raref_pal, name = "") + #color scales ribbon
  labs(x = "Number of individuals", y = "Number of species"  ) + # axis titles
  scale_x_continuous(expand = c(0, 0)) + # to force axes to start at 0
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 5, 10, 15, 20, 25)) + #for labels in y axes
  #Layout
  theme(text = element_text(size=20),
        axis.ticks = element_blank(), #taking out ticks from axis
        axis.text=element_text(size=16), #axis labels size
        axis.line = element_line(), #adding lines for both axis
        legend.key=element_blank(), #taking out background from legend
        panel.grid.major = element_line(colour="#F0F0F0"),#grid lines
        panel.background = element_blank() #Blanking background
  )

dev.off() #this finishes the saving the plot command

## END