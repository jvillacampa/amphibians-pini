# Code to run diversity analysis

##########################################################.
## Data ----
##########################################################.
# bringing packages and functions
source("Scripts R/Functions and packages.R")

##########################################################.
# Preparing files needed for iNEXT diversity calculations
# Abundance by band: total and by functional category
inext_band <- amph_data %>% mutate(group = paste0("B", site)) %>% make_inext_file()

inext_band_repro <- amph_data %>% mutate(group = paste0("B", site, reproduction)) %>% 
  make_inext_file()

inext_band_weight <- amph_data %>% mutate(group = paste0("B", site, weight_group)) %>% 
  make_inext_file()

inext_band_habitat <- amph_data %>% mutate(group = paste0("B", site, habitat)) %>% 
  make_inext_file()

##########################################################.
# Abundance by transect: total and by functional category
inext_transect <- amph_data %>% mutate(group = transect) %>% make_inext_file()

inext_trans_repro <- amph_data %>% mutate(group = paste0("B", site, "_", transect, reproduction)) %>% 
  make_inext_file()

inext_trans_weight <- amph_data %>% mutate(group = paste0("B", site, "_", transect, weight_group)) %>% 
  make_inext_file()

inext_trans_habitat <- amph_data %>% mutate(group = paste0("B", site, "_", transect, habitat)) %>% 
  make_inext_file()

##########################################################.
## Diversity analisys ----
##########################################################.
# running diversity analysis. Endpoint double minimum sample size (Colwell et al.)
##########################################################.
# Diversity by band: total and by functional category
# extrapolating to double the band with less cases
band_div_total <- iNEXT(inext_band, q=c(0,1,2), datatype="abundance", endpoint = 132)
band_div_repro <- iNEXT(inext_band_repro, q=c(0,1,2), datatype="abundance")
band_div_weight <- iNEXT(inext_band_weight, q=c(0,1,2), datatype="abundance")
band_div_habitat <- iNEXT(inext_band_habitat, q=c(0,1,2), datatype="abundance")

##########################################################.
# Diversity by transect: total and by functional category
transect_div_total <- iNEXT(inext_transect, q=c(0,1,2), datatype="abundance")
transect_div_repro <- iNEXT(inext_trans_repro, q=c(0,1,2), datatype="abundance")
transect_div_weight <- iNEXT(inext_trans_weight, q=c(0,1,2), datatype="abundance")
transect_div_habitat <- iNEXT(inext_trans_habitat, q=c(0,1,2), datatype="abundance")

##########################################################.
## Preparing files for modelling 
# Obtaining files with diversity by transect for total and functional categories
save_model_file(transect_div_total, "modeldiv_tot")
save_model_file(transect_div_repro, "modeldiv_repro")
save_model_file(transect_div_weight, "modeldiv_weight")
save_model_file(transect_div_habitat, "modeldiv_habitat")

##########################################################.
## Similarity index ----
##########################################################.

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