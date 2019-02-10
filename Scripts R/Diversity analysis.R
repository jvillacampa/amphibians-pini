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
# What if reichlei and cf.reichlei were the same species?
simil_basedata_nodanae <- amph_data %>% 
  mutate(species = recode(species, "Pristimantis danae" = "Pristimantis reichlei")) %>% 
  group_by(species, site) %>% count() %>% 
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
# survey completeness as an expression of observed/estimated
select(band_div_total[["DataInfo"]], site, SC)
survey_compl <- band_div_total[["AsyEst"]] %>% setNames(tolower(names(.))) %>% 
  mutate(survey_compl = observed/estimator *100)

# Reproduction groups
band_div_water <- iNEXT(inext_band_water, q=c(0,1,2), datatype="abundance")
band_div_otherrep <- iNEXT(inext_band_otherrep, q=c(0,1,2), datatype="abundance")
# Weight groups
band_div_less2 <- iNEXT(inext_band_less2, q=c(0,1,2), datatype="abundance")
band_div_2to10 <- iNEXT(inext_band_2to10, q=c(0,1,2), datatype="abundance")
band_div_more10 <- iNEXT(inext_band_more10, q=c(0,1,2), datatype="abundance")
# Habitat
band_div_arboreal <- iNEXT(inext_band_arboreal, q=c(0,1,2), datatype="abundance")
band_div_terrest <- iNEXT(inext_band_terrest, q=c(0,1,2), datatype="abundance")
band_div_semiarb <- iNEXT(inext_band_semiarb, q=c(0,1,2), datatype="abundance")

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

# What if reichlei and cf.reichlei were the same
simil_chao_nodanae <- vegdist(simil_basedata_nodanae, method = "chao") %>% -1 %>% abs() %>%
  round(.,2) %>% as.matrix() %>% as.data.frame() #needed to be saved as csv

write_csv2(simil_chao_nodanae, "Results/diversity/simil_chao_nodanae.csv")

##########################################################.
## Plotting rarefaction -figure1 ----
##########################################################.
# Preparing data
rarefaction_data <- combine_inext(band_div_total, "band") %>% 
  filter(order == 0) #selecting species richness

# used to create the continuous line
raref_non_extrap <- rarefaction_data %>% filter(method == "interpolated")
# used to create the extrapolation point
raref_extrappoint <- rarefaction_data %>% filter(method == "observed")

#export in format requested by journal
#this starts the saving the plot command
jpeg(file="Results/fig1_rarefaction_plot.jpeg",width=190,height=100,
     units="mm",res=1000, pointsize=12) 

# code for the plot
ggplot(rarefaction_data, aes(x = m, y = qd, group = altura)) +
  geom_ribbon(aes(ymin = qd.lcl, ymax = qd.ucl, color=altura, 
                  fill = altura), alpha = 0.2, show.legend=F, linetype=0) +
  geom_line(aes(color=altura),lwd=1.5, linetype = "dotted") +
  geom_line(data = raref_non_extrap, aes(y = qd, color=altura),lwd=1.5) +
  geom_point(data = raref_extrappoint, aes(y = qd, color = altura, size = 2),
             show.legend = F) +
  scale_color_manual(values=band_pal, name = "Altitudinal band") + #color scales line
  scale_fill_manual(values=band_pal, name = "") + #color scales ribbon
  labs(x = "Number of individuals", y = "Number of species"  ) + # axis titles
  scale_x_continuous(expand = c(0, 0)) + # to force axes to start at 0
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 5, 10, 15, 20, 25)) + #for labels in y axes
  #Layout
  theme(text = element_text(size=20),
        axis.ticks = element_blank(), #taking out ticks from axis
        axis.text=element_text(size=16, colour = "black"), #axis labels size
        axis.line = element_line(), #adding lines for both axis
        legend.key=element_blank(), #taking out background from legend
        panel.grid.major = element_line(colour="#F0F0F0"),#grid lines
        panel.background = element_blank() #Blanking background
  )

dev.off() #this finishes the saving the plot command

##########################################################.
## Plotting q1, q2, abundance - figure 2 ----
##########################################################.
# Preparing data for figure
fig2_data <- combine_inext(band_div_total, "band") %>% 
  filter(method == "observed" & order != "0") 

abundance_fig2 <- fig2_data %>% select(m, altura) %>% mutate(order = "Abundance") %>% 
  unique() %>% rename(qd = m)

# Creating theme for plots in fig2
fig2_theme <- theme(text = element_text(size=20),
      axis.text=element_text(size=16, colour = "black"), #axis labels size
      axis.title.x = element_blank(), #no axis titles
      axis.line = element_line(), #adding lines for both axis
      legend.position = "none",
      panel.grid.major = element_line(colour="#F0F0F0"),#grid lines
      plot.margin = margin(l=1, r=1, unit = "cm"),
      panel.background = element_blank() #Blanking background
    )

# Plot for q1
fig2_plot_q1 <- ggplot(data=fig2_data %>% filter(order == 1), 
                       aes(x=altura, y =qd, color = altura)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=qd.lcl, ymax=qd.ucl), width = 0.3, size= 1.5) +
  scale_color_manual(values=band_pal, name = "Altitudinal band: ") + #color scales 
  labs(y ="Shannon's entropy") + fig2_theme

# Plot for q2
fig2_plot_q2 <- ggplot(data=fig2_data %>% filter(order == 2), 
                       aes(x=altura, y =qd, color = altura)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=qd.lcl, ymax=qd.ucl), width = 0.3, size= 1.5) +
  scale_color_manual(values=band_pal, name = "Altitudinal band: ") + #color scales 
  labs(y ="Simpson's diversity") + fig2_theme

# Plot for abundance
fig2_plot_abund <- ggplot() +
  geom_col(data=abundance_fig2, aes(x=altura, y =qd, fill = altura)) +
  scale_fill_manual(values=band_pal, name = "Altitudinal band: ") + #fill colors
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110))+ #axis starting at 0
  labs(y ="Number of individuals") + fig2_theme

# Creating final plot combining previous 3
fig2_arranged <- ggarrange(fig2_plot_abund, fig2_plot_q1, fig2_plot_q2, ncol=3, common.legend = TRUE, 
                           legend="bottom", labels = c("A", "B", "C"))
# Saving plot
ggsave("Results/fig2_q1q2abun.jpeg", plot= fig2_arranged, device = "jpeg", dpi=1000,
       width =20, height = 4)

##########################################################.
## Plotting functional group richness, abundance - figure 3 ----
##########################################################.
# Preparing data for figure
fig3_weight_data <- rbind(#combining weight group inext results
    combine_inext(band_div_less2, "band", "< 2.5g") %>% mutate(group = "< 2.5g"),
    combine_inext(band_div_2to10, "band", "2.5 - 10g") %>% mutate(group = "2.5 - 10g"),
    rbind(
      band_div_more10[["iNextEst"]][["B500> 10g"]] %>% mutate(altura = "450 - 550"),
      band_div_more10[["iNextEst"]][["B700> 10g"]] %>% mutate(altura = "650 - 750"),
      band_div_more10[["iNextEst"]][["B1100> 10g"]] %>% mutate(altura = "1050 - 1150")) %>% 
      mutate(altura = factor(altura, levels = c("450 - 550", "650 - 750", 
                                              "850 - 950", "1050 - 1150"))) %>% 
      setNames(tolower(names(.))) %>% mutate(group = "> 10g")) %>% 
  mutate(cat = "weight") %>% 
  filter(method == "observed" & order == "0") 

abundance_fig3_weight <- fig3_weight_data %>% select(m, altura, group, cat) %>% 
  mutate(order = "Abundance") %>% unique() %>% rename(qd = m)

# For habitat
fig3_hab_data <- rbind(#combining habitat group inext results
    combine_inext(band_div_semiarb, "band", "Semiarboreal") %>% mutate(group = "semiarb"),
    combine_inext(band_div_terrest, "band", "Terrestrial") %>% mutate(group = "terr"),
    rbind(
      band_div_arboreal[["iNextEst"]][["B500Arboreal"]] %>% mutate(altura = "450 - 550"),
      band_div_arboreal[["iNextEst"]][["B700Arboreal"]] %>% mutate(altura = "650 - 750")) %>% 
      mutate(altura = factor(altura, levels = c("450 - 550", "650 - 750", 
                                                "850 - 950", "1050 - 1150"))) %>% 
      setNames(tolower(names(.))) %>% mutate(group = "arb")) %>% 
  mutate(cat = "habitat") %>% 
  filter(method == "observed" & order == "0") 

abundance_fig3_hab <- fig3_hab_data %>% select(m, altura, group, cat) %>% 
  mutate(order = "Abundance") %>% unique() %>% rename(qd = m)

# For reproductive habitat
fig3_repr_data <- rbind(#combining habitat group inext results
  combine_inext(band_div_otherrep, "band", "Other") %>% mutate(group = "otherrep"),
  rbind(
    band_div_water[["iNextEst"]][["B500Bodies of water"]] %>% mutate(altura = "450 - 550"),
    band_div_water[["iNextEst"]][["B700Bodies of water"]] %>% mutate(altura = "650 - 750"),
    band_div_water[["iNextEst"]][["B1100Bodies of water"]] %>% mutate(altura = "1050 - 1150")) %>%  
    mutate(altura = factor(altura, levels = c("450 - 550", "650 - 750", 
                                              "850 - 950", "1050 - 1150"))) %>% 
    setNames(tolower(names(.))) %>% mutate(group = "water")) %>% 
  mutate(cat = "repr") %>% 
  filter(method == "observed" & order == "0") 

abundance_fig3_repr <- fig3_repr_data %>% select(m, altura, group, cat) %>% 
  mutate(order = "Abundance") %>% unique() %>% rename(qd = m)

##########################################################.
# Plots
# put lines between charts. include 0s for absent groups
# one dataset per category, levels for each cat
# create function for plotting
# what about common y axis
# group names need fixing
fig3_weight_q0 <- ggplot(data=fig3_weight_data, 
                       aes(x=altura, y =qd, color = altura)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=qd.lcl, ymax=qd.ucl), width = 0.3, size= 1.5) +
  facet_wrap(.~ group, nrow =1) +
  scale_color_manual(values=band_pal, name = "Altitudinal band: ") + #color scales 
  labs(y ="Number of species") + fig2_theme

fig3_hab_q0 <- ggplot(data=fig3_hab_data, 
                         aes(x=altura, y =qd, color = altura)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=qd.lcl, ymax=qd.ucl), width = 0.3, size= 1.5) +
  facet_wrap(.~ group, nrow =1) +
  scale_color_manual(values=band_pal, name = "Altitudinal band: ") + #color scales 
  labs(y ="Number of species") + fig2_theme

fig3_repr_q0 <- ggplot(data=fig3_repr_data, 
                         aes(x=altura, y =qd, color = altura)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=qd.lcl, ymax=qd.ucl), width = 0.3, size= 1.5) +
  facet_wrap(.~ group, nrow =1) +
  scale_color_manual(values=band_pal, name = "Altitudinal band: ") + #color scales 
  labs(y ="Number of species") + fig2_theme

##########################################################
# Joining plots

ggarrange(fig3_repr_q0, fig3_hab_q0+ rremove("ylab") + rremove("y.axis"), fig3_weight_q0, ncol=3, common.legend = TRUE, 
          legend="bottom", labels = c("A", "B", "C"))


## END