# In this script, running multivariate analysis - principal component analysis

##########################################################.
## Data ----
##########################################################.
# bringing packages, basefile data and functions
source("Scripts R/Functions and packages.R")

pca_veg_data <- veg_mapping %>% column_to_rownames(var="transecto") %>% 
  # taking out temperature and humidity, just focusing on forest structure
  # temp and hum only value per band
  select(-temp, -hum) %>% 
  # Renaming variables to make plotting easier
  rename("Canopy height" = altdosel, "Canopy cover" = cobdosel, "Leaf litter depth" = hojarasca,
         "Understory density" = densidad, "Epiphyte abundance" = epifitas, "N trees DBH>10" = arboles,
         "Necromass abundance" = necromasa, "Fern abundance" = helechos, "Slope" = pendiente)

##########################################################.
## PCA and plot ----
##########################################################.
# rda command without formula/species matrix, is an rda
pca_vegmap <- rda(pca_veg_data)

# Obtain information for supplementary information tables
pca_vegmap
summary(pca_vegmap)
print(pca_vegmap)

# Preparing data for plot
#extract environmental variables and site scores from rda summary
scores_transects <- scores(pca_vegmap, display = c("sites")) %>% as.data.frame() %>% 
  rownames_to_column(var = "transect") %>% 
  mutate(site = substr(transect, 1, unlist(gregexpr("\\-", transect)) - 1),
         band = factor(recode(site, "500" = "450 - 550", "700" = "650 - 750", 
                       "900" = "850 - 950", "1100" = "1050 - 1150"), 
                       levels = c("450 - 550", "650 - 750", 
                                  "850 - 950", "1050 - 1150"))) %>% 
  setNames(tolower(names(.)))

scores_envvar <- scores(pca_vegmap, display = c("species")) %>% as.data.frame() %>% 
  rownames_to_column(var = "env_var") %>% setNames(tolower(names(.))) %>% 
  # To avoid overlap of labels
  mutate(arrow_pc2 = case_when(pc2<0 & !(env_var %in% c("Leaf litter depth", "Epiphyte abundance")) ~ pc2-0.07, 
                               env_var == "Leaf litter depth" ~ pc2-0.07,
                               env_var == "Epiphyte abundance" ~ pc2+0.1,
                               pc2>0 ~ pc2 + 0.1))

##########################################################.
# Plotting PCA
#this starts the saving the plot command
jpeg(file="Results/pca_plot_jan19.jpeg",width=190,height=100,
     units="mm",res=1000, pointsize=12) 
# create palette of shapes

ggplot() +
  geom_point(data = scores_transects, aes(x = pc1, y=pc2, shape = band, color = band)) +
  geom_segment(data = scores_envvar, arrow = arrow(length = unit(0.3,"cm")),
               aes(x=0, xend=pc1, y=0, yend=pc2)) +
  geom_text(data = scores_envvar, aes(label = env_var, x = pc1, y=arrow_pc2)) +
  geom_vline(aes(xintercept=0), linetype= "dashed") + # to create "grid lines"
  geom_hline(aes(yintercept=0), linetype= "dashed") +
  scale_shape_manual(values=c(16, 8, 17, 15), name = "Altitudinal band") + #shape values
  scale_color_manual(values= c("#d7191c", "#fdae61", "#3690c0", "#034e7b"), 
                    name = "Altitudinal band") + #color scales 
  #Layout
  labs(x="PC1 (34.3%)", y="PC2 (16.8%)") + # axis titles
  theme(text = element_text(size=18), #font
        axis.text=element_text(size=16, colour = "black"), #axis labels size
        legend.key=element_blank(), #taking out background from legend
        panel.border = element_rect(colour = "black", fill=NA), 
        panel.background = element_blank() #Blanking background
  )

dev.off() # finish saving plot

## END