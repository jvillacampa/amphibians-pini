# In this script, looking at community structure with whittaker plots

##########################################################.
## Data ----
##########################################################.
# bringing packages, basefile data and functions
source("Scripts R/Functions and packages.R")

# Summarizing data for relative abundance by site
whit_abund <- amph_data %>% group_by(species_abr, site) %>% count() %>% ungroup %>% 
  group_by(site) %>% mutate(abund_rel = n/sum(n)) %>% ungroup() %>% #relative abundance
  select(species_abr, abund_rel, site) %>% 
  spread(species_abr, abund_rel) %>% #species as columns
  mutate_all(funs(replace(., is.na(.), 0))) %>% #nas to 0
  select(-site) %>% as.matrix()

#checking what model fits better the rank structure
radfit(whit_abund, family=Gamma) #mandelbrot fits better

# Running models for each band. Using family Gamma as they are proportions
whit500<- rad.zipfbrot(whit_abund[1,], family=Gamma)
whit500
whit700<- rad.zipfbrot(whit_abund[2,], family=Gamma)
whit700
whit900<- rad.zipfbrot(whit_abund[3,], family=Gamma)
whit900
whit1100<- rad.zipfbrot(whit_abund[4,], family=Gamma)
whit1100

######
# Preparing dataset with abundance, fitted values and species names
ancwhit <- data.frame(type=factor(c(rep("450 - 550", 16), rep("650 - 750", 12), 
                                    rep("850 - 950", 9), rep("1050 - 1150", 7)), 
                                  levels = c("450 - 550", "650 - 750", 
                                             "850 - 950", "1050 - 1150")), 
                      num=1:44, 
                      abun=c(whit500$y, whit700$y, whit900$y, whit1100$y),
                      slope = c(whit500$fitted.values, whit700$fitted.values, 
                                whit900$fitted.values, whit1100$fitted.values),
                      species = c(attributes(whit500$y)$names, attributes(whit700$y)$names,
                                  attributes(whit900$y)$names, attributes(whit1100$y)$names))

plot(log(ancwhit$abun) ~ ancwhit$num, col=ancwhit$type) #exploring ranks
#Base comparison (comparing all the bands)

# Run a linear model with an interaction between NUM (species rank) and TYPE (altitude)
summary(lm(log(abun)~num*type, data=ancwhit))
# Look at the effects table (ignore the single effects and just look at the interactions (NUM:TYPE terms)
# The base factor is 1100, so the P values given are relating to whether the 
# gradient of 1100, 700 and 900 are different to 500
# *** means highly significant, as the estimate is negative, it means the composition is less even
# You can't compare 1100, 700 and 900 with the rest this way so you need to relevel
# Relevel to compare 500 with the rest
summary(lm(log(abun)~num*relevel(ancwhit$type, ref="650 - 750"), data=ancwhit))
# and now for 900
summary(lm(log(abun)~num*relevel(ancwhit$type, ref="850 - 950"), data=ancwhit))

##########################################################.
## Plotting ranks ----
##########################################################.
ggplot(ancwhit, aes(x=num, color = type))+
  geom_point(aes(y=abun))+
  geom_text(aes(y=abun+(0.001), x=num, label = species, hjust =0),  colour = "black")+
  geom_line(aes(y=slope)) +
  scale_y_continuous(trans='log10') + #log scale
  labs(y="Relative abundance")+
  scale_color_manual(values=band_pal, name = "Altitudinal band: ") + #color scales 
    theme(text = element_text(size=20),
        axis.text.y=element_text(size=16, colour = "black"), #axis labels size
        axis.text.x=element_blank(), #no labels axis x
        axis.title.x = element_blank(), #no axis titles
        axis.ticks.x = element_blank(), #no ticks
        axis.line = element_line(), #adding lines for both axis
        panel.grid.major = element_line(colour="#F0F0F0"),#grid lines
        panel.background = element_blank() #Blanking background
  )

####
#Base R
plot(whit500, type="n",xaxt="n",xlab="Rank", ylab="Relative abundance", xlim=c(0,45),ylim=c(0.005,1.5), main="Dominance/abundance", cex.axis=0.7, las=1)

#Add band names and x axis
text(4, 0.9,labels=expression("450-550"))
text(18, 0.9,labels=expression("650-750"))
text(30, 0.9,labels=expression("850-950"))
text(40, 0.9,labels=expression("1050-1150"))
text(22.5, 0.002,labels=expression("Rank"))

# Species as points and then slope lines for each band
points(ancwhit$abun, pch=19, col="black")
lines(0:15, whit500$fitted.values)
lines(16:27, whit700$fitted.values)
lines(28:36, whit900$fitted.values)
lines(37:43, whit1100$fitted.values)
# Species names
text(ancwhit$abun, labels = ancwhit$species, pos=3, offset=0.5, cex=0.5, font=2)

# Add comparison bars 
lines(c(0.5,16), c(0.9,0.9)) ; lines(c(16,16), c(0.7, 1.1)) ; lines(c(0.5,0.5), c(0.7, 0.9))
lines(c(16,40), c(1.1, 1.1))
lines(c(40,40), c(0.8, 1.1)) 

# Add results
# add 0.05 for text
text(8,1.1, expression(paste(Delta,"G = -0.008, p = <0.001***")), cex=0.8, font=2)
text(28,1.4, expression(paste(Delta,"G = -0.008, p = <0.001***")), cex=0.8, font=2)

# END