#Script para plot rarefaction curves
########################.
#Packages and data ----
########################.
library(ggplot2)
library(dplyr)

#data with all the bands
rarefaction_data <- read.delim("Results/Rarefaccion/Rarefacc-R.txt", dec=",") %>% 
  setNames(tolower(names(.))) %>% #variable names to lower case
  rename(rich_lowci = s.est..95..ci.lower.bound, rich_upci = s.est..95..ci.upper.bound, 
         rich_sd = s.est..sd, richness = s.est., individuals = individuals..computed.) %>% 
  mutate(altura = recode(as.factor(altura), "500" = "450 - 550", "700" = "650 - 750",
                         "900" = "850 - 950", "1100" = "1050 - 1150"))
# used to create the continuous line
raref_non_extrap <- rarefaction_data %>% filter(samples < 51)
# used to create the extrapolation point
raref_extrappoint <- rarefaction_data %>% filter(samples == 51)
  
#data for each band
# rar1100 <- read.delim("./Datos mios/Rarefaccion/1100extra2.txt", dec=",")
# rar900 <- read.delim("./Datos mios/Rarefaccion/900extra2-R.txt", dec=",")
# rar700 <- read.delim("./Datos mios/Rarefaccion/700extra2-R.txt", dec=",")
# rar500 <- read.delim("./Datos mios/Rarefaccion/500extra2-R.txt", dec=",")

########################.
# Using ggplot ----
########################.
#export in format requested by journal
raref_pal <- c("#d7191c", "#fdae61", "#3690c0", "#034e7b") #palette

#this starts the saving the plot command
jpeg(file="./rarefaction_plot_dec18.jpeg",width=190,height=100,
     units="mm",res=1000, pointsize=12) 

# code for the plot
ggplot(rarefaction_data, aes(x = individuals, y = richness, group = altura)) +
  geom_ribbon(aes(ymin = rich_lowci, ymax = rich_upci,color=altura, 
                  fill = altura), alpha = 0.2, show.legend=F, linetype=0) +
  geom_line(aes(color=altura),lwd=1.5, linetype = "dotted") +
  geom_line(data = raref_non_extrap, aes(y = richness, color=altura),lwd=1.5) +
  geom_point(data = raref_extrappoint, aes(y = richness, color = altura, size = 2),
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

########################.
#attempt 1 ----
########################.
#These are attempts from the time of doing my thesis
# source("http://www.jennajacobs.org/R/rarefaction.txt")
# RarePini<-rarefaction(Raref)
# points(RarePini$row.names, RarePini$richness.500, RarePini$richness.700, RarePini$richness.900, RarePini$richness.1100, main="Amphibians", xlab="Individuals", ylab="Species Accumulation", type="n", ylim=c(0,35), xlim=c(0,500), las=1, cex.axis=0.7, cex.lab=0.8, xaxt="n")

########################.
#attempt 2 ----
########################.
#These are attempts from the time of doing my thesis
#Las 4 curvas a la vez
# plot(Rar500$Individuals, Rar500$Riqueza, type="n",xaxt="n",xlab="", ylab="Riqueza de especies", xlim=c(0,135),ylim=c(0,25), main="", cex.axis=0.7, las=1)
# axis(side=1, at=c(seq(0,125,25)), labels=c(0,25,50,75,100,125), cex.axis=0.7,)
# text(68, -10,labels=expression("Individuos"))
# 
# lines(Rar500$Individuals, Rar500$Riqueza, col="blue", lty=1, lwd=3)
# lines(Rar700$Individuals, Rar700$Riqueza, col="green", lty=1, lwd=3)
# lines(Rar900$Individuals, Rar900$Riqueza, col="red", lty=1, lwd=3)
# lines(Rar1100$Individuals, Rar1100$Riqueza, col="orange", lty=1, lwd=3)
# #para poner los puntos de comienzo de la extrapolaci?n
# points(86,16, col="blue", type="o", pch=19, cex=1.2)
# points(109,12, col="green", type="o", pch=19, cex=1.2)
# points(76,9, col="red", type="o", pch=19, cex=1.2)
# points(66,7, col="orange", type="o", pch=19, cex=1.2)
# 
# #500
# plot(Rar500$Individuals, Rar500$Riqueza, type="n",xaxt="n",xlab="", ylab="Riqueza de especies", xlim=c(0,135),ylim=c(0,28), main="", cex.axis=0.7, las=1)
# text(68, 32,labels=expression("450-550"))
# axis(side=1, at=c(seq(0,125,25)), labels=c(0,25,50,75,100,125), cex.axis=0.7,)
# lines(Rar500$Individuals, Rar500$Riqueza, col="blue", lty=1, lwd=3)
# polygon(c(Rar500$Individuals, rev(Rar500$Individuals)), c(Rar500$Limsup.riqueza, rev(Rar500$Liminf.riqueza)), col="#0000ff22", border = NA)
# points(86,16, col="blue", type="o", pch=19, cex=1.2)
# 
# #700
# plot(Rar500$Individuals, Rar500$Riqueza, type="n",xaxt="n",xlab="", yaxt="n",ylab="", xlim=c(0,135),ylim=c(0,28), main="", cex.axis=0.7, las=1)
# text(68, 32,labels=expression("650-750"))
# axis(side=1, at=c(seq(0,125,25)), labels=c(0,25,50,75,100,125), cex.axis=0.7,)
# polygon(c(Rar700$Individuals, rev(Rar700$Individuals)), c(Rar700$Limsup.riqueza, rev(Rar700$Liminf.riqueza)), col="#98FB98", border = NA)
# lines(Rar700$Individuals, Rar700$Riqueza, col="green", lty=1, lwd=3)
# points(109,12, col="green", type="o", pch=19, cex=1.2)
# 
# #900
# plot(Rar900$Individuals, Rar900$Riqueza, type="n",xaxt="n",xlab="", yaxt="n",ylab="", xlim=c(0,135),ylim=c(0,28), main="", cex.axis=0.7, las=1)
# text(68, 32,labels=expression("850-950"))
# axis(side=1, at=c(seq(0,125,25)), labels=c(0,25,50,75,100,125), cex.axis=0.7,)
# polygon(c(Rar900$Individuals, rev(Rar900$Individuals)), c(Rar900$Limsup.riqueza, rev(Rar900$Liminf.riqueza)), col="#FFA07A", border = NA)
# lines(Rar900$Individuals, Rar900$Riqueza, col="red", lty=1, lwd=3)
# points(76,9, col="red", type="o", pch=19, cex=1.2)
# 
# #1100
# plot(Rar1100$Individuals, Rar1100$Riqueza, type="n",xaxt="n",xlab="", yaxt="n",ylab="", xlim=c(0,135),ylim=c(0,28), main="", cex.axis=0.7, las=1)
# text(68, 32,labels=expression("1050-1150"))
# axis(side=1, at=c(seq(0,125,25)), labels=c(0,25,50,75,100,125), cex.axis=0.7,)
# polygon(c(Rar1100$Individuals, rev(Rar1100$Individuals)), c(Rar1100$Limsup.riqueza, rev(Rar1100$Liminf.riqueza)), col="#F0E68C", border = NA)
# lines(Rar1100$Individuals, Rar1100$Riqueza, col="orange", lty=1, lwd=3)
# points(66,7, col="orange", type="o", pch=19, cex=1.2)

########################.
#attempt 3 ----
########################.
#These are attempts from the time of doing my thesis
# attach(Rare500)
# plot(Individuals, Riqueza, main="450-550", xlab="Individuos", ylab="Acumulaci?n de especies", type="n", ylim=c(0,35), xlim=c(0,200), las=1, cex.axis=0.7, cex.lab=0.8, xaxt="n")
# axis(side=1, at=c(0,50,100,150,200), labels=c(0,50,100,150,200), cex.axis=0.7,)
# lines(Individuals, Riqueza, pch=22, lty=2)
