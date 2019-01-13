#Estos son los archivos con el logaritmo sobre el porcentaje de la abundancia relativa ya hechos
Logabund500 <- read.delim("Datos/abundancias/Logabund500.txt", dec=",")
Logabund700 <- read.delim("Datos/abundancias/Logabund700.txt", dec=",")
Logabund900 <- read.delim("Datos/abundancias/Logabund900.txt", dec=",")
Logabund1100 <- read.delim("Datos/abundancias/Logabund1100.txt", dec=",")

#Estos son los archivos con la abundancia bruta por banda
Abun500 <- read.delim("Datos/abundancias/Abun500.txt", dec=",")
Abun700 <- read.delim("Datos/abundancias/Abun700.txt", dec=",")
Abun900 <- read.delim("Datos/abundancias/Abun900.txt", dec=",")
Abun1100 <- read.delim("Datos/abundancias/Abun1100.txt", dec=",")
Abuntotal <- read.delim("Datos/abundancias/Abuntotal.txt", dec=",")

#Archivo de abundancias relativas sin transformaciones y el segundo en %
Abuntot.rel <- read.delim("Datos/abundancias/Abuntot.rel.txt", dec=",")
Porc.abun<-read.delim("Datos/abundancias/Porc.abun.txt", dec=",")

#Como las curvas de cada modelo se ajustan a la estructura de ranks
Mod.abun.rel<-radfit(Abuntot.rel, family=Gamma)
Mod.abun.rel

Mod.porc.abun<-radfit(Porc.abun, family=Gamma)
Mod.porc.abun

plot(Mod.abun.rel)
plot(Mod.porc.abun)

#De esto se saca que para las 4 bandas se ajusta mejor el modelo zipfbrot. Por lo que funci?n rad.zipfbrot
whit500<- rad.zipfbrot(Abuntot.rel[1,], family=Gamma)
whit500
whit700<- rad.zipfbrot(Abuntot.rel[2,], family=Gamma)
whit700
whit900<- rad.zipfbrot(Abuntot.rel[3,], family=Gamma)
whit900
whit1100<- rad.zipfbrot(Abuntot.rel[4,], family=Gamma)
whit1100

whit500p<- rad.zipfbrot(Porc.abun[1,], family=Gamma)
whit500p
whit700p<- rad.zipfbrot(Porc.abun[2,], family=Gamma)
whit700p
whit900p<- rad.zipfbrot(Porc.abun[3,], family=Gamma)
whit900p
whit1100p<- rad.zipfbrot(Porc.abun[4,], family=Gamma)
whit1100p

######

#para hacer ancovas
#primero creas un data frame con el tamaño de tus especies (suma de toda la riqueza de cada uno, para eso haces lenght)
#luego se mete toda la info de los modelos
ancwhit <- data.frame(TYPE=rep("NA", 44), NUM=rep("NA",44), ABUN=rep("NA",44))
ancwhit$TYPE <- as.character(ancwhit$TYPE)
ancwhit$NUM <- as.numeric(ancwhit$NUM)
ancwhit$ABUN <- as.numeric(ancwhit$ABUN)

ancwhit$TYPE[1:16] <- rep("whit500", 16)
ancwhit$NUM[1:16] <- 1:16
ancwhit$ABUN[1:16] <- whit500$y

ancwhit$TYPE[17:28] <- rep("whit700", 12)
ancwhit$NUM[17:28] <- 17:28
ancwhit$ABUN[17:28] <- whit700$y

ancwhit$TYPE[29:37] <- rep("whit900", 9)
ancwhit$NUM[29:37] <- 29:37
ancwhit$ABUN[29:37] <- whit900$y

ancwhit$TYPE[38:44] <- rep("whit1100", 7)
ancwhit$NUM[38:44] <- 38:44
ancwhit$ABUN[38:44] <- whit1100$y

ancwhit$TYPE <- as.factor(ancwhit$TYPE)
ancwhit
plot(log(ancwhit$ABUN) ~ ancwhit$NUM, col=ancwhit$TYPE) 
#Base comparison (comparing all the bands)

# Run a linear model with an interaction between NUM (species rank) and TYPE (butterflyanc type)
summary(lm(log(ABUN)~NUM*TYPE, data=ancwhit))
# Look at the effects table (ignore the single effects and just look at the interactions (NUM:TYPE terms)
# The base factor is whit1100, so the P values given are relating to wether the gradient of whit500, 700 and 900 are different to 1100
# *** means highly significant, as the estimate is negative, it means the composition is less even
# You can't compare 500, 700 and 900 with the rest this way so you need to relevel

# Relevel to compare 500 with the rest
summary(lm(log(ABUN)~NUM*relevel(ancwhit$TYPE, ref=2), data=ancwhit))
# and now for whit 700
summary(lm(log(ABUN)~NUM*relevel(ancwhit$TYPE, ref=3), data=ancwhit))
# Boom. Done. 

####
#gráficos
plot(whit500, type="n",xaxt="n",xlab="Rank", ylab="Relative abundance", xlim=c(0,45),ylim=c(0.005,1.5), main="Dominance/abundance", cex.axis=0.7, las=1)

#locator()
text(4, 2,labels=expression("450-550"))
text(18, 0.9,labels=expression("650-750"))
text(30, 0.9,labels=expression("850-950"))
text(40, 0.9,labels=expression("1050-1150"))

text(22.5, 0.002,labels=expression("Rank"))
#prueba con gg plot
ggplot()
# Puntos y luego l?neas
# 500
length(whit500$y) #16
points(0:15, whit500$y, pch=19, col="black")
lines(0:15, whit500$fitted.values)

# 700
length(whit700$y) #12
points(16:27, whit700$y, pch=19, col="black", cex=1)
lines(16:27, whit700$fitted.values)

# 900
length(whit900$y) #9
points(28:36, whit900$y, pch=19, col="black")
lines(28:36, whit900$fitted.values)

# 1100
length(whit1100$y) #7
points(37:43, whit1100$y, pch=19, col="black")
lines(37:43, whit1100$fitted.values)

# texto: nombres especies
text(0:15,whit500$y, labels=paste(substring(attributes(whit500$y)$names , 1,2),substring(attributes(whit500$y)$names , 3,3), sep = ""), pos=3, offset=0.5, cex=0.5, font=2)
text(16:27,whit700$y, labels=paste(substring(attributes(whit700$y)$names , 1,2),substring(attributes(whit700$y)$names , 3,3), sep = ""), pos=3, offset=0.5, cex=0.5, font=2)
text(28:36,whit900$y, labels=paste(substring(attributes(whit900$y)$names , 1,2),substring(attributes(whit900$y)$names , 3,3), sep = ""), pos=3, offset=0.5, cex=0.5, font=2)
text(37:43,whit1100$y, labels=paste(substring(attributes(whit1100$y)$names , 1,2),substring(attributes(whit1100$y)$names , 3,3), sep = ""), pos=3, offset=0.5, cex=0.5, font=2)

#con el de porcentajes

plot(whit500p, type="n",xaxt="n",xlab="", ylab="Abundancia relativa (%)", xlim=c(0,45),ylim=c(0.5,150), main="", cex.axis=0.7, las=1)

#locator()
text(4, 110,labels=expression("450-550"))
text(18, 110,labels=expression("650-750"))
text(30, 110,labels=expression("850-950"))
text(40, 110,labels=expression("1050-1150"))

text(22.5, 0.3,labels=expression("Rango"))

# Puntos y luego l?neas
# 500
length(whit500p$y) #16
points(0:15, whit500p$y, pch=1, col="blue")
lines(0:15, whit500p$fitted.values)

# 700
length(whit700p$y) #12
points(16:27, whit700p$y, pch=1, col="green")
lines(16:27, whit700p$fitted.values)

# 900
length(whit900p$y) #9
points(28:36, whit900p$y, pch=1, col="red")
lines(28:36, whit900p$fitted.values)

# 1100
length(whit1100p$y) #7
points(37:43, whit1100p$y, pch=1, col="orange")
lines(37:43, whit1100p$fitted.values)

# texto
text(0:15,whit500p$y, labels=paste(substring(attributes(whit500p$y)$names , 1,2),substring(attributes(whit500p$y)$names , 3,3), sep = ""), pos=3, offset=0.5, cex=0.5, font=2)
text(16:27,whit700p$y, labels=paste(substring(attributes(whit700p$y)$names , 1,2),substring(attributes(whit700p$y)$names , 3,3), sep = ""), pos=3, offset=0.5, cex=0.5, font=2)
text(28:36,whit900p$y, labels=paste(substring(attributes(whit900p$y)$names , 1,2),substring(attributes(whit900p$y)$names , 3,3), sep = ""), pos=3, offset=0.5, cex=0.5, font=2)
text(37:43,whit1100p$y, labels=paste(substring(attributes(whit1100p$y)$names , 1,2),substring(attributes(whit1100p$y)$names , 3,3), sep = ""), pos=3, offset=0.5, cex=0.5, font=2)



# Add comparison bars

lines(c(0.5,16), c(0.9,0.9)) ; lines(c(16,16), c(0.7, 1.1)) ; lines(c(0.5,0.5), c(0.7, 0.9))
lines(c(16,40), c(1.1, 1.1))
lines(c(40,40), c(0.8, 1.1)) 

# Add results
# add 0.05 for text
#locator()
text(8,1.1, expression(paste(Delta,"G = -0.008, p = <0.001***")), cex=0.8, font=2)
text(28,1.4, expression(paste(Delta,"G = -0.008, p = <0.001***")), cex=0.8, font=2)
