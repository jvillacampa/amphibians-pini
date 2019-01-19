# Code to run models on diversity and abundance.

# Part 1 - Overall Diversity/abundance explained by environmental variables
# Part 2 - Band Diversity/abundance explained by environmental variables
# Part 3 - Diversity/abundance explained by altitude
# Part 4 - #Code to create shannon diversity graphs, figure 2

##########################################################.
## Data ----
##########################################################.
div_mod_tot <- readRDS("Datos/prepared_data/modeldiv_tot.rds")
div_mod_repro <- readRDS("Datos/prepared_data/modeldiv_repro.rds")
div_mod_weight <- readRDS("Datos/prepared_data/modeldiv_weight.rds")
div_mod_habitat <- readRDS("Datos/prepared_data/modeldiv_habitat.rds")

##########################################################.
## Diversity models ----
##########################################################.

##########################################################.
## Old method ----
##########################################################.
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

##########################################################.
## Overall Diversity/abundance explained by environmental variables ----
##########################################################.

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

##########################################################.
## Band Diversity/abundance explained by environmental variables ----
##########################################################.

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

##########################################################.
## Diversity/abundance models explained by altitude ----
##########################################################.

#modelos de comparación entre bandas de diversidad, abundancia y riqueza
#para cambiar orden de los factores para sus comparaciones
mod1<-lm(Abundancia~relevel(Varamb$Banda, ref="b"), data=Varamb)
summary(mod1)
#solo significativo entre 700 y 1100
#ahora diversidad
mod2<-lm(Diversidad~relevel(Varamb$Banda, ref="a"), data=Varamb)
summary(mod2)
plot(mod2)
#sin relevel para hacer exlusion
mod2.2<-lm(Diversidad~Banda, data=Varamb)
summary(mod2.2)
plot(mod2.2)
#punto jodienda en banda 700 solo 1 especie-1 encuentro
#prueba de exclusión relevel y excluir parecen no funcionar bien juntos. quitalo para correrlo
mod2.1<-update(mod2.2, data=Varamb[-21,])
summary(mod2.1)
plot(mod2.1)
#No parece alterar las relaciones 
#prueba con ggplot
ggplot(Varamb, aes(x = Diversidad, y = Abundancia, color=Banda))+
  geom_point()

#continúo con otras bandas
mod3<-lm(Diversidad~relevel(Varamb$Banda, ref="b"), data=Varamb)
summary(mod3)
plot(mod3)

mod4<-lm(Diversidad~relevel(Varamb$Banda, ref="c"), data=Varamb)
summary(mod4)

#ahora con riqueza
mod5<-lm(Riqueza~relevel(Varamb$Banda, ref="a"), data=Varamb)
summary(mod5)

mod6<-lm(Riqueza~relevel(Varamb$Banda, ref="b"), data=Varamb)
summary(mod6)

mod7<-lm(Riqueza~relevel(Varamb$Banda, ref="c"), data=Varamb)
summary(mod7)

##########################################################.
## Functional groups explained by altitude ----
##########################################################.


##########################################################.
## Code to create shannon diversity graphs, figure 2 ----
##########################################################.

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
