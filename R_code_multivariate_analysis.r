#5/05/2022

library(raster)
library(RStoolbox) #contiene funzione per classificazione e per analisi multivariata
library(ggplot2)#per ggplot
library(patchwork)


setwd("c:/lab/")

#misura della varaibilita su una ariabile, qunid si deve scegliere una delle bande 
#per scegliere la migliore o uso una banda di cui ho fiducia (es. NIR) oppure calcolarmi indici spettrali
#(es. NDVI) opure posso compattare tutto in una sola banda (analsisi multivariata)
#e prendere solo la prima banda

# ANALISI MULTI VARIATA

#COME PASSARE DA UN IMMAGINE MULTI DIMASIONALE A UNI DIMENSIONALE
#vari tipi di analisi multivariata 

#come pasare da un immagine a più dimensioni ad una a una dimensione?
#se io mi trovo nella bi dimensione non concepisco le terza manon è detto che degli elemnti non lo siano solo
#che non li vediamo come tali
#ipercubo nel quadro di dalì
#la prima banda è quella con più info e quindi usiamo quella per il calcolo della variabilità

p224r63_2011 <- brick("p224r63_2011_masked.grd")
plot(p224r63_2011) #sistema a 7 bande l'immagine lendsat

#prima di analisi multivariata facciamo il ricampionamento o resambling
#diminuisco la risoluzione della mia immaine con la mouving winfow, aggreggo dei pizel 
# e ne formo con meno dettaglio. la funzione per fare quetso è detta AGGREGATE
#fa dei summary statistics. fact sta per factor cioè di quanto aggerghiamo i pixel 
#es. se è 10 compatto 10pixel x 10 pixel

#RICAMPIONAMENTO
p224r63_2011res <- aggregate(p224r63_2011, fact=10)
 
g1 <- ggRGB(p224r63_2011, 4, 3, 2)
g2 <- ggRGB(p224r63_2011res, 4, 3, 2)
g1+g2 # così vedo la differenza nel dettaglio tra l'immagine con ricampionamento e
#quella originaria

#ricampionamento pù aggresivo 
p224r63_2011res100 <- aggregate(p224r63_2011, fact=100)

g1 <- ggRGB(p224r63_2011, 4, 3, 2)
g2 <- ggRGB(p224r63_2011res, 4, 3, 2)
g3 <- ggRGB(p224r63_2011res100, 4, 3, 2)
g1+g2+g3
