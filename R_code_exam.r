# AUTHOR: Giada Franci

# This code processes images taken from sentinel_2A and sentinel_3B e 3A of the In.Cal.System (sources of data: EarthExplorer, Copernicus).
# The images from sentinel 2A include a little aria (1.385.673 ha) in Yarra Ranges National Park while The images from sentinel 3B e 3A include
# an LST (Land Surface Temperature) variable of a large aria in in South Australia .
# 
# Data include all the 10 mts resolution bands avaiable: RGB.

# NAMES OF THE USED BANDS

# SUMMARY
# 1 # LAND COVER ANALYSIS
## 2 # LAND SURFACE TEMPERATURE ANALYSIS

# 1 # ANALISI DI LAND COVER
# # IMPORT AND DATA PREPARATION

library(raster)   
library(viridis)
library(ggplot2)
library(RStoolbox)
library(patchwork)

setwd("D:/fire_data")

#blu banda 1
#verde banda 2
#rosso banda 3

lg2019 <- brick("2019.jpg")
lg2019

lg2020 <- brick("2020.jpg")
lg2020

lg2022 <- brick("2022.jpg")
lg2022

# collect images in the same window

pg19 <- ggRGB(lg2019, r=1, g=2, b=3, stretch="lin")
pg20 <- ggRGB(lg2020, r=1, g=2, b=3, stretch="lin")
pg22 <- ggRGB(lg2022, r=1, g=2, b=3, stretch="lin")

pg19+pg20+pg22

# ## LAND COVER

#classifico le immagini con la funzione unsuperClass

dev.off()

lg2019c <- unsuperClass(lg2019, nClasses=4)
plot(lg2019c$map)
lg2019c
# class 1: terra nuda
# class 2: bosco
# class 3: nuvole
# class 4: aree coltivate

lg2020c <- unsuperClass(lg2020, nClasses=4)
plot(lg2020c$map)
lg2020c
# class 1: terra nuda
# class 2: terra coltivata
# class 3: area bruciata
# class 4: bosco

lg2022c <- unsuperClass(lg2022, nClasses=4)
plot(lg2022c$map)
lg2022c
# class 1: terra coltivata + ripresa da incendio
# class 2: terra nuda
# class 3: nuvole
# class 4: bosco

#visualizzare questi dati ci è servito per ottenre delle mappe dove ho dei pixel
#di aree verdi, così posso cominciare a calcolare l'area occupata e la proporzione di pixel 
#di aree verdi nell'anno 20019 e rispetto al 2020 e al 2022. Questio conto lo faccio
#calcolando la frequenza (misura di quante volte avviene un certo evento).
#Per il calcolo e uso la funzione freq.

# ### FREQUENCY CALCULATION 

# total pixels 92001420

#suddivisi così:

freq(lg2019c$map)
#value  count:
#classe 1 10563469 pixel
#classe 2 60355174 pixel
#classe 3   818023 pixel 
#classe 4 20264754 pixel 

freq(lg2020c$map)
#value  count:
#classe 1  3397852 pixel
#classe 2 15061480 pixel
#classe 3 29784744 pixel
#classe 4 43757344 pixel 

freq(lg2022c$map)
#value  count:
#classe 1 18657949 pixel
#classe 2  5995202 pixel
#classe 3   288324 pixel
#classe 4 67059945 pixel 

#calcolo proprorzione e percentuale del bosco nel 2019, nel 2020 e nel 2022
tot19 <- 92001420
prop_green_19 <- 60355174 / tot19
percent_green_19 <- 60355174 * 100 / tot19
#percent 65,602437

tot20 <- 92001420
prop_green_20 <- 43757344 / tot20
percent_green_20 <- 43757344 * 100 / tot20
#percent 47,561596

tot22 <- 92001420
prop_green_22 <- 67059945 / tot22
percent_green_22 <- 67059945 * 100 / tot22
#percent 72,890119

#calcolo area bruciata nel 2020

tot20 <- 92001420
prop_fire_20 <- 29784744 / tot20
percent_fire_20 <- 29784744 * 100 / tot20
#percent 32,374222

#calcolo totale alberi persi in tutta l'austalia, considerando dati del WWF che riportano circa 
#19 milioni di ettari impattai dalle fiamme

green_lost <- 60355174 - 43757344 
# = 16597830

percent_green_lost <- ( green_lost / 60355174) * 100
#percent 27,500260

# ### DATAFRAME

class <- c("2019", "2020")
green_perc_19_20 <- c(65.602437, 47.561596)
green_perc_19_22 <- c(65.602437, 72.890119)

#ora creiamo la tabella con la funzione data.frame

multitemporal <- data.frame(class, green_perc_19_20, green_perc_19_22)
multitemporal

#per vederla in formato tabella scrivo View(multitemporal)
View(multitemporal)

# #### PLOT

# plot del  2019 e 2020
ggplot(multitemporal, aes(x=class, y=green_perc_19_20, color=class)) +
  geom_bar(stat="identity", fill="orange")


#plot del 2019 e 2022
ggplot(multitemporal, aes(x=class, y=green_perc_19_22, color=class)) +
  geom_bar(stat="identity", fill="green")


## 2 # LAND SURFACE TEMPERATURE ANALYSIS

# usiamo la variabile LST (Land Surface Temperature) di copernicus per vedere
# quanto è cambiata la temperatura in Australia nella nostra zona di nalisi
# prima dopo e durante l'incendio.

# funzione brick crea un oggetto Raster Brick, da un immagine satellitare con
# tante bande tutte insieme.
# Noi però adesso nonabbiamo un immagine satellitare già pronta con tutte l bande
# insieme ma abbiamo 4 dati diversi (lst2019, lst2020, lst2022).
# per prima cosa li importiamo uno per uno (dopo fa vedere come si iportano tutti insieme)
# lo facciamo mediante la funzione raster

lst2019 <- raster("lst2019.tif")
lst2020 <- raster("lst2020.tif")
lst2022 <- raster("lst2022.tif")

# creaimo una color palet

cl <- colorRampPalette(c("blue", "light blue", "pink", "red")) (100)

# ## importare questo set di immagini tutte insieme.

# usiamo la funzione lapplay che applica una funzione su una lista o un vettore,
# prende una lista di files e applica a tutti la stessa funzione.

# 1) qcreare la lista di files con la funzione list.files

# all'interno di questa funzione useremmo l'argomento pattern che serve a spiegare
# quale è una caratteristica comune di tutti i files che vogliamo importare 

rlist <- list.files(pattern = "lst")
rlist

# 2) applichiamo la funzione raster alla lista di files attraverso la funzione lapplay

import <- lapply(rlist, raster)
import

# 3) adesso potremmo fare lo stack che prende i vari layers e li mette tutti insieme
#    in un singolo file, esattamente come un immagine satellitare.

tgr <- stack(import)
tgr

# tgr è un rasterstack stessa cosa del rasterbrick, ma il rasterbrick è
# quando con la funzione brick importavamo l'intera immagine satellitare, il
# rasterstack invece lo abbiamo creato noi con la funzione stack.

#a questo punto invece di fare i multiframes (quelli fatti fino ad ora dove
#ripetevamo la funzione per ogni elemento), io faccio una lista di files, applico
#funzione lapplay che applica la funzione raster alla lista e poi faccio uno stack.

plot(tgr, col=cl)


