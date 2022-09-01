# AUTHOR: Giada Franci

# This code processes images taken from sentinel_2A of the In.Cal.System (sources of data: EarthExplorer, Copernicus).
# The images include the areas of city's Lucca an Prato.
# Ho scelto queste due aree pervhe considerate Lucca come la città con meno verde urbano e Prato
# come la città con più verde urbano. Volevo quindi andare a vedere come variavano in estate le temperature 
# nelle due cittò in base alla presenza di verde urbano, facendo anche riferuimento a due anni differenti.
# Delle immagini risalgono a Giugno 2022 considerato uno die mesi più caldi di questo anno e altre fanno riderimento
# a Giugno 2018 con delle temperature in alcuni gionri anche di 5 gradi inferiori alle attuali.
# Voglio quindi andare prima a guardare la quantità di aree verdi nelle due città in questi due anni, prendendo più
# o meno la stessa superfice e poi vado a guardare sempre nei due anni come cambieno le temperature nelle 2 aree

# Data include all the 10 mts resolution bands avaiable: RGB and NIR.

# NAMES OF THE USED BANDS

# SUMMARY

# 1 # ANALISI DI LAND COVER
# 1 # IMPORT AND DATA PREPARATION

library(raster)   
library(viridis)
library(ggplot2)
library(RStoolbox)
library(patchwork)

setwd("c:/fire_data")

#blu banda 1
#verde banda 2
#rosso banda 3

## analisi verde urbano 2018

lg2019 <- brick("2019.jpg")
lg2019

lg2020 <- brick("2020.jpg")
lg2020

lg2022 <- brick("2022.jpg")
lg2022

#inserisco immagini nella stessa finestra

pg19 <- ggRGB(lg2019, r=1, g=2, b=3, stretch="lin")
pg20 <- ggRGB(lg2020, r=1, g=2, b=3, stretch="lin")
pg22 <- ggRGB(lg2022, r=1, g=2, b=3, stretch="lin")

pg19+pg20+pg22

# 2 # LAND COVER

#classifico le immagini con la funzione unsuperClass
dev.off()#prima del plot così ho una sola mappa
lg2019c <- unsuperClass(lg2019, nClasses=3)
lg2019c
#prima del plot così ho una sola mappa
plot(lg2019c$map) 
# class 1: bosco
# class 2: aree di terra nuda
# class 3:

lg2020c <- unsuperClass(lg2020, nClasses=3)
plot(lg2020c$map)
lg2020c
# class 1: terra nuda e aree bruciate
# class 2: bosco
# class 3: 

lg2022c <- unsuperClass(lg2022, nClasses=3)
plot(lg2022c$map)
lg2022c
# class 1: bosco
# class 2: terra nuda
# class 3: 

#visualizzare questi dati ci è servito per ottenre delle mappe dove ho dei pixel
#di aree verdi, così posso cominciare a calcolare l'area occupata e la proporzione di pixel 
#di aree verdi nell'anno 20018 e rispetto al 2022. Questio conto lo faccio
#calcolando la frequenza (misura di quante volte avviene un certo evento)

#per il calcolo delle frequenze uso la funzione freq, funzione di base di R e genera 
#delle tabelle di frequenza (dice qanti pixel ci sono)

#value count, i pixel totali nell'aimmagine sono 92001420 e sono suddivisi
#così:

freq(lg2019c$map)
#classe 1 75948983 pixel
#classe 2 16052437 pixel

freq(lg2020c$map)
#value  count:
#classe 1 22194971 pixel
#classe 2 69806449 pixel

freq(lg2022c$map)
#value  count:
#classe 1 76826541 pixel
#classe 2 15174879 pixel

#calcolo proprorzione e percentuale del bosco nel 2019 e nel 2020
tot19 <- 92001420
prop_green_19 <- 75948983 / tot19
percent_green_19 <- 75948983 * 100 / tot19
#percent 64.933819

tot20 <- 92001420
prop_green_20 <- 69806449 / ltot20
percent_green_20 <- 69806449 * 100 / ltot20
#percent 66.298606

#calcolo proprozione e percentuale del bosco nel 2019 e nel 2022

tot22 <- 92001420
prop_green_22 <- 140896 / tot22
percent_green_22 <- 140896 * 100 / tot22
#percent 69.209156

# 4 # DATAFRAME

class <- c("Lucca", "Siena")
green_perc_2018 <- c(64.933819, 68.806366)
green_perc_2022 <- c(66.298606, 69.209156)

#ora creiamo la tabella con la funzione data.frame

multitemporal <- data.frame(class, green_perc_2018, green_perc_2022)
multitemporal
#per vederla in formato tabella scrivo View(multitemporal)
View(multitemporal)

# 5 # PLOT

# plot del  2018
ggplot(multitemporal, aes(x=class, y=green_perc_2018, color=class)) +
  geom_bar(stat="identity", fill="white")


#plot del 2022
ggplot(multitemporal, aes(x=class, y=green_perc_2022, color=class)) +
  geom_bar(stat="identity", fill="white")


## 2 # ANALISI TEMPERATURE DELLA SUPERFICIE

#
