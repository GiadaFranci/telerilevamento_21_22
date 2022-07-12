#20/05/22

library(raster)
library(RStoolbox) #contiene funzione per classificazione e per analisi multivariata
library(ggplot2)#per ggplot
library(patchwork)
library(viridis)


setwd("c:/lab/")

#visualizzare e analizzare dati con LIDAR

#Scarico i file dalla cartella dati.zip che apro nella cartella lab

dsm_2013 <- raster("2013Elevation_DigitalElevationModel-0.5m.tif")

dsm_2013

dtm_2013 <- raster("2013Elevation_DigitalTerrainModel-0.5m.tif")

plot(dsm_2013)

chm_2013 <- dsm_2013 - dtm_2013

ggplot() + 
  geom_raster(chm_2013, mapping =aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis() +
  ggtitle("CHM 2013 San Genesio/Jenesien")

#il blu sono i prati, le varie case (tra 10 e 20) e il bosco a torno con i colri 
#più chiari (tra 20 e 40)

dsm_2004 <- raster("2004Elevation_DigitalElevationModel-2.5m.tif") #risoluzione un pò 
#più bassa

dtm_2004 <- raster("2004Elevation_DigitalTerrainModel-2.5m.tif")

chm_2004 <- dsm_2004 - dtm_2004

ggplot() + 
  geom_raster(chm_2013, mapping =aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis() +
  ggtitle("CHM 2004 San Genesio/Jenesien")

#confronto i due chm del 2004 e 2013

difference <- chm_2013 - chm_2004 #errore peche hanno due risoluzioni diverse le immagini
#per cui dobbiamo cambiare la risoluzione di uno dei due dati. si passa da quella più
#appurata a una meno

chm_2013_r <- resample(chm_2013, chm_2004) #x è quello che vogliamo ricampiponare (2013) e y è la risoluzione a cui trasformarlo
#con qeusto comando ricampiono un immagine sulla base di un'altra.
#avevavmo usato un comando simile che era aggregate pero non fa proprio la stessa cosa, su questo gli dicevamo
#noi di quanto trasformare la risoluzione

#ora rifacciamo il confronto
difference <- chm_2013_r - chm_2004 

ggplot() +
  
  geom_raster(difference, mapping =aes(x=x, y=y, fill=layer)) +
  
  scale_fill_viridis() +
  
  ggtitle("CHM difference San Genesio/Jenesien")
#vediamo così quello che abbiamo trovato, le aree più scure sono aree disboscate
#le aree più chiarie, gialli, c'è un aumento delle piante

#ricreiamo tutto quello che abbiamo visto in 3D 
install.packages("lidR")
library(lidR)

point_cloud <- readLAS("point_cloud.laz")

plot(point_cloud)
