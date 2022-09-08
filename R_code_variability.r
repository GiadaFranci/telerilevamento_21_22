#29/04/22

library(raster)
library(RStoolbox) #contiene funzione per classificazione
library(ggplot2)#per ggplot
library(patchwork)
library(viridis)#serve ad avere a disposizione altri pacchetti di colori, questo è utile
#per coloro che ha difficolta a vedere i colori, contine diverse legende.

setwd("c:/lab/")

#misure di variabilita di un sistema nello spazio
#immaine del Similaun 
#guardiamo variazioni geostrutturali della roccia
#guadiamo variazioni ecologiche dalla parte delle praterie sommitali fino ai boschi 

#importiao l'immagine multimediale del Similaun 
sen <- brick("sentinel_similaun.png")

#plottiamo l'immagine con ggRGB
ggRGB(sen, 1, 2, 3) #stretch qui non serve senno non funziona il comando

#metto l'infraroso nel green
ggRGB(sen, 2, 1, 3)

#con il pacchetto patchwork unisco le due immagini sopra
g1 <- ggRGB(sen, 1, 2, 3)
g2 <- ggRGB(sen, 2, 1, 3)
g1+g2

#uso la funzione focal per calcolare la variabilita statistica nelle nostre immagini
#facciamo un primo calcolo della variabilita usando l'infrarosso, seleziono prima
#il primo starto su cui fare il calcolo
nir <- sen[[1]]
plot(nir) #guardo imagine. c'è tanta variabilita
sd3 <- focal(nir, matrix(1/9, 3, 3), fun=sd) #la matrice è formata da 3x3 pixel, definisco l'unita che è il pixel
#quinid è 1/9, poi definisco il n di colonne e delle righe che sono 3 e 3. poi definisco la funzione
#adesso faccio un plot con una color palet
clsd <- colorRampPalette(c("blue","green","pink","magenta","orange","brown","red","yellow"))(100)
plot(sd3, col=clsd)

#ora plotto con ggplot
ggplot()+
  geom_raster(sd3, mapping = aes(x=x, y=y, fill=layer)) 

#proviamo a plottare con la legenda di viridis con la funzione scale_fill_viridis
ggplot()+
  geom_raster(sd3, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis()+
  ggtitle("standard deviation of PC1 by viridis coulor scale") #le zone a alta variablita sono di colore 
#giallo chiaro, i crepazzi, le zone di transizione tra foresta e prateria.

#posso cambiare anche la legenda
ggplot()+
  geom_raster(sd3, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis(option = "cividis")+
  ggtitle("standard deviation of PC1 by viridis coulor scale")

#cambio la legenda con magma
ggplot()+
  geom_raster(sd3, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis(option = "magma")+
  ggtitle("standard deviation of PC1 by viridis coulor scale")

#noi abbiamo usato una finestra 3x3 ma la posso anche cambiare per esmpio ne metto una 7x7
sd7 <- focal(nir, matrix(1/49, 7, 7), fun=sd) #unità è un pixel ma la matrice questa volta 
#è 7x7 quinid un pixel è 1/49 e la funzione che usiamo è sempre la standard variatio (sd)
#così mi da questo errore : 
#Error in .focal_fun(values(x), w, as.integer(dim(out)), runfun, NAonly) : 
#Evaluation error: could not find function "fun".
#l'errore è stato dare a un oggetto il nome della funzione cioè sd nome oggetto e sd nome
#funzione infatti quando è arrivato a fare la funzione non è andato a ricercarela funzione sd
#ma l'oggetto creato precedentamente
#prima di rilanciare tutto con un nuovo nome di sd devo cancellare il precedente sd 
#creato
