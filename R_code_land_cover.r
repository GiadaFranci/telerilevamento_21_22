#22/04/22

#analisi multitemporale di un cambiamento nell'uso del suolo
#lavoriamo con le mappe di land cover (copertura del suolo da parte della vegetazione
#o strutture artificiali)

#generazione di un codice per generare mappe della copertura del suolo dal staellite

library(raster)
library(RStoolbox) #contiene funzione per classificazione

setwd("c:/lab/")

#importiamo le immagini multispettrali defor1 e defor2
l92 <- brick("defor1_.jpg") #così importo l'intero pacchetto di dati dell'immagine da importare
plotRGB(l92, 1, 2, 3, stretch="lin") #NIR1, R2 e G3

l06<- brick("defor2_.jpg")
plotRGB(l06, 1, 2, 3, stretch="lin")

par(mfrow=c(2,1)) #inseriamo le immagini nella stessa finestra
plotRGB(l92, r=1, g=2, b=3, stretch="lin")
plotRGB(l06, r=1, g=2, b=3, stretch="lin")

#adesso classifichiamo i dati

# install.packages("ggplot2")
library(ggplot2) #oggi lo usiamo per la prima volta per fare dei grafici statistici 
#sulle frequenze delle calssi ma lo useremo anche più avanti come restituzione grafica
#delle immagini da satellitemo drone.
#funzione ggRGB è basta su due pacchetti, il ggplot2 e RStoolbox.
#pacchetto per la visualizzazione dei dati 
#questo il pacchetto RStoolbox contiene la funzione ggRGB, è un plot RGB ma usa ggplot2
#al livello di codice sostituiamo semplicemente plotRGB con ggRGB
p1 <- ggRGB(l92, r=1, g=2, b=3, stretch="lin")
p2 <- ggRGB(l06, r=1, g=2, b=3, stretch="lin")

#adesso queste due immagini le devo unire e lo faccio con il pacchetto patchwork
# per usare questa funzione devo associare i due plot sopra ggRGB a un oggetto
#installo pacchetto patchwork
install.packages("patchwork")
library(patchwork)

p1+p2
p1/p2 #cosi metto le immagini una sopra l'altra

#continuo con la calssificazione e uso la funzione unsuperClass
#NB. asscoio l'acqua al suolo nudo 
l92c <- unsuperClass(l92, nClasses=2)
l92c
dev.off()#prima del plot così ho una sola mappa
plot(l92c$map) #devo sempre specificare map altrimenti non mi restituisce nulla
# class 2: forest
# class 1: agriculture area + acqua

#ora classifico anche l'immagine l06
l06c <- unsuperClass(l06, nClasses=2)
plot(l06c$map)
# class 1: agriculture + acqua
# class 2: forest

#visualizzare questi dati ci è servito per ottenre delle mappe dove ho dei pixel
#di foresta e posso cominciare a calcolarmi l'area occupata o la proporzione di pixel 
#di foresta in un anno rispetto ad un'altro anno. questio conto lo faccio
#calcolando la frequenza (misura di quante volte avviene un certo evento)
# es. frequenza dei pixel appartententi alla classe foresta

#per il calcolo delle frequenze uso la funzione freq, funzione di base di R e genera 
#delle tabelle di frequenza (dice qunti pixel ci sono)
freq(l92c$map)
#   value count, i pixel totali nell'aimmagine del 92 sono 341292 e sono suddivisi
#così:
#classe 1  33983 pixel
#classe 2 307309 pixel

freq(l06c$map)
#value  count:
#classe 1 164267
#classe 2 178459

#ora creo un dataset con tutte le frequenze e poi fo un plot finale con ggplot2
