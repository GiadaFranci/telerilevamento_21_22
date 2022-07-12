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
#delle immagini da satellite o drone.
#funzione ggRGB è basta su due pacchetti, il ggplot2 e RStoolbox.
#pacchetto per la visualizzazione dei dati 
#il pacchetto RStoolbox contiene la funzione ggRGB, è un plot RGB ma usa ggplot2
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
#di foresta in un anno rispetto ad un'altro anno. questo conto lo faccio
#calcolando la frequenza (misura di quante volte avviene un certo evento)
# es. frequenza dei pixel appartententi alla classe foresta

#per il calcolo delle frequenze uso la funzione freq, funzione di base di R e genera 
#delle tabelle di frequenza (dice qunti pixel ci sono)
freq(l92c$map)
#value count, i pixel totali nell'immagine del 92 sono 341292 e sono suddivisi
#così:
#classe 1  33983 pixel
#classe 2 307309 pixel

freq(l06c$map)
#value  count:
#classe 1 164267
#classe 2 178459

#ora creo un dataset con tutte le frequenze e poi fo un plot finale con ggplot2

#calcolo proprorzione e percentuale della foresta nel 92 e nel 06
tot92 <- 341292
prop_forest_92 <- 307309 / tot92
percent_forest_92 <- 307309 * 100 / tot92
#percent 90.042837

tot06 <- 342726
prop_forest_06 <- 164267 / tot06
percent_forest_06 <- 164267 * 100 / tot06
#percent 47.929541

#calcolo proprozione e percentuale dell'agricoltura nel 92 e nel 06
tot92 <- 341292
prop_agr_92 <- 33983 / tot92
percent_agr_92 <- 33983 * 100 / tot92
#percent 9.957162

tot06 <- 342726
prop_agr_06 <- 178459 / tot06
percent_agr_06 <- 178459 * 100/ tot06
#percent 52.070458

#DATI FINALI
#92:
#perce for: 90.042837
#perce agr: 9.957162
#06:
#perce for: 47.929541
#perce agr: 52.070458

#creiamo un dataframe (tabella) con 3 colonne, la prima è la classe, la seconda 
#con i valori % del 92 e la terza colonna sono i valori % del 2006
#nelle righe (classi) abbiamo foresta e agricoltura
#columns o fields (campi)
class <- c("Forest", "Agriculture")
percent_1992 <- c(90.042837, 9.957162)
percent_2006 <- c(47.929541, 52.070458)

#ora cerao la tabella (dataframe) con una funzione data.frame
multitemporal <- data.frame(class, percent_1992, percent_2006)
multitemporal
#per vederla in formato tabella scrivo View(multitemporal)
View(multitemporal)

#il pacchetto ggplot2 ha una funzione particolare che è ggplot dove crea un ggplot nuovo dove dico i dati da usare
#che tipo di colonne uso ecc.. attraverso l'argomento aes (aestetics) in questo caso
#sono le colonne

#faccio un primo plot del 1992

ggplot(multitemporal, aes(x=class, y=percent_1992, color=class)) +#cosi apre un plot vuoto
geom_bar(stat="identity", fill="white")
#ora definisco che plot voglio, nel mio caso a barre con la funzione geom_bar dove inserisco la 
#statistica
#fo lo stesso con i dati del 2006
ggplot(multitemporal, aes(x=class, y=percent_2006, color=class)) +#cosi apre un plot vuoto
  geom_bar(stat="identity", fill="white")

#creo un pdf del 1992 e ddel 2006
pdf("percentages_1992.pdf")
ggplot(multitemporal, aes(x=class, y=percent_1992, color=class)) +
  geom_bar(stat="identity", fill="white")
dev.off()

pdf("percentages_2006.pdf")
ggplot(multitemporal, aes(x=class, y=percent_2006, color=class)) +
  geom_bar(stat="identity", fill="white")
dev.off()
