#07/04/22

#calssificazione delle immagini con il caso del gran canyon

library(raster)
library(RStoolbox)#libreria per analisi e gestione dati satellitari

setwd("c:/lab/")

#foto fatte dalla space station, non abbiamo immagini satellitari come negli
#esercizzi fatti fino ad ora (imp.non dire mai foto da satellite)

#come passare da dati continui a delle classi es.composizione mineralogica

#importo i dati
# Download Solar Orbiter data and proceed further!

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
#solar orbiter è il satellite che prende dati sui movimenti del sole
so

plotRGB(so, 1,2,3, stretch="lin")

soc <- unsuperClass(so, nClasses=3)
plot(soc$map)

soc20 <- unsuperClass(so, nClasses=20)
plot(soc20$map,col=cl)

cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(soc20$map,col=cl)

# scarico immagine del Grand Canyon
# https://landsat.visibleearth.nasa.gov/view.php?id=80948

# When John Wesley Powell led an expedition down the Colorado River and through
#the Grand Canyon in 1869, he was confronted with a daunting landscape. 
#At its highest point, the serpentine gorge plunged 1,829 meters (6,000 feet)
#from rim to river bottom, making it one of the deepest canyons in the United States.
#In just 6 million years, water had carved through rock layers that collectively 
#represented more than 2 billion years of geological history, nearly half of the time Earth has existed.

#importo immagine scaricata su R
gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")
gc #tre bande (tre laier del visibile) le ricompongo come un plot RGB

plotRGB(gc, r=1, g=2, b=3, stretch="lin")

#cmabio lo stretch come un istogramma
plotRGB(gc, r=1, g=2, b=3, stretch="hist") #aumentiamo la visualizzazione delle code
#di una "retta in un grafico

#unsuperClass(unsupervised) funzione per creare le classi
#sceglie lui le classi non le mettiamo tutte noi
gcclass2 <- unsuperClass(gc, nClasses=2)
gcclass2
#restituisce un blocco solo nel caso della classificazione che è la mappa
#faccio il plot del modello con dentro la mappa 
plot(gcclass2$map)
#noi abbiamo creato solo la classe 1 e 2 tutti i valori intermedin non hanno senso
#in questo momento
#classe 1 è la rocia e la 2 è acqua e ombre7composizione mineraligica diversa
#set.seed(17), il numero è a nostra discrezione, serve a mantenre la stessa calssificazione ogni volta che rilanciamo
#il compando di classificazione sulla nostra foto

#es. classificazione la mappa su 4 classi
gcclass4 <- unsuperClass(gc, nClasses=4)
plot(gcclass4$map)

#per definire meglio le classi posso cambiare i colori
clc <- colorRampPalette(c('yellow','red','blue','black'))(100)
plot(gcclass4$map, col=clc)
#nella classe blu abbiamo il caso in cui le nuvole sono unite alla parte di abbie
#probabilmente perche hanno una simile riflettanza.
#e associa l'acqua gialla a delle effettive ombre, quinid probabilmente la parte nera 
#è un'altro tipo di composizione mineralogica, come il rosso.
#per capire poi nella mappa perche abbiamo una variazione di riflettanza devo andare
# a fare un controllo di persona, per esempio per capire la differenza tra parte rossa
#e nera.

#comparo la mappa classificate con la foto originale
par(mfrow=c(2, 1))
plot(gcclass4$map, col=clc)
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

#vedimao cosa succede se uso la lineare al posto degli istogrammi, si capisce semplicemnete
#meno dalla foto.
