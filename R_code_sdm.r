#CARTOGRAMMA
#metodo per mettere in risalto gli errori di un calcolo nei modelli di distribuzione
#delle specie

#usiamo il sito OVERLEAF sito con un monte di progetti e articoli tuti modificabili in latek
#fa passare il linguaggio di programmazione a un linguaggio di testo

library(raster)

#MODELLIZZAZIONE NELLA DISTRIBUZIONE DELLE SPECIE (anche di qualsiasi altra variabile)
#si prendono delle varaibili ambientali e si guarda in funzione di quelle varianbili
#ambinetali quale è la probabilita di torvare una certa specie
#il pacchetto sdm fa la species distribution mode.

install.packages("sdm")
library(sdm)
library(rgdal)

#funzione system.file caeica un file dentro R specificando da quel pacchetto viene preso

file <- system.file("external/species.shp", package="sdm") 
species <- shapefile(file)
species #occurrence presenza o assenza di una certa specie, 200 sono i punti a terra
#fove è stata misurata la specie

plot(species, pch=19) #pch è il simbolo dei punti il numero gli cambia la 

species$Occurrence #punti nello spazio dove è stata registrata la presenza(1) o l'assenza(0)

#facciamo il plot di species e dentro facciamo un subset con le parentesi quadre, 
#ci interessa di mappare solo le presenze. la virgola dopo l'1 va sempre messa senno
#non si chiude il comando

plot(species[species$Occurrence == 1,], col="blue", pch=19) #presenze

#voglio aggiungere dei puniti al plot precedente

points(species[species$Occurrence == 0,], col="red", pch=19) #assenze

#ci prendiamo il path sarebbe il percorso del file, dove sono stoccati i 4 file che si vogliono usare
path <- system.file("external", package="sdm") 

#facciamo una lista di file
list <- list.files(path = path, pattern = 'asc', full.names = T) #pattern è il formati del file che stiamo caricando
# full.name è necessario (finire di scrivere)

stack(list) #stack di 4 file (importati 4 file), le 4 variabili sono nei nomi

cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
preds <- stack(list)
#plot dei predittori (ci aiutano a prevedere dove sono le specie)
plot(preds, col=cl)

elev <- preds$elevation
prec <- preds$precipitation
temp <- preds$temperature
vege <- preds$vegetation

#plottto la distribuzione delle specie per ciascuno dei predittori visti sopra
plot(elev, col=cl)
points(species[species$Occurrence == 1,], pch=19) #specie a cui non piace stare a quote elevate

plot(temp, col=cl)
points(species[species$Occurrence == 1,], pch=19) #non ama il freddo

plot(prec, col=cl)
points(species[species$Occurrence == 1,], pch=19) #gli piacciono le precipitazioni

plot(vege, col=cl)
points(species[species$Occurrence == 1,], pch=19) #ama essere coperta da veetazione

#adesso punto centrale nell'immagine della presentazione 

#andiamo a fare il modello

#nel pacchetto sdm abbiamo la funzione che dichiara i dati che sono tutti quelli sulla
#sinistra della nostra immagine della nostra presentazione (specie e predittori)
#a noi interessano i train dati a terra (specie) e i predictor (predittori)

datasdm <- sdmData(train = species, predictors = preds)
datasdm

#la funzione sdm ha fit prende i dati iniziali fa il modello logistico (asse y 0 e 1 e asse x temp)
#che approssima i dati li dove non ci sono basandosi su quelli esistenti è una 
#funzione lineare. la usiamo per ogni predictor. con tante variabili modello lineare
#generalizzato

m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation,
          data=datasdm,
          methods="glm")

#adesso faccio le previsoni, parte finale, con la funzione predict del pacchetto sdm
#faccio l'output del raster lyer, previsione sulla maggiore probabilita della presenza
#della nostra specie

p1 <- predict(m1, newdata=preds)

#output
plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=19)

#ottengo mappa di previsione della distribuzione della psecie, si vede che dove ci
#sono i punti neri la previsione è + alta tranne che in alcuni casi perche probabilmente
#in quella parte i predittori vanno contro la distribuzione della specie

#riascoltare la registrazione dopo questa parte 

#modello rispetto a tutti i nostri parametri
par(mfrow=c(2,3))
plot(p1, col=cl)
plot(elev, col=cl)
plot(prec, col=cl)
plot(temp, col=cl)
plot(vege, col=cl)

#alternativa a quello sopra (così è meglio)
final <- stack(preds, p1)
plot(final, col=cl)

#si potrebbe fare anche con ggplot ma è un pò troppo complesso

