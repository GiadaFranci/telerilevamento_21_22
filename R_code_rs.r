#4.03.22
# primo script che useremmo a lezione

library(raster)

#lezione 10/03/22

install.packages("raster")
library(raster)

#settaggio cartella di lavoro, con dati raster farlo tutte le volte e ricaricare i dati da 0
#setwd (set working directory) decido la cartella di lavoro
#sapere il percorso della cartella
setwd("c:/lab/")

#carichiamo i dati forniti dal prof
#importo tabella readtable
#file raster fatto da tante bande ogniuna con una riflettanza e un sensorino per ogni banda
#(lunghezza d'onda) ongi senosre regista il paesaggi in un alunghezza d'onda.
#posso scegliere io qunate bande usare in base a quello che voglio fare
#blu banda 1
#verde banda 2
#rosso banda 3
#infrarosso banda 4
#termico banda 6
#per importare un pacchetto di dati uso la funzione brick
#cera un oggetto chiamato l2011 con calsse rasterbrick

l2011 <- brick("p224r63_2011.grd") #nella parentesi il nome della mia immagine
l2011

#classe mi dice il tipo di oggetto che abbiamo: rasterbrick
#1499 numero di righe 
#2967 colonne
#4447533 pixel per ogni banda 

#7 bande 
#n righe per il n di colonne mi da il n di pioxel ovvero la risoluzione
#resolution è la dimensione dei pixel 30x30m
#source sorgente del dato
#name sono i nomi delle bande sre (spectral reflectans) vale per tutte le bande tranne per quella del termico
#valori minimi è sempre 0 (tranne nella banda del termico) e poi valori massimi
#i valori vanno da 0 a 1: perchè il flusso radinate (quante anergia arriva ) riflettanza divisione tra flusso radibante riflesso e quanto entra
#se non riflettiamo nulla la riflettanza è uguale a 0
#se rifletto tutto ho 1
#quindi valore minimo 0 e valore massimo 1
#non sempre i valori vanno da 0 a 1(ne parliamo più avanti)

#guardiamo il primo plot della mia immagine
#plot fa un plot generico x y
#nel nostro caso x y sono gia nella stessa immahine quindi basta dire il nome della nostra immagine

plot(l2011)

#cambio legenda
#oggi noi usiamo valori bassi si mettono nero, e valori alti bianchi
#funzione colorRampPalette decido io i colri e la funzione adatta il colore
#dell'immagine a i colori che io scelgo
#la funzione ha due maiuscole, usarle o non funziona
#prendo i clori dai coliri di r che cerco su internet
#devo mettere una c davanti perche in r una serie di dati si mette con la c davanti
#metto il nuemro di passaggi, cioè il numero di colori possibili
#gli do un nome, creo un oggetto

cl <- colorRampPalette(c("black", "grey", "light gray")) (100)

#rifo il plot usando per ongi banda i nuovi colori
plot(l2011, col=cl)

#si nota che nella banda 4 c'è una riflettanza sparatissima perche è quella 
#dell'infrarosso e dove c'è vegetazione questa è alta
#lezione 18/03/22

#plottare una singola banda, quella del BLU 
#prima di tuto devo individuarla B1_sre (banda blu)
l2011
plot(l2011$B1_sre)  #vantaggiooso perche capisco cosa sto plottando

#2° metodo
plot(l2011[[1]]) #seleziono elemento n°1 che è proprio la banda ddel blu

#camibio la legenda anche a questo grafico
cl <- colorRampPalette(c("black", "grey", "light grey")) (100)  #100 sono le possibili tonalità tra i tre colori impostati
plot(l2011$B1_sre, col=cl)

#plotto che va da blu scuro a chiaro passando da azzurro
clblue <- colorRampPalette(c("dark blue", "dark cyan", "light blue")) (100)  #100 sono le possibili tonalità tra i tre colori impostati
plot(l2011$B1_sre, col=clblue)

#esporto l'immagine in blu come un pdf, salvo in cartella lab
pdf("banda1.pdf") #salvato nella working directory (come nei progetti in R, ma attenzione perchè i dati raster non sonosalvati bene se uso working directory di r )
plot(l2011$B1_sre, col=clblue)
dev.off()

#esporto l'immagine in blu come un png, salvo in cartella lab
png("banda1.png") #salvato nella working directory (come nei progetti in R, ma attenzione perchè i dati raster non sonosalvati bene se uso working directory di r )
plot(l2011$B1_sre, col=clblue)
dev.off()

#plotto che va da verde scuro a chiaro
clgreen <- colorRampPalette(c("dark green", "green", "light green")) (100)  #100 sono le possibili tonalità tra i tre colori impostati
plot(l2011$B2_sre, col=clgreen)


#immagine con solo alcune bande (mf=multiframe)
par(mfrow=c(1,2)) #1 righe 2 colonne, nrow vuol dire che parto dalle righe 
plot(l2011$B1_sre, col=clblue)
plot(l2011$B2_sre, col=clgreen)
dev.off()  #serve per chiudere la finestra che ho aperto con par

pdf("multiframe.pdf")
par(mfrow=c(1,2)) #1 righe 2 colonne, nrow vuol dire che parto dalle righe 
plot(l2011$B1_sre, col=clblue)
plot(l2011$B2_sre, col=clgreen)
dev.off()

#voglio plot invertito con blu sopra e verde sotto
#multiframe gli dico di rimepire da colonna 2 righe 1 colonna

pdf("multiframe2.pdf")
par(mfrow=c(2,1)) #1 righe 2 colonne, nrow vuol dire che parto dalle righe 
plot(l2011$B1_sre, col=clblue)
plot(l2011$B2_sre, col=clgreen)
dev.off()

clred <- colorRampPalette(c("brown3", "brown2", "brown1")) (100)  #100 sono le possibili tonalità tra i tre colori impostati
plot(l2011$B3_sre, col=clred)

clif <- colorRampPalette(c("red", "orange", "yellow")) (100)  #100 sono le possibili tonalità tra i tre colori impostati
plot(l2011$B4_sre, col=clif)

pdf("multiframe3.pdf")
par(mfrow=c(2,2)) 
plot(l2011$B1_sre, col=clblue)
plot(l2011$B2_sre, col=clgreen)
plot(l2011$B3_sre, col=clred)
plot(l2011$B4_sre, col=clif)
dev.off()

dev.off()
pdf("multiframe4.pdf")
par(mfrow=c(2,2))
plot(l2011$B1_sre, col=clblue)
plot(l2011$B2_sre, col=clgreen)
plot(l2011$B3_sre, col=clred)
plot(l2011$B4_sre, col=clif)
dev.off()

