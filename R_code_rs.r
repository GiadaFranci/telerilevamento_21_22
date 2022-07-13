
install.packages("raster")
library(raster)

#settaggio cartella di lavoro, con dati raster farlo tutte le volte e ricaricare i dati da 0
#setwd (set working directory) decido la cartella di lavoro
#sapere il percorso della cartella
setwd("c:/lab/")

#carichiamo i dati forniti dal prof
#importo tabella readtable
#file raster fatto da tante bande ogniuna con una riflettanza e un sensorino per ogni banda
#(lunghezza d'onda) ongi senosre regista il paesaggi in una lunghezza d'onda.
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
#i valori vanno da 0 a 1: perchè la riflettanza è la divisione tra flusso radiante riflesso e quanto entra
#se non riflettiamo nulla la riflettanza è uguale a 0
#se rifletto tutto ho 1
#quindi valore minimo 0 e valore massimo 1
#non sempre i valori vanno da 0 a 1(ne parliamo più avanti)

#guardiamo il primo plot della mia immagine
#plot fa un plot generico x y
#nel nostro caso x y sono gia nella stessa immagine quindi basta dire il nome della nostra immagine

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

#rilancio il plot usando per ongi banda i nuovi colori
plot(l2011, col=cl)

#si nota che nella banda 4 c'è una riflettanza altissima perche è quella 
#dell'infrarosso e dove c'è vegetazione questa è alta

#plottare una singola banda, quella del BLU 
#prima di tuto devo individuarla B1_sre (banda blu)
l2011
plot(l2011$B1_sre)  #vantaggiooso perche capisco cosa sto plottando

#2° metodo
plot(l2011[[1]]) #seleziono elemento n°1 che è proprio la banda del blu

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

#creo i plot come ella lezioe precedente oppure
#poso scrivere plot(l2011[[4]]) il 4 naturalmente è riferito alla banda 4

#posso usare solo tre bande perche il pc lavora con i colori RGB
#monto le bande corrispondenti al loro colore dell' RGB
#lo faccio con il plotRGB
#stretch fa uno stretch dei valori per far si che si vedano meglio i contrasti tra i colori
#può essere lineare "lin" o a istogramma "ist"
#plotto il layers dell'RGB

plotRGB(l2011, r=3, g=2, b=1, stretch="lin") #l'immagine che restutisce è esattamente come la vedremmo da 200km

#per usare la banda 4 mi slittano tutte
#la pineta riflette monlto nell'infrarosso perchè le cellule a palizzata fanno rimbalzare l'infrarosso vicino
#quindi le pinate riflettono l'infrarosso (anche il verde si riflette, lo vediamo  noi), mentre rosso e blu vengono assorbite

plotRGB(l2011, r=4, g=3, b=2, stretch="lin")

#avendo montato la banda dell'infrarosso sul red tutto quello che è rosso è vegetazione

#sposto l'infrarosso nella componenete green

plotRGB(l2011, r=3, g=4, b=1, stretch="lin")

#così tutto quello che riflette nell'infrarosso vicino diventa verde
#nell'immagine nella foresta possiamo vedere bene la vegetazione e la sua struttura interna 
#in violetto abbiamo le zone con suolo nudo, per visualizzare meglio questi terreni passiamo l'infraroso nel blu

plotRGB(l2011, r=3, g=2, b=4, stretch="lin")

#adesso la vegetazione è blu e il giallo è suolo nudo
#lo stretch, se in una zona non ho tutte le riflettanze (magari ho solo 20 e 25) io posso espandere la scala di valori
#fino a raggiungere una sorta di nuova banda che varia da 0 a 100.
#quinid lo stretch lineare amplia i miei valori possibili
#lo stretch a istogrammi fa la stessa cosa del lineare ma provoca un aumento nella gamma dei colori forte

plotRGB(l2011, r=3, g=4, b=1, stretch="hist")

#così vedo di più la differenza di parti nella foresta, il viola rappresenta il suolo nudo

#per scegliere la composizione delle bande le faccio un pò tutte e poi vedo quale 
#rappresenta meglio la mia immagine

#es. creare un multiframe (insieme di più immagine) con sopra un immagine con  la visulaizzazione a colori natrurali RGB
#(stretch lineare) e sotto un immagine a infrarosso (stretch istogrammi)

par(mfrow=c(2,1))
plotRGB(l2011, r=3, g=2, b=1, stretch="lin")
plotRGB(l2011, r=3, g=4, b=2, stretch="hist")

#adesso immagine 1988 più vecchia dalla cartella lab

l1988 <- brick("p224r63_1988.grd")
l1988

#con brick carica le bande tutte insieme della nostra immagine 
#metto a confronto le due immagini del 1988 e del 2011

par(mfrow=c(2,1))
plotRGB(l1988, r=4, g=3, b=2, stretch="lin")
plotRGB(l2011, r=4, g=3, b=2, stretch="lin")

#l'immagine del 1988 vediamo che c'è un accenno di prime strade e campi coltivati
