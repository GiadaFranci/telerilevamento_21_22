# 25/03/22

#calcoliamo spectral vegetation index
#indice spettrale, noi partiamo con quelli di vegetazione, la pinata quado sta
#male la pinata cambia colore in molte lunchezze d'onda a nche in quelle che noi non vediamo
#questo cambiamento quindi indica uno stres
#possiamo usare le bande che abbiamo visto fino ada ora per creare questi indici

install.packages("RStoolbox")
library(RStoolbox)

l1992 <- brick("defor1_.jpg")
l1992        

#questa immagie non è georefernziata e ha solo 3 bande
#i valori minimi e massimi ci interessano che vanno da 0 a 255 mentre nelle immagini passate
#erano da 0 a 1. questo perche ogni pixel è diverso da quelli vicini e ha diverse riflettanze
#questo vuole dire che in un immagine con tanti pixel questa è molto pesa, per risolvere
#questa cosa interviene Shannon. Per ridurre la dimensione di un file, parte daun informazione binaria
# 0 o 1, questo concetto si chiama bit. La regola generale è 2^n.
# noi stiamo usando immagini a 8 bit, cioè 2^8 CON 256 VALORI POSSIBILI nella mappa, questo 
#se partiamo da 1, come valore minimo, se invece partiamo da 0 avremmo 255.
#gran parte delle immahgini che usiamo son a 8 bit perche così si risparmia molto spazio

plotRGB(l1992, r=1, g=2, b=3, stretch="lin")

#banda 1 è quella dell'infrarosso vicino, quindi siccome di solito le bade si monato in 
#in sequanza abbaimo la banda 2 che è rossa e la terza che è il green

#importo la seconda immagine

l2006 <- brick("defor2_.jpg")
l2006

plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

#es. plot in multiframe le due immagini una sopra l'altra

par(mfrow=c(2,1))
plotRGB(l1992, r=2, g=1, b=3, stretch="lin")
plotRGB(l2006, r=2, g=1, b=3, stretch="lin")


#calcolo indice speziale DVI (difference vegetation index)
#il massimo di DVI in uscita è 255

dvi1992 = l1992[[1]] - l1992[[2]]
dvi1992

#plorriamo questo primo DVI

cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme
plot(dvi1992, col=cl)

#faccio tutto con il 2006

dvi2006 = l2006[[1]] - l2006[[2]]
dvi2006
plot(dvi2006, col=cl)

#un'altro metodo per calcolare DVI è al posto dei numero, nel caloclo, tra le parentesi mettere
# i nomi es. dvi2006 = l2006$defor2_.1 - l2006$defor2_.2

#possiamo fare una differena tra i due DVI

dvi_diff = dvi1992 - dvi2006

#la warning ci dice che in una piccola parte le immgini non si sovrappongon
#dovuto al modo in cuolo abbaimo scaricato le immagin ua ppotrebbe avere una riga
#di pixel in meno

cld <- colorRampPalette(c('blue','white','red'))(100)
dev.off()
plot(dvi_diff, col=cld)

#così vediamo bene dove è avvenuta la deforestazione 

#31/03/22

#altro infice simile al DVI chimato NDVI viene standardizzato sulla somma delle due 
#bande. importante perche se usiamo due immagni con numeri di bit diverse, per
#standardizzare i due indici  (es.266 e 265) è sufficente standardizzare sui 
#valori di rifletanza delle due bande tot. serve solo per standardizzare il 
#nostro indice

#RANGE del DVI se ho un immagine a 8 bit (per ogniuna delle due bande, nero ingrared
# e rosso, usate abbiamo 256 valori possibili) allora un pixel avrà il massimo di nero infrared 
# 255-0= 255(riflette tutto)e un massimo di rosso(assorbe tutto) 0-255=-255
#quindi il range del DVI è -255 e 255
#RANGE NDVI (255-0)/(255+0)=1 questo è massimo, il minimo è (0-255)/(0+255)=1
#il range del NDVI con immagine a 8 bit è -1 e 1

#l'immagine a 16 bit (per identificare i valori possibili dal n di bitsi fa 16^2)
#con valori 65536 (ogni banda ha un range di 0 e 65535). DVI massimo valore
#65535-0 (rosso) e il minimo è 0-65535 (infrarosso) il range è da -65535 a 65535
#il range dell'NDVI in questp caso è 65535-0 (rosso)/65535+0 e
#0-65535 (infrarosso)/65535+0 quinid il range è -1 e 1 anche qui.

#così vediamo che queste due immagini con bit (risoluzione radiometrica, quanti
#bit ci sono a disposizione in un immagine)diversi possono essere paragonate
#attraverso NDVI.

#ricarico le due immagini della scorsa volta
l1992 <- brick("defor1_.jpg")
l2006 <- brick("defor2_.jpg")
l1992

#per calcolare DVI e NDVi si usano sempre solo le due bande infrarosso e rosso

dvi1992 = l1992[[1]] - l1992[[2]]

#calcolo NDVI del 1992
ndvi1992 = dvi1992 / (l1992[[1]] + l1992[[2]])

#posso anche calcolarlo così (l1992[[1]] - l1992[[2]])/l1992[[1]] + l1992[[2]]

#plotto NDVI

cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
plot(ndvi1992, col=cl)

#multiframe con plotRGB dell'immagine sopra e NDVI sotto

par(mfrow=c(2,1))
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
plot(ndvi1992, col=cl)

#calcoli fatti fino ada ora con il 1992 si rifà con il 2006

dvi2006 = l2006[[1]] - l2006[[2]]
ndvi2006 = dvi2006 / (l2006[[1]] + l2006[[2]])
plot(ndvi2006, col=cl)

#multiframe con plotRGB con NDVI 1992 sopra e NDVI 2006 sotto

par(mfrow=c(2,1))
plot(ndvi1992, col=cl)
plot(ndvi2006, col=cl)

#vicino allao 0 è quasi sempre suolo nudo e nella seconda immagni noi abbiamo 
#valori quasi esclusivamente sullo o soto lo 0

#la libreria (RStoolbox), già caricata, sono strumenti che servono ada analizzare i dati di 
#telerilevameno.
#contine anche una funzione chimata spectralIndices, caòcola una serie di indici spettrali
#come NDVi. per usarla dobbiamo scrivere le bande coinvolte, noi calcoleremo gli 
#indici che coinvologono le bande del rosso, infrarosso e verde.

si1992 <- spectralIndices(l1992, green=3, red=2, nir=1)

#visualizziamo quello che abbaimo creato

plot(si1992, col=cl) #così vediamo tutti gli indici che si èpossonocalcolare per
#un immagine

si2006 <- spectralIndices(l2006, green=3, red=2, nir=1)
plot(si2006, col=cl)

#l'indice NDWI si vanno a calcolare gli stres idrici
#pacchetto per la misura della dieristà dallo spazio, Diversity Indices for
#Numerical Matrices (rasterdiv)
#uso NDVI di rasterdiv

install.packages("rasterdiv")
library(rasterdiv)

#un pacchetto è copNDVI è la media del NDVI dal 1999 al 2017, è l'NDVI di copernicus

plot(copNDVI)
#le parti verdi della mappa sono quelle con più alta biomassa (equatore e doreste di
#conifere delle altre latitudini). vedo coì a scala globale come varia la biomassa
