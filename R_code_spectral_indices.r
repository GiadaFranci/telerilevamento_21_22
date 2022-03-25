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
