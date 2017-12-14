#setto la WD ----
pm10<-read.csv2("data/pm2010.csv",dec=".")

#questo è un pacchetto di pacchetti. Contiene molti pacchetti utili :-)
install.packages("tidyverse")

library(tidyr)
#crea variabile per anno, giorno, mese
pm10_a <- separate(pm10, col="data", into = c("anno","mese","giorno"), sep="-")

pm10_a$mese2 <- as.numeric(pm10_a$mese)

str(pm10_a)        

pm10_a$mese2


pm10_a$giorno2 <- as.numeric(pm10_a$giorno)
str(pm10_a)
# stagioni: primavera (21-03 / 20-06), estate (21-06/22-09), autunno (23-09 /21-12)
# inverno(22-12/20-03)

#divido in stagioni
getSeason <- function(DATES) {
  WS <- as.Date("2010-12-22", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2010-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2010-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2010-9-23",  format = "%Y-%m-%d") # Fall Equinox
  d <- as.Date(strftime(DATES, format="2010-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


k<-getSeason(pm10$data)



pm10_a$Stagione<- k

table(pm10_a$Stagione)


summary(pm10_a)

### PCA ANALYSIS ----
require(ade4)
names(pm10_a)

#estraggo da pm10 solo le variabili numeriche da inserire nella PCA escludendo giorno mese e anno che sono da considerarsi factors
data_pca <- pm10_a[,c(1:9)]

# con nf = 3 gli stò dicendo di tenere tre assi
pcapm<-dudi.pca(data_pca,scannf = FALSE, nf = 3)

#faccio il biplot, cos'è clab.row?
scatter(pcapm, clab.row = 0.5)


#valori dei punteggi per le variabili
pcapm$c1

# CARATTERIZZIAMO GLI ASSI
# se vedi come ci si aspetta mediana media e max stanno insieme perchè sono molto correlati e vanno tutti sulla prima componente
# sulla seconda ci vanno i fattori ambientali tmp, umr, rdz, pgg e prs. Solo che tmp, prs e rdz hanno correlazione negativa rispetto
# a umr  e pgg [pgg è al limite. ha un punteggio abbastanza basso]
# sulla terza si prende tutto il vento

#cerchio di correlazione, i punteggi delle varibili sono plottati su una circonferenza di raggio unitario
par(mfrow=c(2,2))
s.corcircle(pcapm$c1, xax = 1, yax = 2) #plotto prima e seconda
s.corcircle(pcapm$c1, xax = 1, yax = 3) #plotto prima e terza
s.corcircle(pcapm$c1, xax = 2, yax = 3) #plotto seconda e terza

#calcolo gli autovalori
pcapm$eig/sum(pcapm$eig) 

#calcolo la somma cumilata degli autovalori
cumsum(pcapm$eig/sum(pcapm$eig)) # con i primi tre assi spiego circa il 70% che non è male


# da cosa lo capisci??? perchè il 3° dice 0.71?



#vediamo adesso come si comportano le variabili qualitative - lascio a te l'interpretazione!!!!!!
# STAGIONE
par(mfrow=c(1,1))
par(mfrow=c(2,2))
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 1, yax = 2,col=c(1,2,3,4)) 
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 1, yax = 3,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 2, yax = 3,col=c(1,2,3,4))



X11()
par(mfrow=c(2,2))
#MESE



s.class(pcapm$li, factor(pm10_a$mese), xax = 1, yax = 2,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$mese), xax = 1, yax = 3,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$mese), xax = 2, yax = 3,col=c(1,2,3,4))

s.class(pcapm$li, factor(pm10_a$mese), xax = 1, yax = 2,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$mese), xax = 1, yax = 3,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$mese), xax = 2, yax = 3,col=c(1,2,3,4))

# PROVA A FARLO ANCHE CON IL GIORNO E LA DIREZIONE DEL VENTO E PROVA A METTERE COLORI DIVERSI


s.class(pcapm$li, factor(pm10_a$giorno), xax = 1, yax = 2,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$giorno), xax = 1, yax = 3,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$giorno), xax = 2, yax = 3,col=c(1,2,3,4))

# mi sembra chiaro che in estate ed inverno (sia per giorno,mese,stagione)
# sono opposti, quindi i dati sono inversi, e che invece autunno ed inverno
# che sono le stagioni di mezzo la situazione è sovrapposta, non essendoci
# estremi... sbaglio o in inverno ci sono dei brutti picchi???

s.class(pcapm$li, factor(pm10_a$dv), xax = 1, yax = 2,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$dv), xax = 1, yax = 3,col=c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$dv), xax = 2, yax = 3,col=c(1,2,3,4))


# il vento da nord ovest è attaccato all'asse y, mentre quello da
# nord est è attaccato all'asse x nella seconda s class, ma lo fa 
# anche nella terza... non capisco perchè stanno tutti al centro uno
# sull'altro sti scemi.





#### HILLSMITH SU DATI ----

data_hills<-(pm10_a[,1:10])
data_hills$stagione<-pm10_a$Stagione
str(data_hills)
data_hills$stagione<-as.factor(data_hills$stagione)
dd1<-dudi.hillsmith(data_hills,scannf = FALSE,nf=3)
par(mfrow=c(1,1))

scatter(dd1,clab.row=0.5)
# perchè lo scatter sta a fanculo in alto?

dd1$c1

par(mfrow=c(1,1))
s.corcircle(dd1$c1, xax = 1, yax = 2) #plotto prima e seconda
s.corcircle(dd1$c1, xax = 1, yax = 3) #plotto prima e terza
s.corcircle(dd1$c1, xax = 2, yax = 3) #plotto seconda e terza


dd1$eig/sum(dd1$eig) 
cumsum(dd1$eig/sum(dd1$eig))


# quindi aggiungendo le qualitative e facendo la hills 
# i 3 assi mi perdono di dati??? e spiegano quasi il 50% dei dati?

# ma sopratutto ora che so che:
# media mediana e max pesano un botto e sono super correlate
# tmp,rdz,prs sono inversamente prop all'umidità e che la pioggia 
# fa un pò come gli pare e la velocità del vento va per cazzi suoi
# d'inverno la situazione è opposta all'estate e nelle stagioni di 
# mezzo la situazione è mista.... che cazzo gli chiedo ai miei dati?

##### CHE CE FACCIO CO STI DATI?!?!?! ----



