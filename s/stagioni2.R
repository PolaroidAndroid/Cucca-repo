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

#vediamo adesso come si comportano le variabili qualitative - lascio a te l'interpretazione!!!!!!
# STAGIONE
par(mfrow=c(1,1))
par(mfrow=c(2,2))
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 1, yax = 2,col=rainbow)
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 1, yax = 3)
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 2, yax = 3)

#MESE
s.class(pcapm$li, factor(pm10_a$mese), xax = 1, yax = 2)
s.class(pcapm$li, factor(pm10_a$mese), xax = 1, yax = 3)
s.class(pcapm$li, factor(pm10_a$mese), xax = 2, yax = 3)

# PROVA A FARLO ANCHE CON IL GIORNO E LA DIREZIONE DEL VENTO E PROVA A METTERE COLORI DIVERSI


str(pm10_a)
pm10_a$giorno<-as.factor(pm10_a$giorno)
pm10_a$mese<-as.factor(pm10_a$mese)
pm10_a$anno<-as.factor(pm10_a$anno)
pm10_a$Stagione<-as.factor(pm10_a$Stagione)


dd1 <- dudi.hillsmith(pm10_a, scann = FALSE)
scatter(dd1)
