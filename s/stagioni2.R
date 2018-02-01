#setto la WD ----
pm10 <- read.csv2("data/pm2010.csv", dec = ".")

#questo è un pacchetto di pacchetti. Contiene molti pacchetti utili :-)
#install.packages("tidyverse")

library(tidyr)
#creO variabile per anno, giorno, mese
pm10_a <- separate(pm10, col = "data", into = c("anno","mese","giorno"), sep = "-")

pm10_a$mese2 <- as.numeric(pm10_a$mese)

str(pm10_a)        

pm10_a$mese2


pm10_a$giorno2 <- as.numeric(pm10_a$giorno)
str(pm10_a)
# stagioni: primavera (21-03 / 20-06), estate (21-06/22-09), autunno (23-09 /21-12)
# inverno(22-12/20-03)

#divido in stagioni ----
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


k <- getSeason(pm10$data)



pm10_a$Stagione<- k

table(pm10_a$Stagione)


summary(pm10_a)

### PCA ANALYSIS ----
require(ade4)
names(pm10_a)

#estraggo da pm10 solo le variabili numeriche da inserire nella PCA escludendo giorno mese e anno che sono da considerarsi factors
data_pca <- pm10_a[,c(1:9)]

# con nf = 3 gli stò dicendo di tenere tre assi
pcapm <- dudi.pca(data_pca, scannf = FALSE, nf = 3)

#faccio il biplot, cos'è clab.row?
# clab.row serve a gestire la grandezza dei quadratini delle osservazioni
# clab.col invece controlla le variabili
scatter(pcapm, clab.row = 0.5)


#valori dei punteggi per le variabili
pcapm$c1

# CARATTERIZZIAMO GLI ASSI
# se vedi come ci si aspetta mediana media e max stanno insieme perchè sono molto correlati e vanno tutti sulla prima componente
# sulla seconda ci vanno i fattori ambientali tmp, umr, rdz, pgg e prs. Solo che tmp, prs e rdz hanno correlazione negativa rispetto
# a umr  e pgg [pgg è al limite. ha un punteggio abbastanza basso]
# sulla terza si prende tutto il vento

#cerchio di correlazione, i punteggi delle varibili sono plottati su una circonferenza di raggio unitario
par(mfrow = c(2,2))
s.corcircle(pcapm$c1, xax = 1, yax = 2) #plotto prima e seconda
s.corcircle(pcapm$c1, xax = 1, yax = 3) #plotto prima e terza
s.corcircle(pcapm$c1, xax = 2, yax = 3) #plotto seconda e terza

#calcolo gli autovalori
pcapm$eig / sum(pcapm$eig) 

#calcolo la somma cumilata degli autovalori
cumsum(pcapm$eig / sum(pcapm$eig)) # con i primi tre assi spiego circa il 70% che non è male


# da cosa lo capisci??? perchè il 3° dice 0.71?
# gli autovalori corrispondono alla quantità di variabilità spiegata lungo quella direzione (autovettore)
# se divido ogni singolo autovalore per la somma di tutti gli autovalori (riga 80) ottengo la % che ogni 
#singola componente spiega. Facendo la somma cumulata (riga 83) non faccio altro che:
# 1c , 1c + 2c, 1c + 2c + 3c. La somma di tutte chiaramente mi darà 100 (o meglio 1)
# quindi in questo modo io so che tenedo le prime tre componenti io riesco a vedere il 71% dell'informazione presente
#nel mio dataset

#vediamo adesso come si comportano le variabili qualitative - lascio a te l'interpretazione!!!!!!
# PCA STAGIONE ----
par(mfrow= c(1,1))
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 1, yax = 2, col = c(1,2,3,4)) 
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 1, yax = 3, col = c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 2, yax = 3, col = c(1,2,3,4))



s.class(pcapm$li, factor(pm10_a$Stagione), xax = 2, yax = 3, col = rainbow(4)) #se volessi usare la funzione rainbow

#un modo carino per selezionare i colori
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

# questo comando crea un oggetto che si chiama color che contiene tutti e 433 i colori base di R escluse le scale di grigio
# poi basta fare col = sample(color, n) all'intero di una funzione che usa come argument col
# dove n è il numero di colori che devi utilizzare

#alternativa se 433 colori sono troppi
library(RColorBrewer)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# questa funzione produce un vettore di 74 colori

# Interpretazione (la devi fare tu), stai attenta! :
# 1 - come si dispongono le ellissi rispetto alle tre componenti? 
# 2 - ci sono delle direzioni in cui la variazione è più evodente e altre invece dove non sembra esserci?
# 3 - cosa rappresentano le tre componenti?
# 4 - l'asse maggiore delle ellissi su che componente si allinea?
# 5 - che cosa mi racconta quindi il grafico?


#facile dire la devi fare tu gne gne gne gne XD alloraaaa( sto rosicando che l'avevo
#già fatta ma cambiando computer ho fatto un panico... dunque)
# rispondo alle domande per stagioni:

# 1) le ellissi praticamente si sovrappongono rispetto alle componenti, in particolare
# le componenti 1 e 3 , mentre sono trasversali tra 1 e 2 
# 2) se per variazione intendi i picchi, in inverno c'è variazione
# 3) le tre componenti sono la 1a la concentrazione del pm10 la 2a le variabili ambientali, la 3a la velocità del vento
# 4) l'asse maggiore è parallelo alla componente che rappresenta la concentrazione
# 5) essendo così tutte attaccate mi sa che il fattore stagione posso anche non cagarlo
# posso invece indagare su st'inverno che esce fuori dagli schemi no??






# attenta mese ha 12 livelli come factor non 4 come le stagioni se usi solo 4 colori non vedi gli altri mesi
# approfitto per usare la funzione che hai usato prima 

par(mfrow = c(1,1))
s.class(pcapm$li, factor(pm10_a$mese), xax = 1, yax = 2, col = sample(color, 12), clabel = 0.8)
s.class(pcapm$li, factor(pm10_a$mese), xax = 1, yax = 3, col = sample(color, 12), clabel = 0.8)
s.class(pcapm$li, factor(pm10_a$mese), xax = 2, yax = 3, col = sample(color, 12), clabel = 0.8)

# GIORNO ----
# chiaramente dal giorno ci capisci poco perchè stai mischiando periodi diversi dell'anno e la natura se ne frega che è il
# primo o il trenta del mese
s.class(pcapm$li, factor(pm10_a$giorno), xax = 1, yax = 2,col = sample(color, 31))
s.class(pcapm$li, factor(pm10_a$giorno), xax = 1, yax = 3,col = sample(color, 31))
s.class(pcapm$li, factor(pm10_a$giorno), xax = 2, yax = 3,col = sample(color, 31))

# direi che a questo punto non mi interessa per niente il fattore giorno e mese,
# perche già con la stagione ho abbastanza livelli non aggiungo ulteriore confusione al tutto

#DIREZIONE DEL VENTO ----
s.class(pcapm$li, factor(pm10_a$dv), xax = 1, yax = 2, col = sample(col_vector, nlevels(pm10$dv)))
s.class(pcapm$li, factor(pm10_a$dv), xax = 1, yax = 3, col = sample(col_vector, nlevels(pm10$dv)))
s.class(pcapm$li, factor(pm10_a$dv), xax = 2, yax = 3, col = sample(col_vector, nlevels(pm10$dv)))


s.class(pcapm$li, factor(pm10_a$dv), xax = 1, yax = 2, col = rainbow(nlevels(pm10$dv)),clabel = .8)
s.class(pcapm$li, factor(pm10_a$dv), xax = 1, yax = 3, col = rainbow(nlevels(pm10$dv)))
s.class(pcapm$li, factor(pm10_a$dv), xax = 2, yax = 3, col = rainbow(nlevels(pm10$dv)))


# ARI-DOMANDE
# 1) le ellissi sono sempre accozzagliate l'una sull'altra rispetto alle componenti
# 2) mi sembra che il vento da sudest ha sempre sti picchi malefici che escono dagli schemi
# 3) le componenti sono sempre le stesse
# 4) l'asse maggiore sta sempre sulla concentrazione di pm10
# 5) anche qui stanno tutte una sull'altra non ci sta niente di "eclatante" apparte i picchi di vento 
# da sudest

# fatti le stesse domande che ti ho scritto prima


# HILL-SMITH ----




#### HILLSMITH SU DATI ----

data_hills<-(pm10_a[,1:10])
data_hills$stagione<-pm10_a$Stagione


str(data_hills)
data_hills$stagione<-as.factor(data_hills$stagione)
dd1 <- dudi.hillsmith(data_hills, scannf = FALSE, nf = 3)
par(mfrow = c(1,1))

scatter(dd1, clab.row = 0.5, posieig = "bottom")

# perchè lo scatter sta a fanculo in alto?
# stavi pensando a cosa rappresentano i valori della hills e quali sono le 
# componenti principali in questo caso...
dd1$c1

par(mfrow=c(1,1))
s.corcircle(dd1$c1, xax = 1, yax = 2) #plotto prima e seconda
s.corcircle(dd1$c1, xax = 1, yax = 3) #plotto prima e terza
s.corcircle(dd1$c1, xax = 2, yax = 3) #plotto seconda e terza


dd1$eig/sum(dd1$eig) 
cumsum(dd1$eig/sum(dd1$eig))


# quindi aggiungendo la stagione  e la dv e facendo la hills 
# i 3 assi mi perdono quantità di informazione e spiegano quasi il 50% dei dati.


# ma sopratutto ora che so che:
# media mediana e max sono super correlate, quindi mi conviene tenerne una sola
# tmp,rdz,prs sono inversamente prop all'umidità e che la pioggia sta a se
#velocità del vento va per fatti suoi e tira i miei dati
# d'inverno ho dei picchi particolari che non so se hanno valenza
# e mi sembra che la direzione NW può avere una valenza 




# provo a vedere sta velocità del vento che problemi ha 


hist(pm10_a$vv)

plot(pm10_a$media~pm10_a$vv,col="2",pch="20")
boxplot(pm10_a$vv)

#faccio un modello di regressione multipla con tutto dentro
modvv<-lm(vv~.,data=data_hills)
summary(modvv)
# la vv del vento con la direzione nord fa sempre vedere qualcosa
# per me c'è qualcosa sotto, dice che sono significative 
# umr,pgg,prs



names(data_hills)

corr<-cor(data_hills[,1:9])
summary(corr)

# potresti cominciare a fare una bella regressione multipla per vedere se le variabile che hai spiegano la conc di pm10
# poi commentiamo meglio magari su skype!!!!

### REGRESSIONE MULTIPLA ----
# mod 1 tutto dentro

mod1<-lm(media~.,data=data_hills)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))
qqnorm(mod1$residuals)
qqline(mod1$residuals,col=2)


mod1s=step(mod1,direction="both")
summary(mod1s)
par(mfrow=c(2,2))
plot(mod1s)

# mod 2 media+massima
mod2<-lm(media~tmp+max+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)
summary(mod2)
# l'r2 sembra buono qui 0.83
par(mfrow = c(2,2))
plot(mod2)
# mi sembra che ci siano dei valori anomali, 48 334 e 50 <- LUI IN AUTOMATICO SI SEGNALA I TRE VALORI PIU' ESTREMI 
# NON E' DETTO CHE SIANO VALORI ANOMALI
# per  residual vs fitted: la nuvola c'è  ed è abbastanza compatta e non proprio del tutto centrata e c'è una 
# lieve nello smooth ...che dovrebbe essere al centro, andando così in alto mi fa pensare a qualcosa di logaritmico
# SEMBREREBBE ESSERCI UNA LEGGERA ETEROSCHEDASTICITA'. MA PER QUELLO CHE DOVETE FARE VOI VA BENE. AVENDO DELLE VARAIBILI FATTORIALI
# PLOTTA ANCHE I RESIDUI vs VARAIBILE FATTORIALE (vento e stagione)
# magari sto a di una cazzata magari no...è la pioggia?)
# nel normal q-q sembrano nomarli sempre eccetto quei due tre valori anomali
# MAGARI AVERE DEI RESIDUI COSi' NORMALI
# negli standardizzati/fitted c'è sempre una bella nuvola ma c'è sempre questa 
# retta che sembra un logaritmo..quindi direi che non sono omoschedastici, altrimenti dovrebbe essere
# dritta la retta 
# SE VUOI VEDERE LA DISTANZA DI COOK FAI plot(mod1, which = 4)
# nella leverage è palese che i valori sono oltre la distanza di cook quindi qualcosa mi influenza il tutto
# mi viene da dire che nemmeno questo va bene 
par(mfrow=c(1,1))

mod2<-lm(media~tmp+max+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)


summary(mod2)

mod2s=step(mod2,direction="both")
summary(mod2s)
par(mfrow=c(2,2))
plot(mod2s)
# anche con la stepwise ho problemi nei residui, i valori anomali sono sempre gli stessi
# qui si vede nella leverage un "distacco in due nuvole" non so se ha un significato o meno

qqnorm(mod2s$residuals)
qqline(mod2s$residual, col=2)
par(mfrow=c(2,2))
plot(mod2s)
par(mfrow=c(1,1))
plot(mod2s$residuals~data_hills$dv)
plot(mod2s$residuals~data_hills$stagione)
acf(mod2s$residuals)
cor(data_hills[,c(1:9)])


#direi che questo è il modello migliore. Se ti va provati a vedere come funzionao i gam. Ti passo lo zuur appena posso. 
# sono un po' perplesso dal fatto che la max si prenda tutta la variabilità però se la prof vi ha detto di metterla lo sa 
# meglio lei di sicuro
# pacchetto mgcv 

# che cosa vedi in questo modello???
# che ti dice l'output???

#iniziate a buttare giu anche il testo


mod4<-lm(media~tmp+vv+dv+umr+stagione,data=data_hills)
summary(mod4)
anova(mod4,mod2s)

# mod 3 max + mediana
mod3<-lm(max~tmp+mediana+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)
summary(mod3)
# anche qui l' r2 è buono 0.73

plot(mod3)
# per il residual vs fitted qui va un pò meglio
# i valori anomali qui sono 335, 78, 48
# il normal qq un pò peggio 
# mentre finalmente non ho più eteroschedasticità con la leverage
# cook rimane sempre un pò un panico


mod3s=step(mod3,direction="both")
summary(mod3s)
plot(mod3s)
# pure con la stepwise mi sembra che più o meno siamo li...

qqnorm(mod3s$residuals)
qqline(mod3s$residuals,col=2)

# mod 4 mediana

mod4<-lm(mediana~tmp+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)
summary (mod4)
plot(mod4)

mod4s=step(mod4,direction="both")
summary(mod4s)
plot(mod4s)
# r2 bassino e valori anomali 335,334,50
# i residui hanno uno smooth inverso a quello dei modelli prima

# mod 5 media

mod5<-lm(media~tmp+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)
summary(mod5)           
plot(mod5)           

mod5s=step(mod5,direction="both")
summary(mod5s)
plot(mod5s)
# anchce qui r2 basso, e residui inversi con valori anomali 335,334,50

# mod 6 max 

mod6<-lm(max~tmp+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)     
summary(mod6)
plot(mod6)

mod6s=step(mod6,direction="both")
summary(mod6s)
plot(mod6s)

# dai residui qui abbiamo 78 50 e 335 che sono anomali
# smooth sempre inverso a quello di prima con un'inclinazione specifica.

# se dovessi scegliere un modello sceglierei il 3° perchè ha un buon r2 e non mi
# sembra messo malissimo con i residui...
# ma sicuro non ho capito nulla e mi redarguirai per bene ps. lo so che mi odi

# ricorda di provare il modello standardizzando le variabili!!!!


#NON TROVO LA PARTE CON I GAM



