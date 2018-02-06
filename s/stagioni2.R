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

#boxplot


boxplot(data_hills$media~data_hills$stagione,col=col_vector,main="Concentrazioni medie di Pm10")
boxplot(data_hills$max~data_hills$stagione,col=col_vector, main="Concentrazioni massime di Pm10 ")

plot(data_hills$dv,data_hills$media,xlab="Direzione del vento",ylab="Concentrazione Pm10", col=col_vector)


# grafico a coppie

pairs(data_pca,upper.panel =panel.smooth,lower.panel=panel.cor,diag.panel = panel.hist)


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
plot(mod1)
qqnorm(mod1$residuals)
qqline(mod1$residuals,col=2)


mod1s=step(mod1,direction="both")
summary(mod1s)
plot(mod1s)

<<<<<<< HEAD
# mod 2 media+massima


mod2<-lm(media~tmp+max+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)
summary(mod2)
# l'r2 sembra buono qui 0.83

par(mfrow=c(1,1))
par(mfrow = c(2,2))

plot(mod2)

# mi sembra che ci siano dei valori anomali, 48 334 e 50 
# per  residual vs fitted: la nuvola c'è  ed è abbastanza compatta e non proprio del tutto centrata e c'è una 
# lieve nello smooth ...che dovrebbe essere al centro, andando così in alto mi fa pensare a qualcosa di logaritmico
# magari sto a di una cazzata magari no...è la pioggia?)
# nel normal q-q sembrano nomarli sempre eccetto quei due tre valori anomali
# negli standardizzati/fitted c'è sempre una bella nuvola ma c'è sempre questa 
# retta che sembra un logaritmo..quindi direi che non sono omoschedastici, altrimenti dovrebbe essere
# dritta la retta 
# nella leverage è palese che i valori sono oltre la distanza di cook quindi qualcosa mi influenza il tutto
# mi viene da dire che nemmeno questo va bene 
=======

  
####modelli che non usiamo ----

#prima di fare la stepwise elimina mediana e max
data_hills2 <- data_hills[c(1,4:11)]


# a giudicare da questo credo che eliminerò max e mediana e provo a comparare 
# max e media con due modelli differenti in quanto sono sempre correlate e vedo
# se cambia qualcosa 


mod2 <- lm(media ~ tmp + vv + dv + rdz + pgg + umr + prs + stagione, data = data_hills)
mod2bis <- lm(media ~ tmp + vv + rdz + pgg + umr + prs, data = data_hills)
summary(mod2)
summary(mod2bis)


reg2 <- lm(media ~ stagione, data = data_hills)
summary(reg2)
data_hills2$stagione<-as.factor(data_hills2$stagione)
#così cambi il corner point
type2 <- relevel(data_hills2$stagione, ref = "Summer")

reg2.bis <- lm(media ~ type2, data = data_hills)
summary(reg2.bis)
#La stepwise serve a selezionare le variabili che spiegano di
#più. Chiaramente mettendo sia dv che stagione stai aggiungendo #
# tanta carne al fuoco e hai poche osservazioni 
#per alcune direzioni. Vediamo un attimino come si comporta


#modello che usiamo!!!!---- 
  
  
mod2<-lm(media~tmp+max+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)
summary(mod2)

par(mfrow)
mod2s=step(mod2,direction="both")
summary(mod2s)
plot(mod2s)


qqnorm(mod2s$residuals)
qqline(mod2s$residual, col=2)
par(mfrow=c(2,2))
plot(mod2s)
par(mfrow=c(1,1))
plot(mod2s$residuals~data_hills$dv)
plot(mod2s$residuals~data_hills$stagione)
acf(mod2s$residuals)
cor(data_hills[,c(1:9)])


#direi che questo è il modello migliore. Se ti va provati a vedere come funzionao i gam. T
# pacchetto mgcv 
# che cosa vedi in questo modello???
# che ti dice l'output???



#STANDARDIZZO LE VARIABILI ----

data_media<-data_pca[,-2]
data_scaled<-scale(data_media)

data_scaled<-as.data.frame(data_scaled)
modscale<-lm(media~tmp+max+vv+rdz+pgg+umr+prs,data=data_scaled)
plot(modscale)
summary(modscale)
AIC(modscale)
# TEST SU RESIDUI ----
library(nortest)
ad.test(modscale$residuals)

shapiro.test(modscale$residuals)
summary(modscale)

modscale_s=step(modscale,direction="both")
summary(modscale_s)
plot(modscale_s)
plot(modscale_s$residuals)
AIC(modscale_s)

shapiro.test(modscale_s$residuals)

# TENTATIVO GAM ----

library(mgcv)

require(mgcv)

a<-gam(max~s(pgg,by=vv),data=data_media)
plot(a,pages=1)
summary(a)


b<-gam(max ~ s(tmp,by=vv),data=data_media)
plot(b,pages=1)
summary(b)


c<-gam(media~ s(tmp,by=rdz),data=data_media)
plot(c,page=1)
summary(c)


d<-gam(media~s(tmp,by=vv),data=data_scaled)
plot(d,page=1)
summary(d)


e<-gam(media~s(umr,by=pgg),data=data_scaled)
plot(e,page=1)
summary(e)



f<-gam(media~s(umr,by=stagione),data=data_hills)
summary(f)
plot(f)
par(nfrow=(2,2))

gamp<-gam(max~s(pgg,by=stagione),data=data_hills)
summary(gamp)
plot(gamp)


names(data_hills)

g<-gam(media~ s(umr)+s(tmp)+s(pgg)+s(rdz)+s(vv)+s(prs),data=data_hills)
summary(g)
plot(g)
gam.check(gamp)
g2<-predict(g,se=TRUE)




# Verifico la pioggia ----

boxplot(data_pca$pgg)
plot(data_pca$pgg,type="l")
pgg_l<-data_hills$pgg_l<-log(data_pca$pgg-1)


str(pgg_l)


plot(data_hills$max,data_hills$pgg,type="l")
plot(data_hills$max,data_hills$pgg_l,type="l")


#zuur mio amuuur ----


dotchart(data_pca$media,groups=factor(data_hills$stagione))


str(pm10)
names(pm10)
summary(pm10)



# medie e norma di legge ----

data_hills$normaL<-ifelse(data_hills$media<50.000,"low","high")

data_hills$normaL<-as.factor(data_hills$normaL)

plot(data_hills$media~data_hills$normaL)

pm10$data2 <- seq(1,365,1)


ggplot(pm10, aes(x = data2)) +
  geom_path(aes(y = media), color = "red") +
  geom_path(aes(y = mediana), color = "blue") +
  geom_path(aes(y = max), color = "green")





plot()
cor(data_pca)
