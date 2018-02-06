
######## setto la WD ----
# la WD(working directory, dove ho salvato il file)
######### e carico il dataset
pm10 <- read.csv2("data/pm2010.csv", dec = ".")

#installo questo pacchetto per poter dividere i dati in gg-mm-aa
# con install.packages("tidyverse") o su packages - install - tidyverse

# richiamo il pacchetto con il comando library

library(tidyr)
# creo un nuovo data frame in cui gli dico di separare 
# la colonna  data del vecchio (pm10) in gg-mm-aa 

pm10_a <- separate(pm10, col = "data", into = c("anno","mese","giorno"), sep = "-")

# creo un mese2 nel dataset che sia numeric perchè era un caracter
# che mi servirà dopo
pm10_a$mese2 <- as.numeric(pm10_a$mese)



#########divido in stagioni ----

# Le stagioni sono : primavera (21-03 / 20-06), estate (21-06/22-09), autunno (23-09 /21-12)
# inverno(22-12/20-03)# utilizzo la funzione getseason

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

# chiamo la funzione k
k <- getSeason(pm10$data)

# gli dico di "attuare k sul data frame alla colonna Stagione

pm10_a$Stagione<- k
# guardo se ci sono riuscito con il comando table
table(pm10_a$Stagione)

# faccio il summary di pm10_a per essere sicuro

summary(pm10_a)

# Fatto questo che mi servirà dopo parto con le analisi

######## ANALISI ESPLORATIVA ----
# creo un dataset con le sole colonne da 1 a 10
data_hills<-(pm10_a[,1:10])
# impongo che la colonna stagione dei due data frame sia la stessa
data_hills$stagione<-pm10_a$Stagione
# trasformo stagione in factor così da poterlo utilizzare nelle
# analisi (prima era un carattere)
data_hills$stagione<-as.factor(data_hills$stagione)

# creo dei vettori di colori da utilizzare per i boxplot-plot-etc-etc
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
library(RColorBrewer)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))



#  faccio dei boxplot per vedere i valori anomali e l'andamento
# generale nella stagione 
# il primo per media e stagione
boxplot(data_hills$media~data_hills$stagione,col=col_vector,main="Concentrazioni medie di Pm10")
# il secondo per massima e stagione
boxplot(data_hills$max~data_hills$stagione,col=col_vector, main="Concentrazioni massime di Pm10 ")
# il terzo per media e mese, dove si vede bene la stagionalità

boxplot(pm10_a$media~pm10_a$mese2,col=col_vector,main="Concentrazioni medie di Pm10 per mese")
# faccio un plot per la direzione del vento 
plot(data_hills$dv,data_hills$media, main=" Boxplot concentazione Pm10 e direzione del vento",ylab="Concentrazione media di Pm10", col=col_vector)
# il vento da sud est ha valori anomali quindi concentrazione maggiori di pm10


# grafico a coppie

# prima runno le funzioni grafiche per creare il grafico 
# a coppie

# questa è per l'istogramma
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# questo è per la correlazione
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# creo un altro dataframe con le sole variabili quantitative
# che userò dopo per la pca, guardo i nomi per capire chi levare
names(pm10_a)
data_pca <- pm10_a[,c(1:9)]



# runnate le funzioni gli do la funzione pairs per creare il 
# grafico a coppie dicendo dove mi deve mettere l'istogramma,
# la correlazione, e lo smooth ovvero l'andamento generale  

pairs(data_pca,upper.panel =panel.smooth,lower.panel=panel.cor,diag.panel = panel.hist)

########## PCA ANALISYS (SEMPRE ESPORATIVA ) ----
# faccio l'analisi delle componenti principali
# ovvero vedo quali componenti hanno maggior peso e spiegano 
# la maggioranza dei dati

# richiamo il pacchetto ade4 se non lo ho lo scarico
require(ade4)

# faccio la pca con nf = 3 gli stò dicendo di tenere tre assi
pcapm <- dudi.pca(data_pca, scannf = FALSE, nf = 3)

#faccio il biplot per vedere la PCA
scatter(pcapm, clab.row = 0.5)

#valori dei punteggi per le variabili, chi "pesa di più" per ogni
# direzione , ho 3 autovettori principali
pcapm$c1
#calcolo gli autovalori

pcapm$eig / sum(pcapm$eig) 
#calcolo la somma cumilata degli autovalori
cumsum(pcapm$eig / sum(pcapm$eig))
# con i primi tre assi spiego circa il 70% dei dati

# faccio il cerchio di correlazione, i punteggi delle varibili
#sono plottati su una circonferenza di raggio unitario


par(mfrow = c(2,2))# divido la finestra grafica 
#per vedere i cerchi insieme

s.corcircle(pcapm$c1, xax = 1, yax = 2) #plotto prima e seconda componente
s.corcircle(pcapm$c1, xax = 1, yax = 3) #plotto prima e terza componente
s.corcircle(pcapm$c1, xax = 2, yax = 3) #plotto seconda e terza componete

par(mfrow= c(1,1))# faccio tornare la finestra grafica normale

# verifico (graficamente) le variabili qualitative in relazione 
# alle componenti principali che ho calcolato con la pca
# prima con la stagione
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 1, yax = 2, col = c(1,2,3,4)) 
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 1, yax = 3, col = c(1,2,3,4))
s.class(pcapm$li, factor(pm10_a$Stagione), xax = 2, yax = 3, col = c(1,2,3,4))

# poi con la direzione del vento

s.class(pcapm$li, factor(pm10_a$dv), xax = 1, yax = 2, col = rainbow(nlevels(pm10$dv)),clabel = .8)
s.class(pcapm$li, factor(pm10_a$dv), xax = 1, yax = 3, col = rainbow(nlevels(pm10$dv)))
s.class(pcapm$li, factor(pm10_a$dv), xax = 2, yax = 3, col = rainbow(nlevels(pm10$dv)))

######## ANALISI STATISTICA ----
# Regressione multipla ( cioè il modello)
# creo un modello con la funzione lm in cui metto le variabili
# ambientali cioè quantitative (tutte tranne la mediana) e 
# le due variabili qualitative ovvero dv e stagione
# 
mod2<-lm(media~tmp+max+vv+dv+rdz+pgg+umr+prs+stagione,data=data_hills)
summary(mod2)
# ha un ottimo r2 ( o.83) e r2adj (0.82), quindi sembrerebbe andare bene per i dati
# faccio il plot del modello
plot(mod2)
# devo però verificare i residui 

plot(mod2$residuals)
# vedo l'AIC per confrontarlo con altri modelli e
# trovare quello che ha AIC più basso, questo ha 2077
AIC(mod2)

# adesso facci la stepwise in entrambe le direzioni (both)
# per vedere se il modello riesce ancora meglio

mod2s=step(mod2,direction="both")
summary(mod2s)
plot(mod2s)
# questo  ha sempre buon r2 (0.83) e r2adj(0,82)

AIC(mod2s)# qui l'aic è di poco più basso 2075, quindi prenderei 
# questo modello 

# faccio un analisi approfondita dei residui
qqnorm(mod2s$residuals)
qqline(mod2s$residual, col=2)
# plotto i residui , prima da soli e poi in relazione alle variabili qualitative
# per vedere se derivano da queste
plot(mod2s$residuals)
plot(mod2s$residuals~data_hills$dv)
plot(mod2s$residuals~data_hills$stagione)

# NORMA DI LEGGE ----
data_hills$normaL<-ifelse(data_hills$media<50.000,"low","high")

data_hills$normaL<-as.factor(data_hills$normaL)
# sto ancora pensando a come metterla nei dati 

# provate a vedere se vi viene in mente qualcosa runnando tutto XD

