### regressione multipla
dati1<-read.table("MResempio1.csv",header=T,sep=";")
#pdf("../images/MR1pairs.pdf")
pairs(dati1)
#dev.off()
#### matrice di correlazione
mr1=cor(dati1)
#isolo le correlazione tra le variabili indipendenti

Rxx<-as.matrix(mr1[1:2,1:2]) 
#isolo le correlazioni tra variabile risposta e variabili indipendenti
Ry<-as.matrix(mr1[1:2,3]) 
B=t(Ry)%*%solve(Rxx)%*%Ry #R quadro
sqrt(B) #coefficiente di correlazione multiplo
##########
#ora stimo il modello di regressione tra water e temperature
y1<-lm(WC~Temp, data=dati1)
# e quello con tutte le variabili
y2<-lm(WC~Temp+TMG, data=dati1)

#vediamo i risultati
summary(y1)
summary(y2)
######### esempio 2 airquality data in new york
data(airquality)
#### visualizziamo i dati nel loro complesso
pairs(airquality,panel=panel.smooth)

summary(airquality)
### ci sono dati mancanti, vanno tolti per evitare problemi con le procedure di stima
### individuiamo la posizione dei dati mancanti (le righe)
wo<-is.na(airquality$Ozone)
ws<-is.na(airquality$Solar.R)
#eliminiamo i dati mancanti
air<-airquality[!(wo | ws),]
summary(air) #ora non ci sono più dati mancanti
#correlazione
mr<-cor(air)

#isolo le correlazione tra le variabili indipendenti

Rxx<-as.matrix(mr[2:6,2:6]) 
#isolo le correlazioni tra variabile risposta e variabili indipendenti
Ry<-as.matrix(mr[2:6,1]) 
B=t(Ry)%*%solve(Rxx)%*%Ry #R quadro
sqrt(B) #coefficiente di correlazione multiplo che non è altissimo
#############
#grafici a dispersione con alcune aggiunte:
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{#con questa funzione vogliamo aggiungere in pairs i valori di correlazione tra le variabili esaminate
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.hist <- function(x, ...)
{#con questa funzione vogliamo aggiungere gli istogrammi delle singole variabili
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


pairs(air,panel=panel.smooth,lower.panel=panel.cor,diag.panel=panel.hist)
#l'ozono è molto asimmetrico forse bisogna trasformarlo su scala log
##### esploriamo la variazione dell'ozono nei vari mesi
boxplot(Ozone~factor(Month),data=air,col=rainbow(5))
### le mediane dei boxplot differiscono in modo sostanziale tra un mese e l'altro, mi aspetto che il mese abbia un effetto rilevante sulla concentrazione di ozono
#### esploriamo la variazione dell'ozono rispetto ai giorni del mese
boxplot(Ozone~factor(Day),data=air,col=rainbow(5))
###molta variabilità mi aspetto che l'effetto del giorno sia poco rilevante
### verifichiamo quanto le variabili (a parte il mese e il giorno) si comportino come normali, in particolare l'ozono
qqnorm(air$Ozone)
qqline(air$Ozone,col=2)
###### trasfomando l'ozono su scala log
qqnorm(log(air$Ozone))
qqline(log(air$Ozone))

##### l'ozono su scala log si comporta meglio
air.log<-air
air.log$Ozone<-log(air[,1])
pairs(air.log,panel=panel.smooth,lower.panel=panel.cor,diag.panel=panel.hist)

yy=lm(Ozone~., data=air.log)
yy1=step(yy,direction="both")
