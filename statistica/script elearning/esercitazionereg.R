#### esercitazione sulla regressione
#### dati di velocità del vento e produzione di energia elettrica
vento<-read.table("windspeed.txt",header=T,sep=",")
#### come prima cosa vediamo come si comportano i dati sia graficamente che in base alle statistiche di base

summary(vento)

plot(vento[,2],vento[,1],xlab="speed",ylab="output",type="b")
text(8,1,paste("correlazione=",round(cor(vento)[1,2],2)))
#la correlazione tra le due variabili è elevata dando indicazione che esiste un forte legame lineare tra le due
## quindi un modello lineare può essere molto indicato per descrivere la relazione tra le due
#in realtà sembra esserci una relazione parabolica che potrebbe disturbare la stima del modello


vento.reg1<-lm(output~speed,data=vento)

#### vediamo il risultato
summary(vento.reg1)
##### diagnostica grafica
#### grafico dei residui
plot(vento.reg1,which=1)

### l'intercetta non è significativa è quindi opportuno verificare se tramite una trasformazione delle variabili o di una sola è possibile ottenere un miglioramento nell'adattamento del modello

## trasformiamo la velocità (speed) con la radice quadrata
vento$sqspeed=sqrt(vento$speed)
plot(vento[,3],vento[,1],xlab="sqrt-speed",ylab="output",type="b")
text(2.5,1,paste("correlazione=",round(cor(vento)[3,1],2)))
#### stimiamo il modello e vediamo il risultato
vento.reg2=lm(output~sqspeed,data=vento)
summary(vento.reg2)
plot(vento.reg2,which=1)


