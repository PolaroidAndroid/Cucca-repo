### presenza assenza del Phoxinus phoxinus (VAI) nel bacino idrologico del fiume Doub nel massiccio del Jurat
## abbiamo a disposizione le seguenti variabili (tutte ricodificate in modo da assumere valori tra 0 e 9)
## x,y coordinate dei siti di monitoraggio
##Alt altitude 
##Das Distance from the source
##Pen (log(x + 1) where x is the slope (per mil * 100)
##Smm Sezione media (bagnata)
##Qmm debito medio
##Vme Velocità media
##Tmm Temperatura media 
##Con Conduttività
##pH
##Dur total hardness of water (mg/l of Calcium)
##Cl
##SO4
##PO4
##NO3
##N
##O2
##OXY dissolved oxygen 
##DBO biological demand for oxygen
#### alla ricerca del modello ottimo 
dati=read.table("exlogit.csv",header=T,sep=";")
summary(dati)
plot(dati$x,dati$y,pch=20,xlab="x",ylab="y")
points(dati$x[dati$pa==1],dati$y[dati$pa==1],pch=20,col=3)
######################
attach(dati)
aic=AIC(glm(pa~1,data=dati,family=binomial(link="logit")))
for(i in 2:ncol(dati)){
	aic=c(aic,AIC(glm(pa~dati[,i],family=binomial(link="logit"))))
	
}
###min i=5 Das AIC=62.43
i=5
summary(glm(pa~dati[,i],family=binomial(link="logit")))

dati2=dati[,-c(1,5)]
aic=c()
for(i in 1:ncol(dati2)){
	aic[i]=AIC(glm(pa~Das+dati2[,i],data=dati,family=binomial(link="logit")))
}
match(min(aic),aic)
####min i=19, DBO AIC=56.281

dati2=dati[,-c(1,5,21)]

aic=c()
for(i in 1:ncol(dati2)){
	aic[i]=AIC(glm(pa~Das+DBO+dati2[,i],data=dati,family=binomial(link="logit")))
}
match(min(aic),aic)
####min i=8, Tmm AIC=48.54950

summary(glm(pa~Das+DBO+Tmm,data=dati,family=binomial(link="logit"))) ### ~~64% di devianza spiegata

dati2=dati[,-c(1,5,21,10)]

aic=c()
for(i in 1:ncol(dati2)){
	aic[i]=AIC(glm(pa~Das+DBO+Tmm+dati2[,i],data=dati,family=binomial(link="logit")))
}
match(min(aic),aic)
colnames(dati2)[3]

### min i=3 Alt, AIC=47.36471

summary(glm(pa~Das+DBO+Tmm+Alt,data=dati,family=binomial(link="logit")))


dati2=dati[,-c(1,5,21,10,4)]

aic=c()
for(i in 1:ncol(dati2)){
	aic[i]=AIC(glm(pa~Das+DBO+Tmm+Alt+dati2[,i],data=dati,family=binomial(link="logit")))
}
match(min(aic),aic)
### min i=5 Qmm AIC=47.40885 notiamo che l'AIC ricomincia a crescere, controlliamo la devianza spiegata

summary(glm(pa~Das+DBO+Tmm+Alt+Qmm,data=dati,family=binomial(link="logit")))

### studiando il summary del modello vediamo che:

# - con l'introduzione di Qmm la variabile Alt è diventata non significativa
# - la devianza residua è diminuita rispetta al modello precedente ma di pochissimo
# - l'AIC è leggeremente più elevato del precedente

# da un punto di vista statistico è migliore il modello senza Qmm

#### quindi
modello=glm(pa~Das+DBO+Tmm+Alt,data=dati,family=binomial(link="logit"))

### valutiamo il modello in senso previsivo, ovvero il mio modello predice correttamente le presenze e le assenze?

pred=ifelse(predict(modello,type="response")>=0.5,1,0)
### ifelse assegna valore 1 quando il valore di probabilità di osservare 1 è >=0.5, 0 altrimenti
### costruiamo una tabella in cui incrociamo i valori di p/a osservati e quelli predetti dal modello
table(dati$pa,pred)
### il modello sbaglia in solo 6 casi su 92, quindi possiamo affermare che dal punto di vista previsivo il modello è valido