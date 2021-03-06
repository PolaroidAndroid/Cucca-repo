##### regressione
### i modelli lineari in R si stimano tutti con un'unica funzione: lm 
### qui di seguito sviluppiamo un primo esempio, la situazione ideale, due variabili altamente correlate.
# leggiamo i dati salvati in formato testo
ageheight<-read.table("ageheight.txt",header=T,sep=",")
pdf("../images/ageh.pdf")
### grafico a dispersione dei dati per valutare il tipo di relazione esistente tra i due
plot(ageheight)
text(20, 77, c(expression(r^2)))
#aggiungiamo il valore del coefficiente di correlazione
#questo si calcola usando la funzione cor
text(21,77, paste("= ",round(cor(ageheight[,1],ageheight[,2]),2)))
dev.off()
#calcoliamo le principali statistiche descrittive sui dati ora letti. La funzione summary applicata ad un dataframe o ad una matrice fa questo:
summary(ageheight)


#verifichiamo la normalità tramite il quantile plot
pdf("../images/agehqq.pdf")
par(mfrow=c(2,1))
qqnorm(ageheight$height,sub="height")
qqline(ageheight$height,add=T,col=2)
qqnorm(ageheight$age,sub="age")
qqline(ageheight$age,add=T,col=2)
dev.off()
# è importante verificare se vi sono valori anomali in quanto il loro effetto può essere notevole sulla stima del modello 
pdf("../images/agehbox.pdf")
par(mfrow=c(1,2)) #affianchiamo i due boxplot
boxplot(ageheight[,1],main="age")
boxplot(ageheight[,2],main="height")
dev.off()
#ora stimiamo il modello: height=a+b*age + errore
yy<-lm(height~age, data=ageheight)
#per visualizzare in forma tabellare i risultati usiamo la funzione summary
summary(yy)
z<-summary(yy)#memorizzo il risultato per poi estrarre ciò che mi occorre per costruire gli intervalli di confidenza
### costruiamo gli intervalli di confidenza per intercetta e pendenza
t1=qt(0.975,10) #prendo il valore della t con 10 gdl e alfa=0.05
IC.a=c(z$coeff[1,1]-t1*z$coeff[1,2],z$coeff[1,1]+t1*z$coeff[1,2])
IC.b=c(z$coeff[2,1]-t1*z$coeff[2,2],z$coeff[2,1]+t1*z$coeff[2,2])
pdf("../images/laretta.pdf")
par(mfrow=c(1,1))
#visualizziamo la retta di regressione appena stimata evidenziando la distanza tra retta e punti osservati #inoltre aggiungiamo due rette che delimitano una fascia di rette possibili. Queste due rette hanno 
plot(ageheight, pch=20)
abline(a=IC.a[1],b=IC.b[1],lty=5,col="green",lwd=2)
abline(a=IC.a[2],b=IC.b[2],lty=5,col="green",lwd=2)
abline(a=yy$coeff[1],b=yy$coeff[2],col=4)
segments(ageheight$age,ageheight$height,ageheight$age,yy$fitted, col=2)
legend(18,83, c("retta di regressione", "distanza dalla retta","banda di confidenza "),lty=1,col=c(4,2,"green"),cex=0.8)
dev.off()

