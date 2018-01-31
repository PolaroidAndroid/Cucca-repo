data(CO2)
##### prendiamo i dati di un esperimento in cui si misura l'assorbimento di CO2 da parte di piante provenienti
#### da due aree diverse e sottoposte in egual numero a trattamenti diversi 
par(mfrow=c(1,2))
boxplot(CO2$uptake~CO2$Type)
boxplot(CO2$uptake~CO2$Treatment*CO2$Type,cex.axis=0.5,las=2)

# invio e vedo la rappresentazione grafica, sia l'effetto tipo che quello
# trattamento sembrano essere significativi, i boxplot del quebec sembrano
# essere sovrapposti, mississipi si sovrappone a quebec
# gli effetti di interazione sono ambigui

par(mfrow=c(1,1))
interaction.plot(CO2$Type,CO2$Treatment,CO2$uptake,xlab="Type",ylab="Treatmet")
# vedo l'interazione tra trattamento e tipo tanto che le linee sono parallele
# questa interazione non ha effetto sulla media ma potrebbe sulla variabilità
interaction.plot(CO2$Plant,CO2$Treatment,CO2$uptake,xlab="Type",ylab="Treatmet")
# tra pianta e trattamento non sembra esserci alcuna interazione,
# le interazioni che osserviamo non agiscono sulla media del gruppo
# ma possono interagire sulla variabilità, potrei avere errori non omoschedastici


w=CO2$Treatment=="chilled"
pp=rep(20,nrow(CO2))
pp[w]=22
### vediamo l'asorbimento in funzione della concentrazione al variare delle diverse caratteristiche
par(mfrow=c(1,1))
plot(CO2$uptake~CO2$conc,pch=pp,col=CO2$Type,xlab="Concentrazione CO2",ylab="assorbimento CO2")
legend("topleft",c("Quebec non raffreddate","Quebec raffreddate", "Mississipi non raffredate", "Mississipi raffredate"), pch=c(20,22,20,22),col=c(1,1,2,2),cex=0.5)
# non c'è un unica relazione, ci possono essere più rette di regressione
# sembrano abbastanza separati sopratutto alle alte concentrazioni


cor(CO2$uptake,CO2$conc)
cor.test(CO2$uptake,CO2$conc)

#####c'Ã¨ una correlazione positiva tra assorbimento di CO2 e concentrazione atmosferica
# costruiamo un modello con solo le variabili quantitative
mod0=lm(uptake~conc,data=CO2)
summary(mod0)### Rquadro basso

# c'è un R quadro bassissimo, c'è una relazione significativa tra le variabili
# e che il coeff di correlazione parziale p 0,01

plot(mod0)

# i residui sembrano avere un po' di distacco, qq plot , i residui sndard. 
# il modello non spiega ancora nulla

shapiro.test(residuals(mod0)) ##abbastanza bene
# dal grafico iniziale sembrano essere abbastanza separati, costruisco il modello
# che chiamo con solo la variabile type
mod1=lm(uptake~conc+Type,data=CO2)##intercetta diversa per ogni gruppo
summary(mod1)## meglio
# è tutto significativo, una grande fetta di variabilità è spiegata dalla 
# diversa provenienza delle piante, ma è sufficiente introdurre solo type
# ma implica una diversa retta per ogni gruppo?, metto l'interazione
# tr avariabile quantitativa e qualitativa
mod2=lm(uptake~conc*Type,data=CO2)### intercetta e inclinazione diverse per ogni gruppo
summary(mod2)#ancora meglio!
# l'R2 è salito di poco, ma tutto quello che ho aggiunto è significativo
# c'è una diversa retta per ogni tipo, la media del quebec ha un inclinazione
# a diversa, 

# retta di regressione quebec y=23,5+0,023*concentrazione
# la retta di mississipi y=15,5+0,013* concentrazione
# sono quindi significativamente diverse
plot(CO2$uptake~CO2$conc,pch=pp,col=CO2$Type,xlab="Concentrazione CO2",ylab="assorbimento CO2")
legend("topleft",c("Quebec non raffreddate","Quebec raffreddate", "Mississippi non raffredate", "Mississippi raffredate"), pch=c(20,22,20,22),col=c(1,1,2,2),cex=0.5)
w=CO2$Type=="Quebec"
abline(lm(uptake~conc,data=CO2[w,]))
abline(lm(uptake~conc,data=CO2[!w,]),col=2)
# quella in nero è la prima y=0,013 (intercetta) inclinazione(15,5)
# la rossa l'altra


# l'esempio coinvolge parametri grafici come legend 


### corner point \`e Quebec se vogliamo cambiare allora usiamo la funzione relevel
Type.new=relevel(CO2$Type,ref="Mississippi")
summary(lm(uptake~conc*Type.new,data=CO2))
### senza intercetta
summary(lm(uptake~conc*Type.new-1,data=CO2))


