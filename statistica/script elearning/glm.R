### carichiamo i dati

env=read.table("environment.csv",header=T,sep=";")
pa=read.table("presenceabsence.csv",header=T,sep=";")
abun=read.table("abbundance.csv",header=T,sep=";")

###quale specie e' piu' presente?

apply(pa,2,sum)

### LOC (Nemacheilus barbatulus ) con 24 presenze
#### Cerchiamo  un modello che spieghi la presenza di LOC in funzione delle variabili ambientali a disposizione


### vediamo come si comportano le 11 variabili ambientali

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(env, panel=panel.smooth,
      cex = 1.5, pch = 20, bg="light blue",
      diag.panel=panel.hist, cex.labels =0.8, font.labels=2, lower.panel=panel.cor)
     
### occorre operare una scelta delle variabili indipendenti molte sono tra loro fortemente correlate questo puo' provocare problemi seri come nei modelli di regressione
 ##togliamo das, pno,deb e dbo
      pairs(env[,-c(1,4,7,11)], panel=panel.smooth,
      cex = 1.5, pch = 20, bg="light blue",
      diag.panel=panel.hist, cex.labels =0.8, font.labels=2, lower.panel=panel.cor)
      
env1=env[,-c(1,4,7,11)]
### modello lineare generalizzato 
### per comodita' costruisco un dataset con solo LOC e le variabili ambientali
app=data.frame(LOC=pa$LOC,env1)

### modello saturo
mod1=glm(LOC~.,data=app,family="binomial")
summary(mod1)
mods=step(mod1,direction="both")
summary(mods)
par(mfrow=c(2,2))
plot(mods)
### togliamo l'intercetta
mod1=glm(LOC~.-1,data=app,family="binomial")
summary(mod1)
mods=step(mod1,direction="both")
summary(mods)
### il modello e' una schifezza in termini interpretativi nessuna delle variabili ambientali a disposizione sembra spiegare la probabilita' di presenza
### vediamo in termini predittivi:
ap=ifelse(predict(mods,type="response")>=0.5,1,0)

table(app$LOC,ap)
### il modello e' piu' interessante in termini predittivi, infatti sbaglia solo in 3 casi su 30

## proviamo con un'altra specie un po' meno diffusa 
app=data.frame(VAI=pa$VAI,env1)
mod1=glm(VAI~.-1,data=app,family="binomial")
### con questa specie la funzione glm ha un problema di convergenza
### partiamo dal modello :
mod1=glm(VAI~alt+amm+pH+dur+nit+oxy,data=app,family="binomial") 
mods=step(mod1,direction="both")
### da un punto di vista interpretativo otteniamo qualhe indicazione ma il modello spiega molto poco 

## da un punto di vista predittivo:
ap=ifelse(predict(mods,type="response")>=0.5,1,0)
table(app$VAI,ap)
### sbaglia in 4/30 anche questo modello e' accettabile dal punto di vista predittivo

library(qcc)
#### regressione poisson
##abbondanze totali delle specie
apply(abun,2,sum)
### vediamo LOC che e' la specie piu' abbondante
app=data.frame(LOC=abun$LOC,env1)
mod1.p=glm(LOC~-1+.,data=app,family="poisson")
mods.p=step(mod1.p,direction="both")
summary(mods.p)
### verifichiamo se c'e' overdispersion
qcc.overdispersion.test(app$LOC)

### c'e una debole evidenza in favore dell'overdispersion quindi proviamo con il modello quasipoisson

mod1.p=glm(LOC~-1+.,data=app,family="quasipoisson")
#Il problema e' che per questo modello non e' definito l'AIC quindi non possiamo usare la funzione step e dobbiamo basarci solo sulla proporzione di devianza spiegata
mod2.p= glm(LOC~-1+alt+pen+nit+amm+oxy,data=app,family="quasipoisson")
summary(mod2.p)
mod3.p=glm(LOC~-1+alt+pen+nit+amm+oxy+pH,data=app,family="quasipoisson")
summary(mod3.p)
## vediamo la qualita' delle stime 
par(mfrow=c(2,1))
pp=predict(mods.p,type="response")
plot(app$LOC,pp,xlab="osservati",ylab="stimati",main="modello poisson",pch=20)
abline(c(0,1),col=2)

pp1=predict(mod3.p,type="response")
plot(app$LOC,pp,xlab="osservati",ylab="stimati",main="modello quasi-poisson",pch=20)
abline(c(0,1),col=2)

### oppure
table(app$LOC,round(pp))
table(app$LOC,round(pp1))

#### Provare a replicare l'analisi con la specie  VAI
