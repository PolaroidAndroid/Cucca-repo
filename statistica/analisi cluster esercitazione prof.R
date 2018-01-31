require(ade4)


data("doubs")

env=doubs$env
fish=doubs$env
summary(env)

# vogliamo costruire una classificazione dei siti a monte del campionamento
# lasciamo le variabili sulla propria scala ed andiamo a calcolare la classificazione
# gerarchi usando il legame completo medio e singolo
# la funzione chiede la matrice (dist), ed il metodo del legame medio (ave)
hc.ave=hclust(dist(env),"ave")
hc.comp=hclust(dist(env),"complete")
hc.ward=hclust(dist(env),"ward.D")
hc.single=hclust(dist(env),"single")
par(mfrow=c(2,2))

plot(hc.ave,hang=-1,cex=0.6,sub="avarage",xlab="sites")
plot(hc.comp,hang=-1,cex=0.6,sub="complete",xlab="sites")
plot(hc.ward,hang=-1,cex=0.6,sub="ward.D",xlab="sites")
plot(hc.single,hang=-1,cex=0.6,sub="single",xlab="sites")     


# mi creo una nuova matrice con dentro i valori con variabili standardizzate

env1=scale(env)

hc.ave1=hclust(dist(env),"ave")
hc.comp1=hclust(dist(env),"complete")
hc.ward1=hclust(dist(env),"ward.D")
hc.single1=hclust(dist(env),"single")
par(mfrow=c(2,2))
X11()
plot(hc.ave1,hang=-1,cex=0.6,sub="avarage",xlab="sites")
plot(hc.comp1,hang=-1,cex=0.6,sub="complete",xlab="sites")
plot(hc.ward1,hang=-1,cex=0.6,sub="ward.D",xlab="sites")
plot(hc.single1,hang=-1,cex=0.6,sub="single",xlab="sites")     

par(mfrow=c(1,1))
plot(doubs$xy,type="n")
text(doubs$xy,rownames(env1))

# prendiamo i dati e vediamo le differenze, 30 e 23 sembrano vicini
env1[c(30,25,24,23),]
# se le prendo non standardizzate, vedo anche qui le differenze
env[c(30,25,24,23),]







