#### proporzioni
xx<-rbinom(1000,10,0.3)
table(xx)
pdf("images/binom1.pdf")


par(mfrow=c(2,2))
xx<-c(0:10)
y<-data.frame(y=dbinom(xx,10,0.1))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,1),main="Bin(10,0.1)",names.arg=xx,space=1)
y<-data.frame(y=dbinom(xx,10,0.5))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,1),main="Bin(10,0.5)",names.arg=xx,space=1)

y<-data.frame(y=dbinom(xx,10,0.7))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,1),main="Bin(10,0.7)",names.arg=xx,space=1)


y<-data.frame(y=dbinom(xx,10,0.9))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,1),main="Bin(10,0.9)",names.arg=xx,space=1)
dev.off()

pdf("images/binom2.pdf")

par(mfrow=c(2,2))
xx<-1:5
y<-data.frame(y=dbinom(xx,5,0.5))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,1),main="Bin(5,0.5)",names.arg=xx,space=1)
xx<-1:10
y<-data.frame(y=dbinom(xx,10,0.5))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,1),main="Bin(10,0.5)",names.arg=xx,space=1)
xx<-1:20
y<-data.frame(y=dbinom(xx,20,0.5))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,1),main="Bin(20,0.5)",names.arg=xx,space=1)

xx<-1:50
y<-data.frame(y=dbinom(xx,50,0.5))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,1),main="Bin(50,0.5)",names.arg=xx,space=1)
dev.off()

pdf("images/binom3.pdf")

par(mfrow=c(2,2))
xx<-c(0:50)
y<-data.frame(y=dbinom(xx,50,0.1))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,0.3),main="Bin(50,0.1)",names.arg=xx,space=1)
y<-data.frame(y=dbinom(xx,50,0.5))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,0.3),main="Bin(50,0.5)",names.arg=xx,space=1)

y<-data.frame(y=dbinom(xx,50,0.7))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,0.3),main="Bin(50,0.7)",names.arg=xx,space=1)


y<-data.frame(y=dbinom(xx,50,0.9))
barplot(as.matrix(y),beside=T,col=4,ylim=c(0,0.3),main="Bin(50,0.9)",names.arg=xx,space=1)

dev.off()
##### Esempio delle balene in CAST
### H0: p=0.4 l'apparecchio spaventa balene non ha effetto
### H1: p>0.4 l'apparecchio spaventa balene ha effetto
########## osserviamo n=30 balene di cui 15 vanno via 
###### se H0 è vera il modello di riferimento è Bin(30,.40)
sum(dbinom(c(15:25),30,0.4))### è 0.1754 accetto H0 

#### approssimazione normale
#verifichiamo le condizioni
m=30*0.4 ## è 12
ss=30*0.4*0.6 #è 7.2
####la prima condizione  è verificata vediamo la seconda
abs((1/sqrt(30))*(sqrt(0.6/0.4)-sqrt(0.4/0.6)))<0.3
#### anche la seconda è verificata per cui il risultato del test non dovrebbe cambiare usando l'approssimazione normale
pnorm(15,m,ss,lower.tail=F) ## è 0.13177
### a applicando la correzione ottengono un pessimo risultato in termini numerici ma non inficio il risultato del test
pnorm(15.5,m,ss,lower.tail=F) ## è 0.09605322


