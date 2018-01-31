###### esempio di media e mediana per una distribuzione simmetrica
##### genero dati da una distribuzione normale(10,1)

x<-rnorm(8000,mean=2,sd=8)
mean(x)#media
sqrt(var(x))#deviazione standardad
#asimmetria
mean((x-mean(x))^3)/(sqrt(var(x)))^3
pdf("images/distribnorm.pdf") #istruzione per scrivere i grafici su di un file pdf
hist(x,nclass=20,main=" ",prob=T) #istogramma delle frequenze relative

abline(v=mean(x),col=2,lwd=2) #aggiungo linee verticali
abline(v=median(x),col="blue",lwd=2)
##### aggiungo la legenda
legend(20,0.04,c("mediana","media"),lty=1,col=c("blue","red"))
### chiudo il file pdf
dev.off()

##### media e mediana per una distribuzione asimmetrica
#chiquadro con 2 gradi di libertà
y<-rchisq(8000,df=2)
mean(y)#media
median(y) #mediana
sqrt(var(y))#deviazione standard
#asimmetria
mean((y-mean(y))^3)/(sqrt(var(y)))^3
pdf("images/distribchisq2.pdf") #istruzione per scrivere i grafici su di un file pdf
hist(y,nclass=20,main=" ",prob=T) #istogramma delle frequenze relative
abline(v=mean(y),col=2,lwd=2) #aggiungo linee verticali
abline(v=median(y),col="blue",lwd=2)
##### aggiungo la legenda
legend(10,0.3,c("mediana","media"),lty=1,col=c("blue","red"))
### chiudo il file pdf
dev.off()
####### quando la media funziona davvero male
#x1<-rnorm(1000,0,1)
#x1[1:300]<-x1[1:300]+50
x1<-c(rnorm(1000,0,1),rnorm(1000,20,10))
pdf("images/badmean.pdf")
hist(x1,nclass=20,main=" ")
abline(v=mean(x1),col=2,lwd=2) #aggiungo linee verticali
abline(v=median(x1),col="blue",lwd=2)
legend(25,400,c("mediana","media"),lty=1,col=c("blue","red"))
dev.off()
############# Esempio con dati reali
######## lettura dati da file di testo
m<-read.table("ticino1.txt",header=T)
row.names(m)[19]<-">90"
########## grafici a barre con media e mediana
##### calcolo della media
centri.cl<-c(0,3,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,84.5,95)
##### età media alla morte 1950
round(sum(centri.cl*m$n1950)/sum(m$n1950))
###### età media alla morte 2005
round(sum(centri.cl*m$n2005)/sum(m$n2005))
##### età mediana vedo quale è il valori in centri.cl che corrisponde al 50% dei dati 
cbind(centri.cl,(cumsum(m$n1950)/sum(m$n1950)),cumsum(m$n2005)/sum(m$n2005))
####  per il 1950 il 50% dei dati si colloca a cavallo tra  67 e 72 quindi (67+72)/2=69.5 che arrotondiamo a 64
### per il 2005 si trova tra 77 e 84 quindi (77+84)/2=80.5 che arrotondiamo a 80
ww<-c(1,rep(5,16),10,10)
#### grafici a barre
pdf("images/ticino_50.pdf")
barplot(m[,3],axes=F,cex.names=0.6,width=ww,names.arg=row.names(m),space=0,col="yellow",main="Morti % per classi di età nel Caton Ticino\n anno 1950")
abline(v=63,col=2)
abline(v=69,col=4)
legend(0,10, c("mediana","media"),lty=1,col=c(4,2))
dev.off()
pdf("images/ticino_05.pdf")
barplot(m[,4],axes=F,cex.names=0.6,width=ww,names.arg=row.names(m),space=0,col="yellow",main="Morti % per classi di età nel Caton Ticino\n anno 2005")
abline(v=78,col=2)
abline(v=80,col=4)
legend(0,20, c("mediana","media"),lty=1,col=c(4,2))
dev.off()
####################### grafici
data(iris)#con questa istruzione richiamo un dataset dalle librerie dati di R
 plot(iris$Sepal.Length,iris$Sepal.Width,xlab="Length",ylab="Width",main="IRIS Sepal")
 w<-iris$Species=="setosa"
 points(iris$Sepal.Length[w],iris$Sepal.Width[w],pch=20,col=2)
 w<-iris$Species=="versicolor"
 points(iris$Sepal.Length[w],iris$Sepal.Width[w],pch=20,col=4)
 w<-iris$Species=="virginica"
 points(iris$Sepal.Length[w],iris$Sepal.Width[w],pch=20,col=3)
 legend(6.5,4.2,c("setosa","versicolor","virginica"),pch=20,col=c(2,4,3))
 
 pdf("images/irissepal-pet.pdf") 
 plot(iris$Sepal.Length,iris$Petal.Width,xlab="Sepal Length",ylab="Petal Width",main="IRIS ")
 #creo una variabile logica (TRUE/FALSE) per selezionare i valori corrispondenti a ciascuna specie e quindi colorarli diversamente sul grafico
 w<-iris$Species=="setosa" #istruzione di confronto 
 points(iris$Sepal.Length[w],iris$Petal.Width[w],pch=20,col=2)
 w<-iris$Species=="versicolor"
 points(iris$Sepal.Length[w],iris$Petal.Width[w],pch=20,col=4)
 w<-iris$Species=="virginica"
 points(iris$Sepal.Length[w],iris$Petal.Width[w],pch=20,col=3)
 #aggiungo una legenda
 legend(7.0,0.5,c("setosa","versicolor","virginica"),pch=20,col=c(2,4,3))
dev.off() 

######### serie storica
data(sunspot.month)
pdf("images/sun.pdf")
plot(sunspot.month,type="l",main="Macchie solari dal 1749 al 1997")
dev.off()

#generiamo una variabile gaussiana con media 0 e varianza 1 (dev. standard=1)
x<-rnorm(100,0,1)
#costruiamo una variabile gaussiana correlata ad x con variabilità  elevata
y<- 100+x+rnorm(100,0,5)
pdf("images/boxspiega1.pdf")
 boxplot(x, main="", ylim=c(-3,3))
 qq<-quantile(x)
#text(1.5,qq[1],"Q1")
text(1.5,qq[2],"Q1")
arrows(1.45,qq[2],1.2,qq[2])
text(1.5,qq[3],"Q2")
arrows(1.45,qq[3],1.2,qq[3])
text(1.5,qq[4],"Q3")
arrows(1.45,qq[4],1.2,qq[4])
text(1,qq[1]-0.05,"min")
text(1,qq[5]+0.05,"max")
dev.off()
###### boxplot con valori anomali
qqy<-quantile(y)

pdf("images/boxspiega2.pdf")
boxplot(y)
text(1.5,qqy[4],"Q3")
arrows(1.45,qqy[4],1.2,qqy[4])
text(1.5,qqy[2],"Q1")
arrows(1.45,qqy[2],1.2,qqy[2])
text(1.1,qqy[1],"min")
text(1.1,qqy[5],"max")
dev.off()


pdf("images/boxiris1.pdf")
#con l'opzione col posso usare anche degli insiemi di colori predefiniti in R ad esempio rainbow, per vederli digitare ?palette sulla console di R
boxplot(iris[,1:4],col=rainbow(4))
dev.off()
pdf("images/boxiris2.pdf")
par(mfrow=c(1,2)) #con questa istruzione divido la finestra grafica in 1 riga e due colonne, quindi potrà contenere due grafici affiancati
boxplot(iris$Petal.Length~iris$Species,col=c(2,4,3),main="iris Petal Length")
boxplot(iris$Petal.Width~iris$Species,col=c(2,4,3),main="iris Petal Width")
 dev.off()
############ dati qualitativi
data(Titanic)
#latex.table(Titanic[,,2,2],file="vivi",caption="Titanic")
#latex.table(Titanic[,,2,1],file="morti",caption="Titanic")

##### sopravvissuti Adulti sul Titanic per sesso e classe valori assoluti
pdf("images/barplot1.pdf")
barplot(Titanic[,,2,2],beside=T,legend=T,col=rainbow(4))
dev.off()
##### Sopravvissuti Adulti sul Titanic per sesso e classe percentuali rispetto al sesso
xx<-apply(Titanic[,,2,2],2,sum) #calcolo i totali per sesso
yy<-Titanic[,,2,2]
yy[,1]<-100*yy[,1]/xx[1]
yy[,2]<-100*yy[,2]/xx[2]
pdf("images/barplot2.pdf")
barplot(yy,beside=T,legend=T,col=rainbow(4))
dev.off()
##### Morti Adulti sul Titanic per sesso e classe valori assoluti
pdf("images/barplot3.pdf")
barplot(Titanic[,,2,1],beside=T,legend=T,col=rainbow(4))
dev.off()
##### Morti Adulti sul Titanic per sesso e classe percentuali
xx<-apply(Titanic[,,2,1],2,sum) #calcolo i totali per sesso
yy<-Titanic[,,2,1]
yy[,1]<-100*yy[,1]/xx[1]
yy[,2]<-100*yy[,2]/xx[2]
pdf("images/barplot4.pdf")
barplot(yy,beside=T,legend=T,col=rainbow(4))
dev.off()

###### in questo caso ci può interessare di più l'analisi rispetto alla classe e quindi costruire le percentuali per riga
pdf("images/barplot5.pdf")
barplot(t(Titanic[,,2,1]),beside=T,legend=T,col=rainbow(2))
dev.off()

xx<-apply(Titanic[,,2,1],1,sum) #calcolo i totali per classe
yy<-Titanic[,,2,1]
yy[1,]<-100*yy[1,]/xx[1]
yy[2,]<-100*yy[2,]/xx[2]
yy[3,]<-100*yy[1,]/xx[3]
yy[4,]<-100*yy[2,]/xx[4]
pdf("images/barplot6.pdf")
barplot(t(yy),beside=T,legend=T,col=rainbow(2))
dev.off()
####### esercizio costruire i barplot con le percentuali rispetto al totale dei presenti per sesso e poi per classe

######################################
##### visione complessiva della tavola
pdf("images/mosaic1.pdf")
mosaicplot(Titanic, main = "Survival on the Titanic",col=c(2,4))
dev.off()

###########################################
nn01<-function (x)
		{
			#densità gaussiana con media 0 e varianza 1
		cc<-1/sqrt(2*pi)
		y=cc*exp(-(1/(2)*(x)^2))
	return(y)
	}
	pdf("images/norm01.pdf")
plot(dnorm,from=-5,to=5,ylab="density",ylim=c(0,0.45))

pn<-c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)
qn<-qnorm(pn)
dn<-dnorm(qn)
for(i in 1:length(pn)){
segments(qn[i],0,qn[i],dn[i],col=4)
}
abline(h=0)
dev.off()
##### esempio simulato
nn60<-function(x){
	dnorm(x,mean=60,sd=2.5)
	}

weights<-rnorm(100,60,2.5)
pdf("images/unasd.pdf")
hist(weights,nclass=10,prob=T,lty=5)
plot(nn60,from=50,to=70,add=T,col=2,lwd=2)#sovrappongo la curva normale teorica con media 60 e sd=2.5
mc<-mean(weights) #media campionaria
sdc<-sqrt(var(weights)) #sd campionaria
segments(mc-sdc,0,mc-sdc,dnorm(mc-sdc,mean=60,sd=2.5),col=4,lwd=2)
segments(mc+sdc,0,mc+sdc,dnorm(mc+sdc,mean=60,sd=2.5),col=4,lwd=2)
arrows(mc-sdc,dnorm(mc-sdc,60,2.5),mc+sdc,dnorm(mc-sdc,60,2.5),code=3,lwd=2)
text(60,0.10,c("1 sd"),col=4,cex=1.5)
dev.off()

pdf("images/duesd.pdf")
hist(weights,nclass=10,prob=T,lty=5)
plot(nn60,from=50,to=70,add=T,col=2,lwd=2)#sovrappongo la curva normale teorica con media 60 e sd=2.5
mc<-mean(weights) #media campionaria
sdc<-sqrt(var(weights)) #sd campionaria
segments(mc-2*sdc,0,mc-2*sdc,dnorm(mc-2*sdc,mean=60,sd=2.5),col=4,lwd=2)
segments(mc+2*sdc,0,mc+2*sdc,dnorm(mc+2*sdc,mean=60,sd=2.5),col=4,lwd=2)
arrows(mc-2*sdc,dnorm(mc-2*sdc,60,2.5),mc+2*sdc,dnorm(mc-2*sdc,60,2.5),code=3,lwd=2)
text(60,0.10,c("2 sd"),col=4,cex=1.5)
dev.off()

############# verifica grafica
pdf("images/qqplot1.pdf")
qqnorm(weights, main="Normal QQplot: weights")
qqline(weights,col=2) #il risultato evidenzia come i dati abbiano code più pesanti della normale teorica
dev.off()
data(precip)
pdf("images/qqplot2.pdf")
qqnorm(precip, main="Normal QQplot: rain per yr.")
qqline(precip,col=2) #il risultato evidenzia come i dati abbiano code più pesanti della normale teorica
dev.off()

####### cosa fare quando i dati non sono normali
xx<-rnorm(100,mean=0,sd=10)
pdf("images/sqrt1.pdf")
qqnorm(xx^2)
qqline(xx^2,col=2)
dev.off()
pdf("images/sqrt2.pdf")
qqnorm(sqrt(xx^2))
qqline(sqrt(xx^2),col=2)
dev.off()


xx<-rlnorm(100,meanlog=5)
pdf("images/log1.pdf")
qqnorm(xx)
qqline(xx,col=2)
dev.off()
pdf("images/log2.pdf")
qqnorm(log(xx))
qqline(log(xx),col=2)
dev.off()

############ variabilità di media e varianza campionarie esempio
#################
n1<-5000
ese1<-data.frame(mean=rep(0,n1),var=rep(0,n1))
ese2<-data.frame(mean=rep(0,n1),var=rep(0,n1))
ese3<-data.frame(mean=rep(0,n1),var=rep(0,n1))

campioni<-function(n,m=60,sd=2.5){
	x<-rnorm(n,mean=m,sd=sd)
	mm<-mean(x)
	ss<-sqrt(var(x))
	return(c(mm,ss))
	}
	for(i in 1:n1){
		ese1[i,]<-campioni(100)
		ese2[i,]<-campioni(10)
		ese3[i,]<-campioni(50)
		
		}
#stimo le densità - distribuzioni di frequenza dei vari esempi

ym1<-density(ese1[,1])
ym2<-density(ese2[,1])
ym3<-density(ese3[,1])
ys1<-density(ese1[,2])
ys2<-density(ese2[,2])
ys3<-density(ese3[,2])
#definisco i limiti degli assi per disegnare le densità
xml<-range(c(ese1[,1],ese2[,1],ese3[,1]))	
xsl<-range(c(ese1[,2],ese2[,2],ese3[,2]))	
yml<-range(ym1$y,ym2$y,ym3$y)
ysl<-range(ys1$y,ys2$y,ys3$y)

		pdf("images/mediacamp1.pdf")
				plot(ym1,xlim=xml,ylim=yml,xlab="sample mean",ylab="density",main="5000 campioni, media vera 60")
		abline(v=60,col=2)
		lines(ym2,col=4)
			lines(ym3,col=3)
			legend(61,max(yml), c("n=10","n=50","n=100"), lty=1,col=c(4,3,1))
		dev.off()
		pdf("images/sdcamp1.pdf")
	plot(ys1,xlim=xsl,ylim=ysl,xlab="sample sd ",ylab="density",main="5000 campioni, sd vera 2.5")
	
abline(v=2.5, col=2)
lines(ys2,col=4)
			lines(ys3,col=3)
		legend(4,max(ysl), c("n=10","n=50","n=100"), lty=1,col=c(4,3,1))

dev.off()
############# La t di student
dt5<-function(x){
	y=dt(x,5)
	return(y)
	}
	dt10<-function(x){
		y=dt(x,10)
		return(y)
		}
		dt20<-function(x){
		y=dt(x,20)
		return(y)
		}
		dt30<-function(x){
		y=dt(x,30)
		return(y)
		}
			dt100<-function(x){
		y=dt(x,100)
		return(y)
		}
		pdf("images/t.pdf")
	plot(dt5,from=-5,to=5,main="t di student",ylim=c(0,0.44))
	plot(dt10,from=-5,to=5,add=T,col=2)
	plot(dt20,from=-5,to=5,add=T,col=3)
	plot(dt30,from=-5,to=5,add=T,col=4)
	plot(dt100,from=-5,to=5,add=T,col=5)
legend(3,0.4,c("t gdl=5","t gdl=10","t gdl=20","t gdl=30","t gdl=100"),lty=1,col=c(1,2,3,4,5),cex=0.7)
dev.off()
pdf("images/normet.pdf")
plot(dnorm,main="confronto tra t e gaussiana",from=-5,to=5)
plot(dt5,from=-5,to=5,add=T,col=2)
plot(dt100,from=-5,to=5,add=T,col=5)
legend(3,0.4,c("N(0,1)","t gdl=5","t gdl=100"),lty=1,col=c(1,2,5),cex=0.7)
dev.off()

		
	