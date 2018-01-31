
setwd()



altezze <- read.csv2("Altezze.csv",dec=".")
str(altezze)
?str
plot(altezze)
boxplot(altezze,col=2)
??median
??mean
mean(altezze$Altezze)

nrow(altezze)
n_righe<-nrow(altezze)
altezze[51,1]<- 172
altezze[n_righe+1,1]>-172
altezze[51,1]
altezze[52,]<-NA
median(altezze$Altezze,na.rm=TRUE )
boxplot(altezze$Altezze,col="green",range=.1,horizontal=TRUE,log="xy",xlab="altezze cm",main ="altezze")
plot(x=1:nrow(altezze),y=altezze$Altezze,pch=13,col=5)
altezze$sex<- factor("M",levels=c("M","F","Other"))
altezze[3,2]
altezze$sex[3]<-"F"
summary(altezze)
table(altezze$sex)
barplot(table(altezze$sex)/nrow(altezze),ylim=c(0,1))
     
        