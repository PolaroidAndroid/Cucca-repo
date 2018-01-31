x<-1:100
matrice<-matrix(0,100,3)
matrice[,1]<-rnorm(100,0,1)
matrice[,2]<-rnorm(100,10,2)
plot(matrice[,1],matrice[,2],xlab="normale 0,1",ylab="normale 10,2")
plot(matrice[,1],matrice[,2],xlab="normale 0,1",ylab="normale 10,2",pch=20,col="red")
hist(matrice[,1],nclass=10)
hist(matrice[,2],nclass=10)


x1<-rnorm(100,20,1)
x2<-rnorm(100,0,20)
x3<-rnorm(100,2,0.5)
matrice2<-cbind(x1,x2,x3) 
matrice2<-data.frame(matrice2)
colnames(matrice2)
matrice2$nomi<-paste("nome",1:100,sep="")