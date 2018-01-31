data("iris")
boxplot(iris[,4]~iris[,5],col=c(2,3,4))
# dati di R
hist(iris[,4])
hist(iris[,4],col="red",xlab="Lunghezza dei petali",main="Istogramma della lunghezza dei petali",ylab="Frequenze assolute")

plot(iris$Sepal.Length,iris$Petal.Width,xlab="Lunghezza dei petali",ylab="Larghezza dei sepali")
w=iris$Species=="setosa"
points(iris$Sepal.Length[w],iris$Petal.Width[w],col=2,pch=11)
x=iris$Species=="versicolor"
points(iris$Sepal.Length[x],iris$Petal.Width[x],col=3,pch
z=iris$Species=="virginica"
points(iris$Sepal.Length[z],iris$Petal.Width[z],col=4,pch=8)

hist(iris[,w],col=2)

read.csv2()
