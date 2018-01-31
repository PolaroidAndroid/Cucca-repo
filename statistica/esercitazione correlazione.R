data(iris)

summary(iris)

#applico una funzione per standardizzare la y, voglio che la mia media abbia
# sottrae la media e divide per la deviazione standard, per confrontarla con la
#normale standard, in maniera da poterle confrontare con la stessa scala

y=scale(iris$Petal.Length)
mean(y)
var(y)

#abline aggiunge una retta di cui do intercetta e coeff angolare
#per la bisettrice i valori sono 0 e 1 

qqnorm(y)
abline(c(0,1))

#vedendo il qqplot capisco che non sono simmentrici per un cazzo,faccio il test
#di shapiro wings


shapiro.test(y)
#che mi conferma l'hp che non è normale come diceva il qqplot, con tanti dati
#ancora di più sfattona 


#quanto possono essere correlate tra loro le grandezze che ho preso?
#queste sono lunghezze ovvero variabili reali, posso usare il coeff di
#correlazione di person, che misura la correlazione tra due variabili
# ho quattro variabili, che posso correlare tra coppie
#ottenendo una tabella della 4 
# ho levato l'ultima colonna che riguarda la specie [,-5]

cor(iris[,-5])

#vedo la correlazione tra i vari caratteri, la correlazione mi fa capire
#dove ne ho molta che alcune variabili sono proporzionali, es petali lunghezza
#e larghezza, utilizzare variabili molto correlate può portare a distorsione dei
# dati, uso la funzione round, in cui arrotondo e metto le cifre decimali che 
#voglio

round(cor(iris[,-5]),2)


#ho bisogno di due colonne per il cor.test, è meglio farlo per essere sicuri con
# la correlazione, l'intervallo di confidenza che passa per 0 mi deve mettere
# in allarme


round(cor(iris[,-1],iris [,3])


cor.test(iris[,1],iris[,3])

#se ci sono dati mancanti e via dicendo devo dirlo a cor, c'ee pearson su variabili
#di tipo quantitativo, quando ha senso calcolare una distanza euclidea
#ma su variabili discrete non ha senso
# utilizzo le altre variabili, kendall e spearman, che misurano in senso
#di cograduazione, li mettono in ordine e verifico se le due variabili cograduano
# ottengo l'equivalente di un coeff di correlazione di  per variabili che non sono reali

# il coeff di spearman è meno sensibile agli errori di campionamento




cor(iris[,-5],method = "spearman")


# spearman più robusto di pearson, 

setosa=iris[iris$Species=="setosa",]
versicolor=iris[iris$Species=="versicolor",]
virginica=iris[iris$Species=="virgi"
               
               
               
summary(setosa)

a=scale(setosa$Sepal.Length)
mean(a)


qqnorm(a)
abline(0,1)

shapiro.test(a)


cor(setosa[,-5])
round(cor(setosa[,-5]),2)
cor.test(setosa[,1],setosa[,2])

library(ggplot2)

ggplot(setosa)
ggplot(setosa,aes(x=setosa$Sepal.Length))+
  geom_point(aes(y=setosa$Sepal.Width),color="red")+
  geom_point(aes(y=setosa$Petal.Length),color="green")+
  geom_point(aes(y=setosa$Petal.Width),color="blue")


ggplot(setosa,aes(x=setosa$Sepal.Length))+
  geom_line(aes(y=setosa$Sepal.Width),color="red")+
  geom_line(aes(y=setosa$Petal.Length),color="green")+
  geom_line(aes(y=setosa$Petal.Width),color="blue")


  

             