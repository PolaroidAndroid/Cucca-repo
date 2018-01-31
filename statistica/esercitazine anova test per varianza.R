data(iris)
#mando il test per verificare la varianza
anova(aov(iris$Petal.Length~iris$Species))

#leggendo l'analisi vedo che è molto significativa (valori)
#devo ricordarmi che ho fatto delle assunzioni che riguardano gli errori del 
# del modello che vengono calcolati come residui: mi creo un oggetto che chiamo
#residui, questo oggetto mi contiene una stima del termine di errore



residui=residuals(aov(iris$Petal.Length~iris$Species))

# mi servono anche i valori stimati dal modello, i valori che ottengo sono stimati
# dal modello, vedo se hanno media zero e se sono simmetrici
stima=fitted(aov(iris$Petal.Length~iris$Species))

#faccio un istogramma per capire se sono simmetrici e ne vedo la media con abline

hist(residui)
abline(v=mean(residui), col="red")

#faccio un test shapiro

shapiro.test(residui)
#il test di shapiro è molto sensibile alla numerosita del campione, quindi faccio 
#un qqplot, che mi dice che i residui sono normali.

qqnorm(scale(residui))
abline(c(0,1), col="green")

#diamo l'hp di indipendenza data per vera, se avessi i dati distribuiti nel tempo
# devo andarla a verificare


#vediamo se i residui hanno varianza costante, valori stimati dal modello oriz
# e i residui sul vert.

plot(stima,residui,pch=20,xlab="fitted",ylab="residuals")
# faccio il test per vedere se i residui sono a var costante oppure no, e vedo
# che si c'è un po' di varianza ma non crea conseguenze molto gravi.

bartlett.test(residui,iris$Species)


