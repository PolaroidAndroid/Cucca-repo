# modelli di regressione lineare


data(iris)
# per vedere come si comportano i dati iris li vediamo come funzione pairs
# vediamo che relazione c'è tra lunghezza dei petali con i sepali, la regressione
# tra y dipendente e x indipendente per scambiarle devo ristimare il modello
pairs(iris)

# faccio il plot per vedere come si comportano,mi aspetto una correlazione di tipo
#positivo, so che ho una valenza per la specie
plot(iris$Sepal.Length,iris$Petal.Length,pch=20)
# faccio il modello lineare, uso la tilde che significa relazione tra oggetti

mod1=lm(Petal.Length~Sepal.Length,data=iris)
# se applico il summary all' output di un modello mi fa il summary del modello

summary(mod1)
# la tabella mi da diverse informazioni, che in soldoni mi dice quanto è buono il 
# modello, nel senso vicino ai dati, R-squeredro tra 0-1 più è vicino ad 1 piu la
# regeressione è vicina ai dati
# plot(mod1) mi da diversi grafici che posso vedere mandando invio
plot(mod1)

# qui ho perso lo screen della prof XD
-7.101445+1.85843*iris$Sepal.Length[1
#il secondo grafico ci fa vedere i residui che sono normali, 
# sembrano normali , faccio shapiro
shapiro.test(residuals(mod1))
# shapiro che sbaglia con grandi numeri ha cmq p-value= 0.83, quindi sono normali





