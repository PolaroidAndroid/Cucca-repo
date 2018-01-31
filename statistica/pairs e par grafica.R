# fare grafici con pairs
# con R si possono fare delle funzioni
data("iris")


# la prima funzione disegna gli istogrammi

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# la seconda calcola la correlazione tra le variabili


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


# nel pannello basso metto la correlazione, nel pannello diagonale l'istogramma
pairs(iris[,-5],lower.panel=panel.cor,diag.panel=panel.hist)
# aggiungo smooth ovvero la tendenza dell'istogramma ( linea rossa)

pairs(iris[,-5],lower.panel=panel.cor,diag.panel=panel.hist,upper.panel = panel.smooth)

# per vedere due istogrammi, uso la funzione par, mi permette di fare piu istogrammi
# nella stessa finestra grafica

par(mfrow=c(1,2))

# faccio il primo grafico
plot(iris$Sepal.Length, iris$Petal.Length,pch=20,col=iris$Species)
# poi il secondo e mi appare nella stessa frequenza

plot(iris$Sepal.Width, iris$Petal.Width,pch=20,col=iris$Species)

# comando layout per gestire la finestra grafica


