

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}



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

data("airquality")

pairs(airquality,upper.panel =panel.smooth,lower.panel=panel.cor,diag.panel = panel.hist)
# rendp le variabili mese e giorno come factor

airquality$Month=factor(airquality$Month)
airquality$Day=factor(airquality$Day)

# funzione is.na mi dice se il valore manca, costruisco due variabili logiche
# per capire dove sono gli na e li coordino

wo<-is.na(airquality$Ozone)
ws<-is.na(airquality$Solar.R)
# elimino i dati mancanti, su tutte le colonne, ho creato il nuovo dataset
# ! ? la negazione logica, quello che era false ora ? true, | significa or/oppure
air<-airquality[!(wo|ws),]

summary(air)

# vedo la correlazione 
mr<-cor(air[,1:4])
# diminuisco le cifre decimali
round(mr,3)

boxplot(air$Ozone~air$Month)

attach(air)
boxplot(Ozone~factor(Month),data=air, col=rainbow(5))

boxplot()


# verifico la normalità di Ozone


qqnorm(Ozone)
qqline(Ozone)
#creo nuovo dataframe in cui cambio in scala logaritmica la prima colonna

air.log<-air
air.log$Ozone<-log(air[,1])
pairs(air.log[,1:4],panel=panel.smooth,lower.panel=panel.cor,diag.panel=panel.hist)


# modello di regressione , il punto ? tutte le variabili, sto stimando il 
#modello saturo con tutte le variabili

mod1=lm(Ozone~.,data=air.log)
# facendo il summary vedo il panico di roba
summary(mod1)

# faccio il modello con "step" partendo dall primo saturo, dicendogli 
# con "both" da entrambi le parti, e  mi va a calcolare l'aic 

mod1s=step(mod1,direction="both")
# gli chiedo qual'è, l'AIC del modello 

AIC(mod1s)
AIC(mod1)

# faccio il summary del modello 1s 
summary(mod1s)

# faccio il modello con modifica in scala logaritmica
air.log.scale=air.log
air.log.scale[,1:4]=scale(air.log[,1:4])
mod3=lm(Ozone~Solar.R+Wind+Temp,data=air.log.scale)
summary(mod3)
