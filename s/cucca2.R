# Carico i miei dati
pm10 <- read.csv2("pm2010.csv",dec=".")

summary(pm10)
names(pm10)

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

pairs(pm10[,c(1:9)],lower.panel = panel.cor,
      diag.panel = panel.hist,upper.panel = panel.smooth)

library(tidyr)
pm10_2 <- separate(pm10,col="data",into = c("anno","mese","giorno"),sep = "-")

summary(pm10_2)

m <- as.numeric(pm10_2$mese)

pm10_sub <- pm10[,c(1,2,3,11)]

pm10_sub_L <- gather(pm10_sub,key = "data",value = "pm10_conc",
                     factor_key = TRUE)
names(pm10_sub_L) <- c("data","misure","pm10_conc")



library(ggplot2)
ggplot(pm10_sub_L,aes(x=data,y=pm10_conc,color=misure,group=misure)) +
  geom_line()+
  xlab("Data")+
  ylab("Concentrazione di pm 10")+
  ggtitle("Concentrazione giornaliera di pm10 \n rilevata a Taranto nell'anno 2010")+
  scale_colour_manual(name ="Misurazioni",
                      labels = c("Media","Mediana", "Valore Massimo"),
                      values = c("aquamarine","darkred","gold1")) +
  theme_bw()
  

pm10_2[,11] <- as.numeric(pm10_2[,11])

a <- pm10_2[1:273,]
b <- pm10_2[274:365,]
a$mese2 <- substring(a$mese,2,2)
b$mese2 <- b$mese
pm10_3 <- merge(a,b,all = TRUE)


pm10_2$giorno <- factor(pm10_2$giorno)
pm10_2$giorno <- sort(pm10_2$giorno,decreasing = FALSE)

pm10

pm10_2$Stagione <- NULL  











