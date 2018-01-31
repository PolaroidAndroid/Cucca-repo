
#carico i dati
pm10<-read.csv2("pm2010.csv",dec=".")
# guardo i dati
summary(pm10)
# vedo i nomi delle variabili.
names(pm10)
# faccio il pairs->grafico diviso in 3 zone, superiore inferiore e diagonale,
# le variabili devono essere quantità non qualitative.

pairs(pm10[,c(1:9)])

#questa funzione che inserisco prima del comando mi fa mettere hli istogrammi su la diagonale, che ho trovato sull'help

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

#aggiungo un'altra funzione che mi aggiusta il grafico->vedi help per capire cosa fa

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

#devo usare le due funzioni per capire come leggo l'help
#per andare a capo e leggere bene il comando mando a capo dopo la virgola
#sto facendo un'analisi esplorativa dei dati, pe capire che farci,
#ho fatto un grafico generale di tutti i dati. il valore di correlazione,
#numero es. 0.95 mi da una stima generale del match tra i miei dati
pairs(pm10[,c(1:9)],lower.panel = panel.cor,
      diag.panel = panel.hist,upper.panel = panel.smooth)

#gioco con ggplot, media, mediana e max parlano dello stesso valore(pm10),
#vediamo se cambiano i valori in relazione alla direzione del vento

library(ggplot2)

ggplot(pm10,aes(x=data))+
  geom_point(aes(y=media),color="red")+
  geom_point(aes(y=mediana),color="green")+
  geom_point(aes(y=max),color="blue")
  
ggplot(pm10,aes(x=data))+
  geom_bar(aes(media),color="red")+
  geom_bar(aes(mediana),color="green")+
  geom_bar(aes(max),color="blue")


ggplot(pm10,aes(x=data))+
  geom_col(aes(y=media),color="red")+
  geom_col(aes(y=mediana),color="green")+
  geom_col(aes(y=max),color="blue")


ggplot(pm10,aes(x=data))+
  geom_point(aes(y=media),color="red")

ggplot(pm10,aes(x=data,y=media))+
  geom_line(aes(x=max,y=media),color="green")


ggplot(pm10,aes(x=data))+
  geom_area(aes(x=tmp,y=media))



ggplot(pm10,aes(x=tmp))+
  geom_area(aes(x=pgg,y=media))



ggplot(pm10,aes(x=data))
  geom_line(aes(x=tmp,y=media))
  
  ggplot(pm10,aes(x=data))+
    geom_point(aes(y=media),color="red")+
    geom_point(aes(y=mediana),color="green")+
    geom_point(aes(y=max),color="blue")
  
  
summary(pm10)
#provo a ricreare la variabile per le stagioni
#creo un nuovo dataframe grazie alla library(tidyr) utilizzando la funzione
#separate

pm10_2<-separate(pm10,col="data",into = c("anno","mese","giorno"),sep="-")

#devo mantere la colonna della data perchè R spacchetta la colonna
#se la spacchetto poi non ho più la serie temporale dei dati ovvero nel corso dell'anno
# creo la variabile data (da che ne avevo 11 ora ne ho 14)
pm10_2$data=pm10$data

#creo un sotto database prendendo le colonne che mi interessano
pm10_sub<-pm10[,c(1,2,3,11)]
#utilizzo la funzione gather per passare da short a long format del dataframe

pm10_subL<-gather(pm10_sub,key="data",value="pm10_conc",
                  factor_key = TRUE)
#se voglio con gather posso farlo per tutte le variabili, ma prima mi posso creare
# vettore di  caratteri che li mantenga tutti insieme senza dover scrivere tutto

#rinomino le variabili usando la funzione names (ne avevo due uguali data)

names(pm10_subL)<-c("data","misure","pm10_conc")

#ora che ho aggiustato i dati uso ggplot, relazione tra conc. med-

library(ggplot2)
ggplot(pm10_subL,aes(x=data,y=pm10_conc,color=misure,group=misure))+
         geom_line()

# andare a capo con i titoli \n

ggplot(pm10_subL,aes(x=data,y=pm10_conc,color=misure,group=misure))+
  geom_line()+
  xlab("Data")+
  ylab("Concentrazione di pm10")+
  ggtitle("Concentrazione giornaliera di pm10 \n rilevata a Taranto nell'anno 2010")

#gestisco i colori e la legenda

ggplot(pm10_subL,aes(x=data,y=pm10_conc,color=misure,group=misure))+
  geom_line()+
  xlab("Data")+
  ylab("Concentrazione di pm10")+
  ggtitle("Concentrazione giornaliera di pm10 \n rilevata a Taranto nell'anno 2010")+
  scale_color_manual(name="Misurazioni",
                     labels=c("Media","Mediana","Valore Massimo"),
                     values = c("aquamarine","darkred","gold1"))+
  theme_bw()                     


#Creo le stagioni per i  miei dati

str(pm10_2)

pm10_2[,c(11,12,13)]<-as.numeric(pm10_2)c(11,12,13)
str(pm10_2)


#se avessi lo stesso numero di giorni nei mesi potrei usare questi comandi
rep(seq(1,30,1),12)

#creo una variabile di appoggio per riuscire a capire come convertire il carattere
# perche mi da la data come 01 mentre mi serve come 1 senza gli zeri,e voglio che
# invece di essere considerato un carattere sia un numero
#creo due sotto dataset d'appoggio dividendolo nelle righe dei mesi che devo cambiare
#da 01 a 09 (a) e da 10 a 12 (b)

a<-pm10_2[1:273,]
b<-pm10_2[274:365,]

#gli dico di tenere solo il secondo numero per il dataset a, 
#mentre  b deve rimanere com'è

a$mese2<-substring(a$mese,2,2)
b$mese2<-b$mese

#ri-unisco il dataframe originale

pm10_3<-merge(a,b,all=TRUE)


#ora penso al giorno, posso rendere un giorno un fattore
pm10_3$giorno<-factor(pm10_3$giorno)
pm10_3$giorno<-sort(pm10_3$giorno,decreasing = FALSE)


c<-pm10_3[1:108,]
d<-pm10_3[109:365,]

c$giorno2<-substring(c$giorno,2,2)
d$giorno2<-d$giorno

pm10_3<-merge(c,d,all=TRUE)


str(pm10_3)

pm10_3$giorno2<-as.numeric(pm10_3$giorno2)
pm10_3$mese2<-as.numeric(pm10_3$mese2)



