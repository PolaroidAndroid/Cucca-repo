library(ade4)
# comando se ho gia caricato libreria require
require(ade4)

data("tortues")
names(tortues)
# i nomi delle colonne
# creo un nuovo insieme di dati a cui cambio le colonne
pturtles<-tortues
names(pturtles)<-c("lengh","widht","height","sex")
# mi puo interessare distinguere maschi e femmine

sex<-pturtles$sex

sexcol<-ifelse(sex=="M","blue","red")
#creo un data set con solo le misure
measures<-pturtles[,1:3]
# vedo le misure e le differenze nei colori
plot(measures,col=sexcol,pch=19)

# visualizzazione in 3d, rappresento le 3 variabili in un plot in 3d
# riporto le variabili sulla stessa scala definendo i limiti 

lims<-c(min(measures), max(measures))
# facendo il plot con la nuova scala, il plot mi fa vedere tutto schiacciato
# devo neutralizzare la scala perche le vedo tutte in un angolo
# è importante la scala, le medie sono spostate nella scala degli assi
# posso sottrarre la media con le variabili centrandole 


mesasure.c<-scale(measures, center=TRUE, scale=FALSE)
lim<-c(min(measure.c), max(mesasure.c))
# e faccio il plot in 3d

plot3d(measure.c,type="s", col=sexcol,xlim=lims,ylim=lims,zlim=lims)
# neutralizzo la differenza tra variabilità, la relazione tra le misure 
# non ha artefatti grafici, ma è effettiva, il sitema di assi in cui sono 
# non è detto che sia quello che mi interessa di più
# queste 3 misure sono molto correlate, devo estrarre delle componenti che 
# mi interessano di più, posso rappresentarle aggiungendo una ellisse o sfera
# i cui assi descrivono la direzione dei dati, poi aggiungo i pallini che escon

# l'asse principale rappresenta le variabili, quelli minori le covarianze


plot3d(ellipse3d(cor(measure.cr)))


# dudi.pca e scatter, visualizzo il risultato, il grafico si chiama bplot
# si ottiene con la funzione scatter, in alto a snx è una rappresentazione degli
# autovalori della matrice di correlazione measure, l'analisi in componenti princ
# standardizza in automatico le, una sola delle variabili da informazioni 
# l'asse orizzontale è il primo autovettore, l'asse verticale è il secondo
# le frecce rappresentano le vecchie variabili, o vecchie misure
# rappresento la cui lunghezza è proporzionale alla correlazione con le nuove 
# variabili, la lunghezza di queste proiettata
# sull'asse rappresenta la correlazione con questo asse
# l'altezza proiettata sul'asse è correlata sia al primo che al secondo vettore


pca1<-dudi.pca(measures,scann=FALSE,nf=3)
scatter(pca1)

# ora vado a vedere l'oggetto pca1, a me interessa l'oggetto eig
# che sta per autovalori, il primo è grande gli altri sono piccoli
names(pca1)
pca1$eig
# mi costruisco la proporzione percentuale, in realtà l'unico asse 
# fondamentale è il primo che ha 97% dell'informazione, devo capire
# come rappresentarlo

100*pca1$eig/sum(pca1$eig)

# co contiene gli autovettori, i loro coefficienti che sono infatti negativi come
# nel plot, questo asse contiene tutte le grandezze, l'altezza ne contiene poca di
# più ma lievemente
pca1$co
