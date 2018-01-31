require(ade4)
#consideriamo il seguente dataset:
data(deug)
#in esso sono contenute 3 tabelle:
#tab contiene i risultati di 104 studenti del secondo anno (università di Lione) in 9 esami (Algebra, Analysis, Proba, Informatic, Economy, Option1, Option2, English, Sport)
#result un vettore di tipo factor che contiene il voto finale dell'anno espresso come A+, A,B, B-, C-, D
#cent un vettore di 9 valori che servono a centrare le variabili per condurre l'analisi in componenti principali (in pratica definisce l'origine del sistema di riferimento)
#### il centrare i dati serve 
# decentred PCA
pca1 <- dudi.pca(deug$tab, scal = FALSE, center = deug$cent, 
    scan = FALSE)
### sguardo d'insieme    
scatter(pca1)
#caratterizziamo gli assi
pca1$c1
### ordiniamo ciascun asse per vedere quali materie hanno punteggi più alti su ciascuno

o1<-order(pca1$c1[,1])
o2<-order(pca1$c1[,2])
pca1$c1[o1,]

### il primo asse ha valori negativi alti per l'esame di probabilità seguito da algebra e valori positivi alti di economia e sport
### evidenzia un comportamento opposto tra i due gruppi di materie 
pca1$c1[o2,]

### il secondo asse ha valori alti positivi di economia, analisi e a pari merito probabilità, algebra e sport seguiti da i due esami opzionali

#### questo tipo di analisi si può condurre anche per via grafica:
s.corcircle(pca1$c1)
#con questa istruzione rappresentiamo i punteggi delle variabili sui due assi principali all'interno di un cerchio unitario, tanto più la freccia che rappresenta la variabile è vicina all'asse tanto più questa variabile è correlata (e quindi caratterizza l'asse) con l'asse fattoriale.

### vediamo come si raggruppano gli studenti sul piano fattoriale rispetto al risultato finale contenuto nella componente deug$result dell'oggetto dei dati

s.class(pca1$li, deug$result)
#pca$li contiene le coordinate delle righe della tabella nel nuovo sistema di riferimento definito dai primi due assi principali.

#aggiungiamo i punteggi delle materie così da vedere la corrispondenza tra queste ed i gruppi
s.arrow(40 * pca1$c1, add.plot = TRUE)

##### osserviamo che la sintassi usata per ottenere la pca equivale a:

xx<-scale(deug$tab,center=deug$cent,scale=F)
### creare un oggetto in cui si riportano i voti dei singoli studenti meno il valore contenuto in deug$cent
pca2<-dudi.pca(xx,center=F,scal=F,scan=F)
### applicare la pca a questi nuovi valori.

############### vediamo ora cosa viene fuori se non indichiamo un centro
pca3<-dudi.pca(deug$tab,center=F,scale=F,nf=2,scan=F)
scatter(pca3)
# dal grafico vediamo subito che tutta l'informazione è schiacciata su di un solo asse
s.corcircle(pca3$c1)
#la caratterizzazione dei due assi non è semplice, possiamo dire che economia caratterizza fortemente il primo asse e probabilità il secondo

#vediamo cosa succede ai gruppi che avevamo trovato
s.class(pca3$li, deug$result)

### sono meno distinti di prima e schiacciati sul primo asse

#### vediamo ora se invece di indicare un centro specifico prendiamo come centro il vettore delle medie
pca4<-dudi.pca(deug$tab,center=T,scale=T,nf=2,scan=F)
scatter(pca4)
#le unità si dispongono attorno all'origine e sono abbastanza separate
s.corcircle(pca4$c1)
##le materie scientifiche si separano abbastanza da quelle umanistiche sui due assi
### vediamo i gruppi
s.class(pca4$li, deug$result)
## i centri dei gruppi sono vicini al primo asse, sono ben separati ma abbiamo perso l'ordinamento che ci derivava dalla prima scelta sono anche meno omogenei 

########## ricerca di gruppi in una matrice ambientale
data(trichometeo)
faulog <- log(trichometeo$fau + 1) # sono conteggi e li esprimiamo su scala log per metterli in relazione alle variabili ambientali
## effettuiamo un'analisi in componenti principali delle variabili meteo per proiettare le informazioni ambientali su di uno spazio di dimensione piccola conservando il massimo dell'informazione
pca1 <- dudi.pca(trichometeo$meteo, scan = FALSE)

##vediamo come si dispongono luoghi e variabili
scatter(pca1)
## e come si caratterizza il primo piano fattoriale
s.corcircle(pca1$c1)

## applichiamo la pca anche alla matrice faunistica
pca2<-dudi.pca(faulog,scan=F)
#### diamo uno sguardo d'insieme ai risultati
scatter(pca2)

### ora mettiamo insieme le due analisi utilizzando il metodo di procruste
#References
#Digby, P. G. N. and Kempton, R. A. (1987) Multivariate Analysis of Ecological Communities. Population and Community Biology Series, Chapman and Hall, London.  Gower, J.C. (1971) Statistical methods of comparing different multivariate analyses of the same data. In Mathematics in the archaeological and historical sciences, Hodson, F.R, Kendall, D.G. & Tautu, P. (Eds.) University Press, Edinburgh, 138–149.  Schönemann, P.H. (1968) On two-sided Procustes problems. Psychometrika, 33, 19–34.  Torre, F. and Chessel, D. (1994) Co-structure de deux tableaux totalement appariés. Revue de Statistique Appliquée, 43, 109–121.  Dray, S., Chessel, D. and Thioulouse, J. (2003) Procustean co-inertia analysis for the linking of multivariate datasets. Ecoscience, 10, 1, 110-119.

proc<-procuste(pca1$tab, pca2$tab, nf = 2)
#### vediamo cosa abbiamo fatto
plot(proc)

#commentiamo insieme i vari grafici...


### in dettaglio
par(mfrow = c(2,2))
s.traject(proc$scor1, clab = 0)
s.label(proc$scor1, clab = 0.8, add.p = TRUE)
s.traject(proc$scor2, clab = 0)
s.label(proc$scor2, clab = 0.8, add.p = TRUE)
s.arrow(proc$load1, clab = 0.75)
s.arrow(proc$load2, clab = 0.75)


