####exploring data
library(ade4)
data(tortues)
names(tortues)
#### si tratta di misure su tartarughe rinomiamole in inglese
pturtles <- tortues
names(pturtles) <- c("length", "width", "height", "sex")
### vogliamo distinguere tra maschi e femmine nei grafici
sex <- pturtles$sex
####costruiamo una variabile che assegna blu ai maschi e red alle femmine
sexcol <- ifelse(sex == "M", "blue", "red")
#Estraiamo dal dataset tutte le variabili continue:  the length, the width
#and the height del carapace delle tartarughe. Tutte le variabili sono misurate in centimetri
measures <- pturtles[, 1:3]
plot(measures, col = sexcol, pch = 19)

#### visualizziamo in 3 d
library(rgl)
plot3d(measures, type = "s", col = sexcol)
### possiamo ruotare il plot con il cursore
#I plot precedenti sono ingannevoli perché le variabili hanno scale molto diverse tra loro. Cerchiamo di riportarle tutte su di una stessa scala 

#####
lims <- c(min(measures), max(measures))
plot3d(measures, type = "s", col = sexcol, xlim = lims, ylim = lims,
zlim = lims)

#In questo plot le variabili sono riportate su scale simili ma il risultato è strano, ciò è dovuto alla differenza tra le medie:

sapply(measures, mean)

#A causa di questa differenza vediamo i punti tutti schiacciati verso la base del grafico in 3-D

###### Centriamo le variabili rispetto alla media. In questo ci aiuta la funzione scale 

measures.c <- scale(measures, center = TRUE, scale = FALSE)
lims <- c(min(measures.c), max(measures.c))
plot3d(measures.c, type = "s", col = sexcol, xlim = lims, ylim = lims,zlim = lims)

#### ora il grafico è più leggibile. Però la diversa variabilità delle variabili influenza ancora molto la sua leggibilità, dobbiamo quindi dividere anche per la deviazione standard, questo al fine di ottenere delle variabili standardizzate e ben confrontabili tra loro

measures.cr <- scale(measures)
lims <- c(min(measures.cr), max(measures.cr))
plot3d(measures.cr, type = "s", col = sexcol, xlim = lims, ylim = lims,
zlim = lims)

#La PCA preferibilmente si applica a variabili standardizzate
#### Vediamo ora l'aspetto complessivo del nostro dataset

plot3d(ellipse3d(cor(measures.cr)), col = "grey", alpha = 0.5)
plot3d(measures.cr, type = "s", col = sexcol, xlim = lims, ylim = lims,
zlim = lims, add = TRUE)

#### L'ellisse che vediamo rappresenta la variabilità del dataset, l'asse principale è definito dalle tre varianze (ovvero la varianza di ciascuna variabile) e le covarianze definiscono gli assi minori.
##
#### facciamo ora andare la pca 
pca1 <- dudi.pca(measures, scann = FALSE, nf = 3)

#### visualizziamo il risultato complessivo
scatter(pca1)

### il grafico in alto a sinistra mostra gli autovalori, questi ci dicono quanta parte di variabilità spiega ogni asse, il primo è il più importante
#### Poi vediamo le frecce che rappresentano le singole variabili, queste ci permettono di dire che ruolo gioca ogni variabile nella definizione di un asse. Vedremo meglio nel prossimo esempio questi aspetti 

