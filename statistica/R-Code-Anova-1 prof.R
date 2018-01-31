###### 
## ANOVA (ANalysis Of VAriance)
######

# Domanda 1:   ANOVA quando?
# Risposta 1:  - La variabile risposta y è quantitativa e le variabili esplicative sono TUTTE qualitative
#              - Ricade nella classe dei modelli lineari (di regressione)          


# Esempio:  si considera un campione di 24 sieri di animali ciascuno dei quali è assegnato in modo 
#           randomizzato a 3 differenti diete. Per ciascun campione è stato calcolato un fattore di 
#           coagulazione e si vuole studiare come le diverse diete agiscono su tale fattore di coagulazione.

#library(faraway)
#data(coagulation)
coagulation=read.table("esempiocoag.txt",header=T)
head(coagulation)
dde
# prima di stimare un modello, facciamo un'analisi grafica su ciò che ci interessa.
# Ad esempio si può pensare ad un boxplot condizionato:

plot(coag ~ diet, data=coagulation,main="Coagulation factor and diets")

# Stimiamo ora il modello ANOVA:

mod=lm(coag ~ diet, data=coagulation)
summary(mod)

# Proviamo ad interpretare il significato dei parametri di questo tipo di modello:

# Domanda 2: manca l'effetto della dieta A!!!
# Risposta 2: - in questo tipo di modelli, la variabile esplicativa è di tipo qualitativo. Per poter "stimare"
#               il modello è necessario trasformare la variabile qualitativa in quantitiva come segue.
#               La variabile "diet" (qualitativa) viene sostituita da 3 variabili quantitative (binarie) definite come
#               dietB: è pari ad 1 se il soggetto è sottoposto alla dieta B altrimenti è 0
#               dietC: è pari ad 1 se il soggetto è sottoposto alla dieta C altrimenti è 0
#               dietD: è pari ad 1 se il soggetto è sottoposto alla dieta D altrimenti è 0
#               Ora definire una variabile dietA è superfluo in quanto il valore di dietA è deducibile
#               dai valori di dietB, dietC e dietD. Infatti, se dietB=0, dietC=0 e dietD=0 allora il soggetto
#               è sottoposto alla dieta A. 
#               Questo modo di riscrivere il modello è detto "parametrizzazione corner point".
# Facciamo un esempio:

coagulation[1,]
# nella nuova parameterizzazione avrei dietB=0, dietC=0 e dietD=0
coagulation[5,]
# nella nuova parameterizzazione avrei dietB=1, dietC=0 e dietD=0

# e così via. Questo tipo di riparametrizzazione viene automaticamente fatta da comando lm quando
# si inseriscono variabili qualitative come predittori.
# Per vedere meglio la nuova riparametrizzazione:

model.matrix(mod)

# Domanda 3: come interpretare i coefficienti?

summary(mod)

# INTERCETTA: l'intercetta rappresenta la media del gruppo di riferimento (o corner). 
#             Nel nostro caso possiamo dire che il gruppo di animali che riceve la dieta A
#             ha un fattore medio di coagulazione pari a 61.00.
#             Tale effetto è significativamente diverso da 0.
#             Domanda: perchè l'intercetta è definita come l'effetto della dieta A?


# DietB:     il valore medio di coagulazione del gruppo di animali che riceve la dieta B è 5 volte
#            più elevato rispetto a quello della dieta A. In particolare, il fattore medio di coagulazione
#            per i soggetti che ricevono la dieta B è pari a 61.00+5.00=65.
#            Inoltre tale incremento è significativo, ossia la dieta B comporta un incremento del fattore
#            di coagualzione maggiore rispetto alla dieta A.

# DietC:     il valore medio di coagulazione del gruppo di animali che riceve la dieta C è 7 volte
#            più elevato rispetto a quello della dieta A. In particolare, il fattore medio di coagulazione
#            per i soggetti che ricevono la dieta C è pari a 61.00+7.00=68.
#            Inoltre tale incremento è significativo, ossia la dieta C comporta un incremento del fattore
#            di coagualzione maggiore rispetto alla dieta A.


# DietD:     il valore medio di coagulazione del gruppo di animali che riceve la dieta D è molto molto basso (circa 0).
#            Pertanto il fattore medio di coagulazione per soggetti che ricevono la dieta D è pari a 61.00+0=61.
#            Inoltre tale incremento non è significativo, ossia la dieta D non comporta un incremento del fattore
#            di coagualzione maggiore rispetto alla dieta A (le due diete comportano lo stesso incremento del fattore
#            di coagulazione).

# Per guardare all'effetto di ciascuna terapia (senza fare i conticini visti sopra), basterà
# stimare il modello senza intercetta:

mod.nointercetta=lm(coag ~ diet-1, data=coagulation)
summary(mod.nointercetta)
# levando l'intercetta l'R2 non ha più senso

# ottengo solo la media e le il test non ha più valore perchè dice
#solo se è diversa da zero, 

# Si noti però che in questo caso i test non sono utili in quanto confrontano il valore di ciascun parametro con
# lo 0 (mentre nel precedente modello confrontavano la media del fattore di coagulazione con la dieta A rispetto 
# a tutte le altre diete.)

# Ora nel modello precedente, abbiamo confrontato A rispetto a tutte le altre diete.
# Ma ad esempio possiamo essere interessati a confrontare:
# - le 4 diete sono equivalenti in termini di incremento del fattore di coagulazione?
# - B rispetto a tutte le altre diete
# - C e D producono un incremento maggiore rispetto ad A e B

# Per rispondere alla prima domanda diamo direttamente un'occhiata a

summary(mod)
# e ci sofferimento sull'ultima riga: F-statistic: 13.57 on 3 and 20 DF,  p-value: 4.658e-05
# si c'è un effetto le diete sono diverse 
# Per la seconda domanda, basterà cambiare il corner point del nostro modello:
# per corn point si intende la categoria di riferimento, funzione relevel, ogni categoria
# è un livello del fattore, li riordina dicendogli prendi questo fattore prendi B

coagulation$diet=relevel(coagulation$diet, ref="B")
mod.B=lm(formula = coag ~ diet, data = coagulation,)
summary(mod.B)
# ora l'intercetta è b , c non è significativo nel boxplot c'è un po' di 
# sovrapposizione, mentre D è molto significativo , la F non è cambiata manco la R 
# ma è campiato l'effetto , A e B si somigliano B e D pure


# Per la terza domanda, dobbiamo introdurre il concetto di contrasto: 
# concetto di contrasto dobbia aggiungere dei vettori che ci permettano 
# di dire quali incrementi vogliamo trattare 
#  prendiamo separatamente  AB e BC , voglio mettere un segno più o segno meno,
# per avere gli incrementi delle coppie di categoria
c1=rep(1,nrow(coagulation))
c1[which(coagulation$diet=="C"|coagulation$diet=="D")]= -1

c2=rep(1,nrow(coagulation))
c2[which(coagulation$diet=="A"|coagulation$diet=="B")]= -1

mat.constrat=cbind(c1,c2)

coagulation$A.vs.D <- c(A=1,B=0,C=0,D=-1)[coagulation$diet]
coagulation$A.vs.C <- c(A=1,B=0,C=-1,D=0)[coagulation$diet]
coagulation$AB.vs.CD <- c(A=0.5,B=0.5,C=-0.5,D=-0.5)[coagulation$diet]

contrasti <- lm(coag~A.vs.D+AB.vs.CD+A.vs.C, data=coagulation)
summary(contrasti)
# servono a confrontare

#########
### ANOVA a 2 vie 
#########
# prendiamo due fattori sperimentali

# Si parla di anova a due vie quando vogliamo considerare due fattori (e non più uno solo come
# nell'esempio precedente).
# Consideriamo un nuovo esempio: 48 topi sottoposti a 4 trattamenti (ABCD) e a 3 veleni. Per ciascun topo
# viene rilevato il tempo di sopravvivenza in decine di ore.

#data(rats)
rats=read.table("esempiorats.txt",header=T)
head(rats)

# Analisi grafica:

plot(time ~ treat,data=rats)
plot(time ~ poison,data=rats)
plot(time ~ treat+poison,data=rats)


# guardiamo, sempre da un punto di vista grafico, a qualche forma di interazione
# tra il trattamento e il veleno.

interaction.plot(rats$treat, rats$poison, rats$time)
interaction.plot(rats$poison, rats$treat, rats$time)


# Se non ci fosse interazione tra i due effetti, allora le linee dovrebbero essere
# abbastanza parallele tra di loro. Verifichiamo però formalmente la presenza di interazione
# e proviamo ad interpretarla.

# E' l'interazione significativa?
#l'asterisco significa interazione
mod.int=lm(time ~ treat + poison + treat*poison,data=rats)
anova(mod.int)
# ... in realtà in questo caso i singoli effetti sono significativi ma non la loro interazione
# Proviamo comunque ad interpretarli:

summary(mod.int)


mod.s=lm(time ~ treat + poison ,data=rats)
anova(mod.s)
summary(mod.s)






