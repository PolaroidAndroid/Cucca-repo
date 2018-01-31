######### esempi glm

############ variabili 0/1
dati=read.table("datilogit.csv",header=T,sep=";")
summary(dati)
#### sono dati di presenza assenza simulati sulla base di due variabili env1 ed env2

### costruiamo il modello di regressione logistica per questi dati

mod1=glm(resp~env1+env2,data=dati,family=binomial)
summary(mod1)
## la qualita' del modello vediamo come questo ricostruisce i valori osservati:

previsione=ifelse(predict(mod1,type="response")>=0.5,1,0)
table(dati$resp,previsione)
#### dalla tabella vediamo che il modello sbaglia in 33 casi su 150 ovvero nel 22% dei casi 
### proviamo a togliere l'intercetta

mod1a=glm(resp~env1+env2-1,data=dati,family=binomial)
summary(mod1a)
previsione=ifelse(predict(mod1a,type="response")>=0.5,1,0)
table(dati$resp,previsione)
#### abbiamo migliorato un pochino ora l'errore e' del 20%

