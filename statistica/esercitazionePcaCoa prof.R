require(ade4)
data(doubs)
?doubs
dudi1 <- dudi.pca(doubs$env, scale = TRUE, scan = FALSE, nf = 3)
dudi2 <- dudi.coa(doubs$fish,  scan = FALSE, nf = 2)
scatter(dudi1)
scatter(dudi2)

#interpretiamo insieme gli assi della pca
dudi1$c1
#### ordiniamo secondo la prima componente
oo=order(dudi1$c1[,1])
dudi1$c1[oo,]
### ordiniamo secondo la seconda componente
oo=order(dudi1$c1[,2])
dudi1$c1[oo,]
####### variabilita' spiegata
cumsum(dudi1$eig/sum(dudi1$eig))### 3 assi 87%
###### la parte di analisi delle corrispondenze
dudi2$c1### possiamo caratterizzare i siti sulla base delle specie presenti

### disegnamo anche il bacino del fiume
plot(doubs$xy,pch=20,col=4)
####coloriamo i siti secondo la lro altitudine, ovvero dividiamo questa variabili in 3 classi
altcl=cut(doubs$env$alt,breaks=c(0,200,700,1000))
plot(doubs$xy,pch=20,col=altcl)
legend("bottomright",levels(altcl),pch=20,col=c(1,2,3))
###l'altitudine che ruolo ha rispetto alle specie?
s.class(dudi2$li,altcl)### sembra che le abbondanze delle specie siano influenzate dall'altitudine

######### mettiamo insieme piu' situazioni
data(aravo)
?aravo
### analisi delle corrispondenze sulla matrice floristica
coa1 <- dudi.coa(aravo$spe, scannf = FALSE, nf = 2)
#### analisi di hillsmith sulla matrice ambientale
dudienv <- dudi.hillsmith(aravo$env, scannf = FALSE, nf = 2, row.w = coa1$lw)
### pca sui tratti morfologici delle piante 
duditrait <- dudi.pca(aravo$traits, scannf = FALSE, nf = 2, row.w = coa1$cw)
####
?rlq
rlq1 <- rlq(dudienv, coa1, duditrait, scannf = FALSE, nf = 2)
plot(rlq1)
#### oppure guardiamo i risultati separatamente
scatter(coa1,clab.col=0.5,clab.row=0)
scatter(dudienv,clab.row=0)
scatter(duditrait)## qui le specie sono sulle righe 