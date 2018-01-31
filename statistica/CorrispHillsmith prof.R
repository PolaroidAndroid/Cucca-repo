require(ade4)
### analisi delle corrispondenze
data(rpjdl)
?rpjdl
chisq.test(rpjdl$fau)
rpjdl.coa <- dudi.coa(rpjdl$fau, scannf = FALSE, nf = 4)
sum(rpjdl.coa$eig)*rpjdl.coa$N # notare che e' uguale alla statistica chi-quadrato
  par(mfrow = c(1,2))
  s.label(rpjdl.coa$co, clab = 0.6, lab = rpjdl$frlab)
  s.label(rpjdl.coa$li, clab = 0.6)
  par(mfrow = c(1,1))
  
  scatter(rpjdl.coa)
  #### analizziamo insieme la matrice faunistica e la matrice ambientale
  rpjdl.pca<-dudi.pca(rpjdl$mil,scannf=F,nf=4)
  scatter(rpjdl.pca,posieig="bottomright")
  proc<-procuste(rpjdl.pca$tab,rpjdl.coa$tab)
  plot(proc)
  #### analisi di hill-smith
  #### questa analisi applica la pca alla parte numerica della matrice dei dati e l'analisi delle corrispondenze multipla alla parte qualitativa rappresentata da fattori
  data(dunedata)
  ?dunedata
  attributes(dunedata$envir$use)$class <- "factor"   
  dd1 <- dudi.hillsmith(dunedata$envir, scann = FALSE)
  scatter(dd1)
  