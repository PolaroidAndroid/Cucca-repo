pm10<-read.csv2("data/pm2010.csv",dec=".")


install.packages("tidyverse")

library(tidyr)
pm10_a <- separate(pm10, col="data", into = c("anno","mese","giorno"), sep="-")

pm10_a$mese2 <- as.numeric(pm10_a$mese)

str(pm10_a)        

pm10_a$mese2


pm10_a$giorno2 <- as.numeric(pm10_a$giorno)
str(pm10_a)
# stagioni: primavera (21-03 / 20-06), estate (21-06/22-09), autunno (23-09 /21-12)
# inverno(22-12/20-03)


getSeason <- function(DATES) {
  WS <- as.Date("2010-12-22", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2010-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2010-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2010-9-23",  format = "%Y-%m-%d") # Fall Equinox
  d <- as.Date(strftime(DATES, format="2010-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


k<-getSeason(pm10$data)



pm10_a$Stagione<- k

table(pm10_a$Stagione)


summary(pm10_a)
require(ade4)
pca1<-dudi.pca(deug$tab,scal=FALSE,center=deug$cent,scan=FALSE,nf=2)
pcapm<-dudi.pca(pm10_a[,-10],scal=FALSE,center=TRUE,scan=FALSE,nf=2)
scatter(pcapm$eig)


str(pm10_a)
pm10_a$giorno<-as.factor(pm10_a$giorno)
pm10_a$mese<-as.factor(pm10_a$mese)
pm10_a$anno<-as.factor(pm10_a$anno)


dd1 <- dudi.hillsmith(pm10_a, scann = FALSE)
scatter(dd1)
