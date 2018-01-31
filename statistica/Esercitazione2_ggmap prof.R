# Installiamo ggmap e i pacchetti necessari
# USIAMO IL SEGUENTE COMANDO SOLO LA PRIMA VOLTA
# POI LO COMMENTIAMO CON IL #
install.packages(c("ggmap","MASS"), dep = TRUE)
# Carichiamo il pacchetto
library(ggmap)
Houston <- get_map()
Houston_sat <- get_map(maptype = "satellite")
Houston_hyb <- get_map(maptype = "hybrid")
####guardo cosa ho scaricato
plot(Houston)
plot(Houston_sat)
plot(Houston_hyb)
### Genero  dati finti (i dettagli saranno chiariti più avanti)
mu <- c(-95.3632715, 29.7632836); nDataSets <- sample(4:10,1)
chkpts <- NULL
### costruisco un CICLO per creare i dati "finti"
for(k in 1:nDataSets){
  a <- rnorm(2)
   b <- rnorm(2) #genero 2 dati da una normale standard
  si <- 1/3000 * (outer(a,a) + outer(b,b))
  chkpts <- rbind(
    chkpts,
    cbind(MASS::mvrnorm(rpois(1,50), jitter(mu, .01), si), k)
  )
}
chkpts <- data.frame(chkpts)
names(chkpts) <- c("lon", "lat","class")
chkpts$class <- factor(chkpts$class)
### salvo l'esempio in modo da poterlo ricaricare a lezione
save.image("ggmap_example.RData")
ggmap(Houston)
ggmap(Houston,extent = normal)

summary(chkpts)
#gioco con ggmap
#creare punti diversi colori

ggmap(Houston, extent="normal")+
  geom_point(aes(x=lon,y= lat, colour=class),
data=chkpts,alpha=.5)+ theme_bw()
#creo curve di livello random sotto

ggmap(Houston, extent="normal")+
  stat_density2d(aes(x=lon,y= lat, colour=class),data=chkpts, alpha = .5)
                 
