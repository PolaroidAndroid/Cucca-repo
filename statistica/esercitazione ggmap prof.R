install.packages(c("ggmap","MASS"), dep = TRUE)
library(ggmap)
Huston<-get_map()
Houston_sat <- get_map(maptype = "satellite")
Houston_hyb <- get_map(maptype = "hybrid")


plot(Houston)
plot(Houston_sat)
plot(Houston_hyb)

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
save.image("ggmap_example.RData")
getwd()

load("ggmap_example.RData")


ls()
plot(Houston_sat)
plot(Houston_hyb)

str(Houston)
ggmap(Houston,extent = normal)
