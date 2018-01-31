pm10<-read.csv2("pm2010.csv",dec=".")

library(tidyr)
pm10_a<-separate(pm10,col="data",into = c("anno","mese","giorno"),sep="-")

pm10_a$mese2<-as.numeric(pm10_a$mese)

str(pm10_a)        

pm10_a$mese2


pm10_a$giorno2<-as.numeric(pm10_a$giorno)
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



str(pm10v)
pm10v$Stagione<-as.factor(pm10v$Stagione)
names(pm10v)
library(ggplot2)
ggplot(pm10v,aes(x=Stagione,y=tmp,fill=Stagione))+
  geom_boxplot()

ggplot(pm10v,aes(x=day,y=tmp))+
  geom_point(aes(color=Stagione))+
  geom_smooth()

pm10v$day<- seq(1,365,1)


mod1<-lm(rdz~tmp,data=pm10v)
summary(mod1)


ggplot(pm10v,aes(x=tmp,y=rdz))+
  geom_point()+
  geom_smooth(method = "lm")


pairs(pm10v[,1:10])


mod2<-lm(tmp~umr,data=pm10v)
summary(mod2)

par(mfrow=c(2,2))
plot(mod2)



ggplot(pm10v,aes(x=umr,y=tmp))+
  geom_point()+
  geom_smooth(method = "lm")


names(pm10v)


modpm <- lm(media~tmp+umr+log(pgg+1),data=pm10v)
summary(modpm)
plot(modpm)
par(mfrow=c(1,1))
acf(modpm$residuals)

a<-lm(media~log(pgg+1),data = pm10v)

summary(a)
summary(pm10v)
pm10v[246,]
