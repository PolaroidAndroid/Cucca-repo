pm10<-read.csv2("pm2010.csv",dec=".")


summary(pm10)
plot(pm10$tmp,pm10$media, pch=20)
modp=lm(media~tmp,data=pm10)

summary(modp)

plot(modp)

plot(pm10$media)

plot(pm10$media,pm10$pgg,pch=20)
modr=lm(pgg~media,data=pm10)

summary(modr)

plot(modr)


modt=lm(tmp~rdz,data=pm10)
plot(modt)
summary(modt)
shapiro.test(re(modt))



