pm10v<-read.csv2("pm2010.csv",dec=".")
summary(pm10v)

anova(aov(pm10v$media~pm10v$max))

residui=residuals(aov(pm10v$media~pm10v$max)) 
 
stima=fitted(aov(pm10v$media~pm10v$max))

hist(residui)
abline(v=mean(residui),col="blue")

shapiro.test(residui)
qqnorm(scale(residui))
abline(c(0,1),col="red")

plot(stima,residui,pch=20,xlab="fitted",ylab="residuals")
bartlett.test(residui,stima)
str(residui)
str(stima)
