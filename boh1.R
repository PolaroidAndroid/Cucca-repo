

plot(data_hills$max,data_hills$vv,xlab="massima",ylab="stagione")


hist(data_hills$max)
hist(data_hills$media)
plot(data_hills$max,data_hills$pgg)

gam(max^(1/3) ~ lo(Solar.R) + lo(Wind, Temp), data=airquality, na=na.gam.replace)