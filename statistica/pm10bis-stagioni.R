pm10<-read.csv2("pm2010.csv",dec=".")
summary(pm10)
plot(pm10[,4],pch=1,col="green",ylab="temperature C°", main="grafico temperature")

mean(pm10[,4])
boxplot(pm10[,4],col="red")
barplot(pm10[,4])
hist(pm10[,4]
str(pm10)
?summary
pm10
levels(pm10$data)
library(tidyr)
pm10bis<-separate(pm10, "data", c("Year", "Month", "Day"), sep = "-")

pm10bis$season=pm10bis$Month
str(pm10bis)
pm10bis$Month<-as.numeric(as.character(pm10bis$Month))
summary(pm10bis)

pm10bis$season=pm10bis$Month
pm10bis$season=ifelse(pm10bis$Month>=4|pm10bis$Month<=10,paste("hot"),paste("cold"))
table(pm10bis$season)

pm10bis$data=pm10$data
plot(pm10bis$data,pm10bis$tmp)
W=ifelse(pm10bis$season=="hot","red","blue")

boxplot(pm10bis$tmp~factor(pm10bis$Month))

w
W        
library(ggplot2)
ggplot(pm10bis,aes(x=Month,y=tmp,fill=season))+
  geom_boxplot()
str(pm10bis$season)
str(pm10bis$Month)
summary(pm10bis$Month)
pm10bis<-separate(pm10, "season", c("Year", "Month", "Day"), sep = "-")

season2 <-factor(levels = c("Winter","Spring","Summer","Fall"))
pm10bis$season3=NA
str(pm10bis)
w=c(1,2,12)
s=c(3,4,5)
s2=c(6,7,8)
f=c(9,10,11)



pm10bis$season3[pm10bis$Month==1|pm10bis$Month==2|pm10bis$Month==12]<-"Winter"
pm10bis$season3[pm10bis$Month==3|pm10bis$Month==4|pm10bis$Month==5]<-"Spring"
pm10bis$season3[pm10bis$Month==6|pm10bis$Month==7|pm10bis$Month==8]<-"Summer"
pm10bis$season3[pm10bis$Month==9|pm10bis$Month==10|pm10bis$Month==11]<-"Fall"

