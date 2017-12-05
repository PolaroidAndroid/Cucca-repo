data(iris)

library(ggplot2)

names(iris)
a<- ggplot(iris,aes(x=Sepal.Length,y=Petal.Length))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Sepal Length")+
  ylab("Petal Length")+
  ggtitle("Regression Line")+
  facet_wrap(~Species,scales = "free")

b<- ggplot(iris,aes(x=Species,y=Petal.Length,fill=Species))+
  geom_boxplot()+
  scale_fill_manual(values = c("red","green","blue"))

library(cowplot)

plot_grid(a,b,labels = c("1)","2)"),nrow=1,ncol=2)





