### mosaic plot del titanic
data(Titanic)
mosaicplot(~Sex,data=Titanic,color=T)
mosaicplot(~ Sex + Class , data = Titanic, color = c(2,3,4))

mosaicplot(~ Sex + Class+Survived , data = Titanic, color =c(2,4))

