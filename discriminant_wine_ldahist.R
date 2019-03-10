## 1. dataset

library(car)
library(help=car)

install.packages('rattle')
data(wine, package='rattle')
 
attach(wine)
head(wine)

# 2. plot

scatterplotMatrix(wine[2:6])

# 3. lda 

library(MASS)

wine.lda <- lda(Type ~ ., data=wine)

wine.lda


# 4. A Stacked Histogram of the LDA Values
attach(wine)
wine.lda.values <- predict(wine.lda)

## 

ldahist(data = wine.lda.values$x[,1], g = wine$Type)  ## 

str(wine.lda.values)  
head(wine.lda.values$x, 10) 

wine$Type

plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],Type,cex=0.7,pos=4,col="red") # add labels



## another method 
 
attach(wine) 
scatterplotMatrix(wine[2:6])


plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],Type,cex=0.7,pos=4,col="red") # add labels

