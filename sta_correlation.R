## correlation analysis


# 1. 

df <- read.csv(file = "http://goo.gl/HKnl74", header = T)

head(df)
str(df) 


# handling NA

colSums(is.na(df))


attach(df)

plot(overall, rides)  # 1. correlation between overall satisfaction and rides 

cov(overall, rides)   # 2. covariance of the both

cor(overall, rides, use = 'complete.obs', method = 'pearson')   # 3. correlation 

plot(df[,4:8])

cor(df[,4:8])  # 4. 

corrplot(cor(df[,4:8]))  # 5. 

#2.  install.packages("corrplot")

library(corrplot)


## compare excel : rank()  sort()  

rank(sort(x))


a1 <- c(4,5,6,5.5,4.5)
a2 <- c(3,7,8,6.5, 7.5)

m = matrix(c(4,5,6,5.5,4.5,3,7,8,6.5, 7.5), nrow = 5)
m

cor(m, method = 'spearman') # refer to excel 


# 3. general test


a1 <- c(1:6)
a2 <- c(2,3,4, 4, 5,5)
d1 <- data.frame(a1,a2)

cor(d1, method = "pearson")  # excel value = 0.960182909

cor.test(a1,a2,conf.level = 0.95, method = "pearson")

