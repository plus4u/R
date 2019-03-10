## 1. lm, predict, 2. multiple regression  3. multinom

# 1. lm : cars

m1 <- lm(dist ~ speed, data = cars)
m1

x <- fitted(m1)[1:10]   # 모형에 의해 예측된 값 

cars


predict(m1, newdata = data.frame(speed = 7))

predict(m1, newdata = data.frame(speed = 1))


residuals(m1)[1:4] # 잔차(residuals)


cars$dist[1:4] - fitted(m1)[1:4]

confint(m1) # 회귀계수에 대한 신뢰구간



deviance(m1) # 잔차제곱합


predict(object, newdata, interval = c("none", "confidence", "prediction")) # 새로운 데이터에 대한 예측값


a <- summary(m1)   # 모형 평가
a

par(mfrow = c(2, 2))  # 모형 진단 그래프
plot(m1)


# 회귀직선의 시각화
plot(cars$speed, cars$dist, xlab = "speed", ylab = "distance")
abline(coef(m1), col = "blue")



# 2. ref : http://www.dodomira.com/2016/01/31/multiple-regression-r/
 
require(ggplot2)
data(swiss)
str(swiss)
summary(swiss)
 
par(mfrow = c(1, 2)) 

hist(swiss$Infant.Mortality)
qqnorm(swiss$Infant.Mortality)
qqline(swiss$Infant.Mortality)

# 
 
model <-lm(Infant.Mortality~. ,data=swiss)

summary(model)
 

#

model_simple <-lm(Infant.Mortality~Fertility ,data=swiss)
summary(model_simple) # A. R review 

anova(model, model_simple)


# predict 

new_Fertility <- rnorm(10, mean=mean(swiss$Fertility), sd=sd(swiss$Fertility)) # good 

new_Fertility <- as.data.frame(new_Fertility)
colnames(new_Fertility) <- c("Fertility")

predict(model_simple, new_Fertility, interval="prediction")






# 3. multinom 
# http://rstudio-pubs-static.s3.amazonaws.com/14459_af8466cb23844215b74c03a471bf8e3c.html 



library(nnet)

m <- multinom(Species ~., data=iris)

m1 <- multinom(formula=Species ~ Sepal.Length+ Sepal.Width + Petal.Length + Petal.Width, data=iris)

summary(m1)

# 
z = summary(m1)$coefficients/summary(m1)$standard.errors
p = (1-pnorm(abs(z), 0, 1))*2
p 

#
head(fitted(m))

x <- fitted(m) ## g 

predict(m) # predict 

predict(m, newdata = iris[c(1, 51, 101), ], type = "class")

predict(m, newdata = iris[c(1, 51, 101), ])

str(m)
head(m)

x1 <- x[c(1, 51, 101), ]  ##
 

x1 <- predict(m, newdata = iris[c(1, 51, 101), ], type = "probs") # 
 
# 

predicted <- predict(m, newdata = iris)

sum(predicted == iris$Species)/NROW(iris)

NROW(iris) #



# extract certain row 
set.seed(123) 
example <- replicate(5, rnorm(n = 10))
example

which(example[, 4] < 0) 

sapply(as.data.frame(example), function(x) which(x < 0)[2]) # 

sapply(as.data.frame(example), function(x) which(x < 0)) 

 
a <- data.frame(A=c(1,2),B=c(3,4),C=c(5,6),D=c(7,7),E=c(8,8),F=c(9,9)) 
a

subset(a, select=c("A", "B")) 

a[2,]



#


x1 <- x ()

apply(fitted(m), 1, max)


a <- apply(fitted(m), 1, max)

str(a)
a

ifelse(a == 1, "setosa", ifelse(a == 2, "versicolor", "virginica"))

predict(m)  ## gg

## gg




#

predict(m, newdata=iris[c(1, 51, 101), ], type="class")

iris[c(1, 51, 101), ] # each of Species 

predict(m, newdata=iris, type="probs")


predicted <- predict(m, newdata=iris)

str(predicted)


sum(predicted == iris$Species) / NROW(predicted)

xtabs(~ predicted + iris$Species)

# 여기서 구한 정확도는 훈련 데이터에 대해서 직접 계산한 것이므로, 
# 새로운 데이터에 대한 예측 성능으로 활용할 수는 없음을 기억하기 바란다.