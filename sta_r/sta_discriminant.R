### 주요한 library 
# https://statkclee.github.io/data-science/data-science-library.html




### 판별분석

# ## 데이터 불러오기 및 확인
library(magrittr)
iris %>% head()

str(iris)

## 결측치 확인
colSums(is.na(iris))


# ## train-test split
# install.packages("caTools")

library(caTools)

# install.packages("tidyverse")
library(tidyverse)

set.seed(123)
sam <- sample.split(iris$Species, SplitRatio = 0.7)
sam %>% head(15)


### error 

train <- subset(iris, split == T)
test <- subset(iris, split == F)
test.y <- test[,5]

# ## lda modeling
library(MASS)
iris_lda <- lda(Species~., data = train, prior = c(1/3,1/3,1/3))
iris_lda

## 만들어진 lda 함수를 가지고 predict
testpred <- predict(iris_lda, test)
testpred

## misclass error 확인
misclass.error <- mean(test.y != testpred$class)
misclass.error

## cross-table
library(gmodels)
CrossTable(x=test.y, y=testpred$class, prop.chisq = F)

## Discriminant modeling: QDA (Quadratic DA)

## 분산-공분산 행렬이 동일한지 체크

library(biotools)
boxM(iris[1:4], iris$Species)  

## 분산-공분산 행렬이 동일한 경우, LDA
## 분산-공분산 행렬이 동일하지 않은 경우, QDA -> 이 경우 p < .05 이므로 이 케이스임

## qda modeling
iris.qda <- qda(Species ~ ., data=train, prior = c(1/3,1/3,1/3))
iris.qda

## 만들어진 qda 함수를 가지고 predict 
testpredq <- predict(iris.qda, test) 
testpredq

## misclass error 확인
misclass.error2 <- mean(test.y != testpredq$class)
misclass.error2

## cross-table
library(gmodels)
CrossTable(x=test.y, y=testpredq$class, prop.chisq = F)



