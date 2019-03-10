## 7. discriminant : lda, qda 

colSums(is.na(iris))

## 데이터 불러오기 및 확인
iris %>% head()
str(iris)

## 결측치 확인
colSums(is.na(iris))


## train-test split

library(caTools)

set.seed(1710)
split <- sample.split(iris$Species, SplitRatio = 0.7)
split %>% head(15)

train <- subset(iris, split == T)
test <- subset(iris, split == F)
test_y <- test[,5]



## lda modeling
library(MASS)

iris_lda <- lda(Species~., data = train, prior = c(1/3,1/3,1/3))
iris_lda

## 만들어진 lda 함수를 가지고 predict
testpred <- predict(iris_lda, test)
testpred

## misclass error 확인
misclass_error <- mean(test_y != testpred$class)
misclass_error

## cross-table
library(gmodels)
CrossTable(x=test.y, y=testpred$class, prop.chisq = F)


## 분산-공분산 행렬이 동일한지 체크
library(biotools)
boxM(iris[1:4], iris$Species)  

## 분산-공분산 행렬이 동일한 경우, LDA
## 분산-공분산 행렬이 동일하지 않은 경우, QDA -> 이 경우 p < .05 이므로 이 케이스임

## qda modeling
iris_qda <- qda(Species ~ ., data=train, prior = c(1/3,1/3,1/3))
iris_qda


## 만들어진 qda 함수를 가지고 predict 
testpredq <- predict(iris_qda, test) 
testpredq

## misclass error 확인
misclass_error2 <- mean(test_y != testpredq$class)
misclass_error2

## cross-table
library(gmodels)
CrossTable(x=test_y, y=testpredq$class, prop.chisq = F)

