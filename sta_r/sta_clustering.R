### clustering 
## a : hierarchical clustering algorithm 

clu <- hclust(dist(iris[, 3:4]))
plot(clu)

iris[, 3:4]

# generates the following dendrogram:

clu_cut <- cutree(clu, 3) # cut off the tree at the desired number of clusters using cutree

str(clu_cut)
# 
table(clu_cut, iris$Species)


# plot 

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clu_cut) + 
  scale_color_manual(values = c('black', 'red', 'green'))


summary(clu_cut)



## b : kmeans 
## K-means 군집 분석은 관측치 간의 거리를 이용하기 때문에 변수의 단위가 결과에 큰 영향을 미친다.
## 변수를 표준화 하는 작업에 scale 함수를 사용해서 표준화를 함

## 
install.packages("caret")

library(caret)

set.seed(123)

train <- createDataPartition(y = iris$Species, p = 0.7, list = F)
str(train)

training <- iris[train,]
testing <- iris[-train,]
training

training_data <- scale(training[-5])

head(training_data, 10)
head(training, 10)

summary(training_data)

iris_kmeans <- kmeans(training_data[,-5], centers = 3, iter.max = 10000)
iris_kmeans$centers

## 군집 분석 결과를 training 데이터셋에 할당하고, 결과를 확인

training$cluster <- as.factor(iris_kmeans$cluster)
qplot(Petal.Width, Petal.Length, colour = cluster, data = training)

table(training$Species, training$cluster)


##  K-means 군집분석에서 군집 중심의 갯수를 결정하는 방법
## K-means 군집분석에서는 입력하는 변수와 함께 중심의 갯수를 지정하는 것이 중요
## 몇개의 군집 중심이 적당한지 결정하는 방법에는 여러가지가 있으며, 
## 자주 사용하는 NbClust 패키지를 사용하는 방법과 군집 내 sum of squares를 사용하는 방법
 
install.packages("NbClust")
library(NbClust)

nc <- NbClust(training_data, min.nc = 2, max.nc = 15, method = "kmeans")
str(nc)

## Some of square means를 3개를 넘어가면서 그룹 내 some of squares가 별로 낮아지지 않아 
## 군집 간 격차가 줄어든다는 것으로 iris 데이터는 3개의 군집으로 구분 

wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}

wssplot(training_data)


## training 데이터 셋을 사용해서 예측 모델을 만들고, testing 데이터 셋으로 모델의 정확성을 다시 한번 확인

training_data <- as.data.frame(training_data)

## e1071 package 
install.packages('e1071', dependencies=TRUE)

fit <- train(x = training_data[,-5], y = training$cluster,  method = "rpart")

testing_data <- as.data.frame(scale(testing[-5]))
testing_data_pred <- predict(fit, testing_data) 

table(testing_data_pred ,testing$Species)

