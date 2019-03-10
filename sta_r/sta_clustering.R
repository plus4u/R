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
## K-means ���� �м��� ����ġ ���� �Ÿ��� �̿��ϱ� ������ ������ ������ ����� ū ������ ��ģ��.
## ������ ǥ��ȭ �ϴ� �۾��� scale �Լ��� ����ؼ� ǥ��ȭ�� ��

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

## ���� �м� ����� training �����ͼ¿� �Ҵ��ϰ�, ����� Ȯ��

training$cluster <- as.factor(iris_kmeans$cluster)
qplot(Petal.Width, Petal.Length, colour = cluster, data = training)

table(training$Species, training$cluster)


##  K-means �����м����� ���� �߽��� ������ �����ϴ� ���
## K-means �����м������� �Է��ϴ� ������ �Բ� �߽��� ������ �����ϴ� ���� �߿�
## ��� ���� �߽��� �������� �����ϴ� ������� ���������� ������, 
## ���� ����ϴ� NbClust ��Ű���� ����ϴ� ����� ���� �� sum of squares�� ����ϴ� ���
 
install.packages("NbClust")
library(NbClust)

nc <- NbClust(training_data, min.nc = 2, max.nc = 15, method = "kmeans")
str(nc)

## Some of square means�� 3���� �Ѿ�鼭 �׷� �� some of squares�� ���� �������� �ʾ� 
## ���� �� ������ �پ��ٴ� ������ iris �����ʹ� 3���� �������� ���� 

wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}

wssplot(training_data)


## training ������ ���� ����ؼ� ���� ���� �����, testing ������ ������ ���� ��Ȯ���� �ٽ� �ѹ� Ȯ��

training_data <- as.data.frame(training_data)

## e1071 package 
install.packages('e1071', dependencies=TRUE)

fit <- train(x = training_data[,-5], y = training$cluster,  method = "rpart")

testing_data <- as.data.frame(scale(testing[-5]))
testing_data_pred <- predict(fit, testing_data) 

table(testing_data_pred ,testing$Species)
