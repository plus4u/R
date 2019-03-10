## clustering 

library(caret)
set.seed(1712)

# 1. train, test dataset

inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
training

testing

# 2. standization

training.data <- scale(training[-5])
summary(training.data)


# 3. 3 clusters

iris.kmeans <- kmeans(training.data[,-5], centers = 3, iter.max = 10000)
iris.kmeans$centers 

# 4. plot result

training$cluster <- as.factor(iris.kmeans$cluster)
qplot(Petal.Width, Petal.Length, colour = cluster, data = training)


table(training$Species, training$cluster)



# 5. find number of center

#install.packages("NbClust")
library(NbClust)

nc <- NbClust(training.data, min.nc = 2, max.nc = 15, method = "kmeans")

## *** : The Hubert index is a graphical method of determining the number of clusters.
##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
##                 significant increase of the value of the measure i.e the significant peak in Hubert
##                 index second differences plot. 
## 


## *** : The D index is a graphical method of determining the number of clusters. 
##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
##                 second differences plot) that corresponds to a significant increase of the value of
##                 the measure. 
##  
## ******************************************************************* 
## * Among all indices:                                                
## * 8 proposed 2 as the best number of clusters 
## * 9 proposed 3 as the best number of clusters 
## * 2 proposed 5 as the best number of clusters 
## * 1 proposed 8 as the best number of clusters 
## * 3 proposed 14 as the best number of clusters 
## * 1 proposed 15 as the best number of clusters 
## 
##                    ***** Conclusion *****                            
##  
## * According to the majority rule, the best number of clusters is  3 
##  
##  
## *******************************************************************


# 6. Some of square means 

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")



wssplot <- function(data, nc = 15, seed = 1234) {
    wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
    for (i in 2:nc) {
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers=i)$withinss)}
    plot(1:nc, wss, type="b", xlab = "Number of Clusters",
         ylab = "Within groups sum of squares")}

wssplot(training.data)


# 7. test dataset 

training.data <- as.data.frame(training.data)
modFit <- train(x = training.data[,-5], 
                y = training$cluster,
                method = "rpart")

testing.data <- as.data.frame(scale(testing[-5]))
testClusterPred <- predict(modFit, testing.data) 
table(testClusterPred ,testing$Species)






