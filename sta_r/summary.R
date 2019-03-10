getwd()
setwd("C:/Users/beomc/working")


### Cluster Analysis  https://www.statmethods.net/advstats/cluster.html

# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables


# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)


# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")


# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)


# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model

# Plotting Cluster Solutions


# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)


# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)


##

### non parametric


## 1. one sample




## 2. paired 

# paired 10 sample of patient's blood sugar
 x1 <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
 x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)
 # wilcox signed rank test
 wilcox.test(x1, x2, 
                +        alternative = c("greater"), 
                +        paired = TRUE, 
                +        conf.level = 0.95)



## 3. 2 sample


# 3.1 independent 2 samples : Wilcoxon rank sum test (È¤Àº Mann-Whitney U-test)

wilcox.test()

# Dataset
library(MASS)
str(Cars93)

##--- way 1 : y ~ Factor
wilcox.test(Price ~ Origin, 
            +             data=Cars93, 
            +             alternative = c("two.sided"), 
            +             mu = 0, 
            +             conf.int = FALSE, 
            +             conf.level = 0.95)




# 3.2 -- way 2. x, y numeric vectors

# x, y numeric vector indexing
Price_USA <- Cars93[which(Cars93$Origin == c("USA")), c("Price")]
Price_nonUSA <- Cars93[which(Cars93$Origin == c("non-USA")), c("Price")]

wilcox.test(Price_USA, Price_nonUSA, 
            +             alternative = c("two.sided"), 
            +             mu = 0, 
            +             conf.int = FALSE, 
            +             conf.level = 0.95)



