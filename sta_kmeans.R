# cluster analysis



# Prepare Data
pcaFa <- na.omit(pcaFa) # listwise deletion of missing
pcaFa <- scale(pcaFa) # standardize variables


# Determine number of clusters
wss <- (nrow(pcaFa)-1)*sum(apply(pcaFa,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(pcaFa, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# K-Means Cluster Analysis
fit <- kmeans(pcaFa, 5) # 5 cluster solution

# get cluster means 
aggregate(pcaFa,by=list(fit$cluster),FUN=mean)

# append cluster assignment
x <- data.frame(pcaFa, fit$cluster)

head(x)
str(x)



