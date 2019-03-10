##--------------------------------------------
## (1) Agglomerative Hierarchical Clustering 
##    (1-1) Single Linkage, MIN
##--------------------------------------------
 
x <- c(1, 2, 2, 4, 5)
y <- c(1, 1, 4, 3, 4)
 
xy <- data.frame(cbind(x, y))

xy
        
# scatter plot of xy
plot(xy, pch=19, xlab = c("x coordinate"), ylab = c("y coordinate"), xlim = c(0, 6), ylim = c(0, 6), main = "scatter plot of xy")
 
# adding student label

text(xy[,1], xy[,2], labels = abbreviate(rownames(xy)), cex = 0.8, pos = 1, col = "blue") # pos=1 : at the bottom
 

 # adding dotted line
abline(v=c(3), col = "gray", lty = 2) # vertical line
abline(h=c(3), col = "gray", lty = 2) # horizontal line
         

## 1.  proximity matrix : squares of euclidean distance matrix for 6 points
dist(xy, method = "euclidean")^2


## 2. Agglomerative Hierarchical Clustering : Single Linkage method (MIN)
hc_sl <- hclust(dist(xy)^2, method="single")
hc_sl

 
## 3. dendrogram
my_par = par(no.readonly = TRUE)
par(oma = c(0, 0, 1, 0))
par(mfrow = c(1, 2))
plot(hc_sl)
plot(hc_sl, hang = -1) # hang = -1 : line from the bottom

 
## 4. custering information 
rev(hc_sl) 



# number of clusters above distance's threshold
# exmaple 1) height threshold = 1

height_threshold <- 1
distance_over_height <- rev(hc_sl)$height >= height_threshold

  number_of_cluster <- sum(distance_over_height)+1
number_of_cluster 

  
# exmaple 2) height threshold = 3

height_threshold <- 3
distance_over_height <- rev(hc_sl)$height >= height_threshold

  number_of_cluster <- sum(distance_over_height)+1
number_of_cluster 

    # exmaple 3) height threshold = 6
  height_threshold <- 6
distance_over_height <- rev(hc_sl)$height >= height_threshold

  number_of_cluster <- sum(distance_over_height)+1
number_of_cluster
 