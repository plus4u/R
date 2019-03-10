### hierarchical clustering

# 1. 

install.packages("kableExtra") # kable function
install.packages("knitr")


d <- data(package = "knitr")
## names of data sets in the package
d$results[, "Item"]

str(d)

# kable

library(kableExtra)
dt <- mtcars[1:5, 1:6]

head(dt,10)


kable(dt)


dt %>%
    kable() %>%
    kable_styling()

kable(summary(dt))

## 1. 표준화 


pca.scaled <-as.data.frame(pcaFa[2:16])
 
## 2. correlation 

cor(pca.scaled[1:15])


pairs(cor(pca.scaled[1:15]))


## hierarchical methods

wssplot <- function(data, nc=15, seed=1234){ 
    wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
    for (i in 2:nc){ set.seed(seed) 
        wss[i] <- sum(kmeans(data, centers=i)$withinss) 
        } 
    plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
    }

wssplot(pca.scaled[1:15])
 
## 

if (!require(NbClust) ){ 
    install.packages("NbClust") 
    require(NbClust) 
    } 
nc <- NbClust(pca.scaled[1:15], min.nc=2, max.nc=20, method="complete")
str(nc) 

## 

# par(mfrow=c(1,3))  --> # 5 is best 

barplot(table(nc$Best.n[1,]), xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen") 
 

# 그룹핑( 유클리드거리, 완전연결법)

par(mfrow=c(1,1))
cereal.hc <- hclust(dist(pca.scaled[1:15]), "complete")
cereal.hc 
 

#

plot(cereal.hc, hang = -1)


##

clusterCut <- cutree(cereal.hc, k=3) 

clusterCut <- cutree(cereal.hc, k=5)   

table(clusterCut)
 

if (!require(knitr) ){ 
    install.packages("knitr") 
    require(knitr) 
    } 


g1<-clusterCut [clusterCut == 1] 

kable(summary(pcaFa[names(g1), 4:15]))
 

pcaFa[names(g1),]



