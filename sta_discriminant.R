## discriminant 

cname <- c("ID", "구매브랜드", "연령","세대연수입", "세대사람수", "방문빈도", "거주년수")
x = read.table("c:/R/work_r/disc.txt", col.names = cname)
head(x)

x1 <- subset(x, 구매브랜드=="A")
x2 <- subset(x, 구매브랜드=="B")

# 

library(MASS)

rs<-lda(구매브랜드~연령+세대연수입+세대사람수+방문빈도+거주년수, data=x)
rs

# 
p <- predict(rs)
str(p)

x <- cbind(x, p$x)
x

# result 
# Coefficients of linear discriminants:
#    LD1
#연령       -0.0982250865
#세대연수입 -0.0008926856
#세대사람수 -0.7287341389
#방문빈도   -0.6346846738
#거주년수   -0.0578973013

# 

c1 <- with(x, 연령 * -0.0982250865 + 세대연수입 * -0.0008926856 + 세대사람수 * -0.7287341389 + 방문빈도 * -0.6346846738 + 거주년수 * -0.0578973013)
x <- cbind(x, c1)

c2 <- with(x, LD1 - c1)
x <- cbind(x, c2)
x

#

x1 <- with(data=x, x$구매브랜드, x$c2)
x1

x2 <- subset(x, select=c("구매브랜드", "c2"))
x2

table(x2)

a <- letters[1:3]
table(a, sample(a))



## iris

x <- iris

library(MASS) 
rs <- lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=x)
rs


# 

p <- predict(rs)

str(p)
x <- cbind(x, p$x)
x[5:7]

plot(rs)
plot(rs, dimen=1, type="both") 

#
mean(x$LD1[x$Species == "setosa"])
mean(x$LD1[x$Species == "versicolor"])
mean(x$LD1[x$Species == "virginica"])




### 

# 1. call data, scatter 

data(iris)

library(psych)

pairs.panels(iris[1:4],
             gap=0,
             bg=c("red", "green","blue","yellow")[iris$Species],
             pch = 21)

# 2. data partition

set.seed(222)

i <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))

## sample 
x <- 1:12
y <- rep(c("a","b","c"), 4)
xy <- data.frame(x,y)
xy

x1 <- sample(2, nrow(xy), replace = TRUE, prob = c(0.7, 0.3))

x1
## 



tr <- iris[i == 1,]
te <- iris[ i == 2,]

head(tr)
head(i)
head(te)

head(iris)

# 3. linear 

library(MASS)

lda(Species ~ . , tr)



## plot color 1
str(iris)

idx <- iris$Species == "setosa"

plot(iris$Sepal.Length, iris$Sepal.Width, xlab = "Sepal.length", ylab = "width")

points(iris$Sepal.Length[idx], iris$Sepal.Width[idx], pch=19, cex=1.5)
points(iris$Sepal.Length[!idx], iris$Sepal.Width[!idx], pch=21, cex=1.5)


## plot color 2

head(iris)

plot(iris[ , c(1,2)], col=iris[ ,2])

x <- sample(1:nrow(iris), 10, replace=FALSE)
x <- sample(iris[c(1,2)], replace=FALSE)

x <- iris[sample(1:nrow(iris), 10, replace=FALSE), ] ## 

plot( x[, c(1,2)], col = x[,1], pch=21, cex=2)
text( x[, c(1,2)], cex=1)

plot( x[, c(1,2)], col = x[,5], pch=21, cex=2)
text( x$Sepal.Length, x$Sepal.Width, row.names(x), cex = 1, pos=4, col="red")

# Example of labeling points
attach(mtcars)
plot(wt, mpg, main="Milage vs. Car Weight", 
     xlab="Weight", ylab="Mileage", pch=18, col="blue")
text(wt, mpg, row.names(mtcars), cex=0.6, pos=4, col="red")




