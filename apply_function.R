### apply, rbind, sapply, lapply


x <- cbind(x1 = 3, x2 = c(4:1, 2:5))

dimnames(x)[[1]] <- letters[1:8]
x

## 1. apply

apply(x, 2, mean, trim = .2)

col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)

col.sums

## 2. rbind


rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

y <- c(col.sums, sum(col.sums))

y
# 

x <- 10:20 
y <- 20:25

# Make sure length of x is equal length of y:
stopifnot (length(x) == length(y))

length(x) == length(y)



stopifnot( apply(x, 2, is.vector))

apply(x, 2, is.vector)

## Sort the columns of a matrix

x2 <- apply(x, 2, sort)

x3 <- x

x3$name = c(1:8)

x3



## 3. lapply

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x, mean)
# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)


## 4.tapply 

require(stats)
groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
groups
tapply(groups, groups, length) #- is almost the same as
table(groups)

#

ind <- list(c(1, 2, 2), c("A", "A", "B"))
ind
table(ind)
tapply(1:3, ind) #-> the split vector
tapply(1:3, ind, sum)



# data handling

# vector

v1 <- c(1:5)
v2 <- c(6:10)
v3 <- c(v1,v2)

v4 <- cbind(v1,v2)
v4

# factor

f1 <- factor(c(1:5))
f1

# matrix

m1 <- matrix(1:10, nrow=2)
m1

# array

a1 <- array(1:14, c(2,3,2))

a1

# data.frame

v1 <- rep(1:3,2)

v2 <- rep(c("a", "b", "c"),2)

d1 <- data.frame(v1,v2)
l1 <- list(v1,v2)
l1 

# list

l1 <- list(v1,m1,d3,f1)
l1



## apply 
sapply(v1, mean)
sapply(l1, mean)

sapply(, mean)  # vector return

x <- sapply(v4, mean)
x

x <- lapply(d4, mean) 
x

x <- lapply(iris[, 1:4], mean)  # list

x <- sapply(iris[, 1:4], mean)
x
str(x)

is.data.frame(x) # FALSE
is.vector(x)

as.data.frame(x)

sapply(iris, class)


# for function

iris.num <- NULL

for(x in 1:ncol(iris)){
    if(is.numeric(iris[, x])) iris.num <- cbind(iris.num, iris[, x])
}

str(iris.num)
head(iris.num)
tail(iris.num)

iris.num

iris.num <- data.frame(iris.num)

t1 <- c(1:10)
t2 <- c(11:20)
t3 <- cbind(t1,t2)

t3

t3 <- data.frame(t3)


## sapply function

iris.num <- iris[, sapply(iris, is.numeric)]
sapply(iris.num, class)

head(iris.num)




## apply() row/column  useful apply  lapply, sapply, tapply  

w <- c(65.4, 55, 380, 72.2, 51, NA)

h <- c(170, 155, NA, 173, 161, 166)

g <- c("M", "F","M","M","F","F")

t <- c("2013/09/01", "2013/09/01", "2013/09/05", "2013/09/14", "2013/10/11", "2013/10/26")

x1 <- data.frame( weight = w, height=h, gender=g, testDate=t)



x2 <- x1[ ,c("weight","height")]
 
x2 <- matrix(1:6, nrow = 3)

apply(x2, 1, mean) 
x2 <- edit(x2) # NA
x2
apply(x2, 2, mean) # NA x 
apply(x2, 2, mean, na.rm=T) 
v1 <- c(1:3,NA, 1:3)
v1
apply(v1, FUN = mean, na.rm=T) 

# NA  

apply(t.01.sub, 1, mean, na.rm=TRUE)

# 

apply(t.01.sub, 2, mean, na.rm=TRUE) 

#  

MulTwo <- function(x){ return(2*x) }

apply(t.01.sub, c(1,2), MulTwo)



# lapply( t.01.sub, mean, na.rm = TRUE )

# sapply( t.01.sub, mean, na.rm = TRUE )

# tapply(data, Index, function ) /  


t1$gender # categorical data

x1
tapply(x1$weight, x1$gender, mean, na.rm=TRUE) # mean
 

## apply(X, MARGIN, FUN, …)

iris.num <- iris[1:10, sapply(iris, is.numeric)]

x <- apply(iris.num, 1, mean) # mean by row mean  

is.vector(x) # TRUE

x <- as.data.frame(x)
x
str(x)
head(x)

iris.num <- cbind(iris.num,round(x,1))

iris.num <- iris.num(iris.num$mean="x round(x, 1)")

names(iris.num) <- c("SL", "SW", "PL", "PW", "M1", "M2")

names(iris.num)[6] <- "mean"

iris.num

apply(iris.num, 2, mean)  # mean by column 
 
apply(iris.num, 2, mean, na.rm = T)

### 

apply(iris.num, 2, function(x) { x * 2 + 1 })

apply(iris.num, 2, function(x) { median(x * 2 + 1) })


apply(iris.num, 2, function(x) { median(x * 2 + 1, na.rm = T) })

vapply(iris.num, fivenum, c("Min." = 0, "1st Qu." = 0, "Median" = 0, "3rd Qu." = 0, "Max." = 0))


## R document

require(stats); require(graphics)

x = exp(1)
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))

# compute the list mean for each list element
lapply(x, mean)

# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)

i39 <- sapply(3:9, seq) # list of vectors
sapply(i39, fivenum)
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

## sapply(*, "array") -- artificial example
(v <- structure(10*(5:8), names = LETTERS[1:4]))
f2 <- function(x, y) outer(rep(x, length.out = 3), y)
(a2 <- sapply(v, f2, y = 2*(1:5), simplify = "array"))
a.2 <- vapply(v, f2, outer(1:3, 1:5), y = 2*(1:5))
stopifnot(dim(a2) == c(3,5,4), all.equal(a2, a.2),
          identical(dimnames(a2), list(NULL,NULL,LETTERS[1:4])))

hist(replicate(100, mean(rexp(10))))



 