## apply practice

set.seed(123)
df <- data.frame(part=factor(sample(c("p1","p2","p3"), 10, replace=TRUE)), speed=round(runif(10),1))

## sort
str(df)
df [order(df$part), ]


tapply(df$speed, df$part, sum) # x, index 

apply(df$speed, 2, sum(speed))  

# by -> list 
by_sum <- by(df$speed, df$dive, sum)
by_sum               
                 
# 
split_sum <- function(df) {
  s <- split( df, df$dive)
  sapply( s, function(x) { sum(x$speed) } )
}
split_sum

split_sum(df)


##
library(data.table)
setDT(df)[ , .(mean_speed = mean(speed)), by = dive]


##

library(dplyr)
group_by(df, dive) %>% summarize(m = mean(speed))


###

data(beavers)

str(beaver1)

apply(t(beaver1),1,max) 


## lapply

l <- list(a=1:10, b=11:20)  # the mean of the value in each element
l
str(l)


lapply(l, mean)


# sapply(): sapply is wrapper class to
# lapply with difference being it returns vector or matrix instead of list object.

sapply(l, mean) 



## 
tapply(mtcars$mpg, mtcars$cyl,mean)



by():
#  by works similar to group by function in SQL, applied to factors, where in we may apply operations on individual results set. In the below example, we apply 
# colMeans() function to all the observations on iris dataset grouped by Species.

  
  
  
## Compute row and column sums for a matrix:
  
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x

dimnames(x)[[1]] <- letters[1:8]  # rownames 

apply(x, 2, mean, trim = .2)

col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)

rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

stopifnot( apply(x, 2, is.vector))

## Sort the columns of a matrix
apply(x, 2, sort)

## keeping named dimnames
names(dimnames(x)) <- c("row", "col")
x3 <- array(x, dim = c(dim(x),3),
            dimnames = c(dimnames(x), list(C = paste0("cop.",1:3))))
identical(x,  apply( x,  2,  identity))
identical(x3, apply(x3, 2:3, identity))

##- function with extra args:
cave <- function(x, c1, c2) c(mean(x[c1]), mean(x[c2]))
apply(x, 1, cave,  c1 = "x1", c2 = c("x1","x2"))

ma <- matrix(c(1:4, 1, 6:8), nrow = 2)
ma
apply(ma, 1, table)  #--> a list of length 2
apply(ma, 1, stats::quantile) # 5 x n matrix with rownames
