## key packages

# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("webshot")


x <- 1:10
quantile(x)


installed.packages()
data()
search()

## confine search to .Library for speed
str(ip <- installed.packages(.Library, priority = "high"))
ip[, c(1,3:5)]
plic <- installed.packages(.Library, priority = "high", fields = "License")
## what licenses are there:
table( plic[, "License"] )

data("mtcars")
?mtcars
str(mtcars)

getwd() # c:/r

setwd("c:/r/work_s")

install.packages() #

data() # 

package-data-list <- data(package=.packages(all.available = TRUE))  #

data("mtcars")

ls()

alldata(package = NULL, lib.loc = NULL, all = TRUE, drop.defaults = FALSE) # package dataset.load // list all dataset


boxplot(result~breed+feed,col="blue")

tapply(x1$result, x1$feed, mean) # factor    function

table(mtcars$cyl)

tapply(mtcars$mpg, mtcars$cyl, mean)

## pca, fa 

x1 <- read_excel("./work_r/source/..xls", col_name=TRUE)

head(x1)

eigen(x1) # Error in eigen(x1) : non-square matrix in 'eigen' cor relation  

cor_x1 <- cor(x1)

d1 <- data.frame(cor_x1)

str(d1)
head(d1)

eigen(cor_x1)

principal(d1, 5, rotate = "varimax", scores = TRUE, method = "regression")  # default  eigen value 1    pc1~pc5

x2 <- principal(d1, 5, rotate = "varimax" , scores = TRUE, method = "regression")

x2

##   quick-r review ##

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 

f1 <- factanal(d1, 3, rotation="varimax")
print(f1, digits=2, cutoff=.3, sort=TRUE)

# plot factor 1 by factor 2 

load <- f1$loadings[,1:2] 
plot(load,type="n") # set up plot 

text(load,labels=names(mydata),cex=.7) # add variable names 


x <- c(1:10)
y <- c(rnorm(10,20,3))
plot(x,y)

x1 <- read.csv("MyData.csv")
head(x1)

 
##
 
 ls()

 data()
 
 summary(iris)

search()

head(iris)


## confine search to .Library for speed
str(ip <- installed.packages(.Library, priority = "high"))
ip[, c(1,3:5)]

plic <- installed.packages(.Library, priority = "high", fields = "License")
## what licenses are there:
table( plic[, "License"] )

installed.packages()

str(ip)

##  
# library   data(diamonds) , require(diamonds)  

library(ggplot2)

table(diamonds$cut)
table(diamonds[,"cut"])

prop.table(table(diamonds$color))*100

round(prop.table(table(diamonds$color))*100, digits = 1)

## library(prettyR) :  

barplot(table(diamonds$cut), col="purple", main=" kim", ylab=" ", ylim=c(0, 25000))

barplot(table(diamonds$cut), col="purple", main=" lee", xlab=" ", xlim=c(0, 25000), horiz=TRUE) # 

library(plotly)

## plot

plot(density(b))
lines(density(a), lty=2)


## data set handling

a <- c(1,2,3)
f <- c("a","b","a")
y <- c(80,90,100)
d1 <- data.frame(a,f,y)

d1$n1 = 1 

d1

rm(d1)

ls()

rm(list=ls())

d1$n1

ls()

# If .txt tab file, use this
my_data <- read.delim(file.choose())

# Or, if .csv file, use this
my_data <- read.csv(file.choose())

# display a random sample of our data using the function sample_n()[in dplyr package]
# install.packages("dplyr")

set.seed(1234)
dplyr::sample_n(iris, 10)


## attach / detach , require 


# require() is the same function of library but, it returns logical value (T,F) so is't useful in other function


# The database is attached to the R search path. This means that the database is searched by R when evaluating a variable, so objects in the database can be accessed by simply giving their names.

library(utils)

ls()

utils

head(utils)


attach(utils)
str(utils)

## list up dataset

d <- data(package = "plyr")

library()

.libPaths() # get library location 
library()   # see all packages installed 
search()    # see packages currently loaded



## matrix

m1 <- matrix(c(1:12), nrow=3, ncol=4, byrow = T)
m1

## array

x <- array(1:12, dim=c(2, 2, 3))

x2 <- array(1:12, dim=c(3, 4))

x2[-1]


