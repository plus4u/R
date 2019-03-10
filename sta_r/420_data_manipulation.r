### data manipulation : 1. basic,  2. strsplit
### https://www.statmethods.net/input/importingdata.html

## 1. vector

a <- c(1, 2,3.1, 4,-5, 6.3) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector

a

a[c(2,4)] # 2nd and 4th elements of vector


# generates 5 x 4 numeric matrix 
y<-matrix(1:20, nrow=5,ncol=4)

# another example
cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2") 
mym <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames))

mym

x[,4] # 4th column of matrix
x[3,] # 3rd row of matrix 
x[2:4,1:3] # rows 2,3,4 of columns 1,2,3


# Arrays are similar to matrices but can have more than two dimensions

d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
names(mydata) <- c("ID","Color","Passed") # variable names

# example of a list with 4 components - 
# a string, a numeric vector, a matrix, and a scaler 
w <- list(name="Fred", mynumbers=a, mymatrix=y, age=5.3)

# example of a list containing two lists 
v <- c(list1,list2)

# identify elements of a list using the [[]] convention.

mylist[[2]] # 2nd component of the list
mylist[["mynumbers"]] # component named mynumbers in list

# variable gender with 20 "male" entries and 
# 30 "female" entries 
gender <- c(rep("male",20), rep("female", 30)) 
gender <- factor(gender) 
# stores gender as 20 1s and 30 2s and associates
# 1=female, 2=male internally (alphabetically)
# R now treats gender as a nominal variable 
summary(gender)

# An ordered factor is used to represent an ordinal variable.

# variable rating coded as "large", "medium", "small'
rating <- ordered(rating)
# recodes rating to 1,2,3 and associates
# 1=large, 2=medium, 3=small internally
# R now treats rating as ordinal

# Useful Functions
length(object) # number of elements or components
str(object)    # structure of an object 
class(object)  # class or type of an object
names(object)  # names

c(object,object,...)       # combine objects into a vector
cbind(object, object, ...) # combine objects as columns
rbind(object, object, ...) # combine objects as rows 

object     # prints the object

ls()       # list current objects
rm(object) # delete an object

newobject <- edit(object) # edit copy and save as newobject 
fix(object)               # edit in place


## 2.strsplit°ú stringr 

path <- getwd()

strsplit(path, "/")

fruits <- c(
  "apples and oranges and pears and bananas",
  "pineapples and mangos and guavas"
)

strsplit(fruits," and ")
str_split(fruits, " and ")
str_split(fruits, " and ", n = 2)
str_split(fruits, " and ", n = 5)

kk<-"k.love"
strsplit(kk,fixed=TRUE,split=".")



## time

timeDate <- as.POSIXct("2015-10-19 10:15")   
str(timeDate)

timeDate
unclass(timeDate)

timeDatelt<- as.POSIXlt("2019-01-19 10:12")  
str(timeDatelt)

timeDatelt

unclass(timeDatelt)


## 

# purely random process with mean 0 and standard deviation 1.5
eps <- rnorm(100, mean = 0, sd = 1)
mu <- 2 # the constant mean
# The process
X_t <- mu + eps

# plotting the time series
ts.plot(X_t, main = "Example of (random) stationary time series", ylab = expression(X[t]))

plot(X_t, main = "Example of (random) stationary time series", ylab = expression(X[t]))


## 

# seed X_0 = 0
X <- 0

# purely random process with mean 0 and standard deviation 1.5
Z <- rnorm(100, mean = 0.5, sd = 1.5)

# the process
for (i in 2:length(Z)){
  X[i] <- X[i-1] + Z[i]
}

# process plotting
ts.plot(X, main = "Random walk process")


# differencing and plotting of the random walk process
ts.plot(diff(X))


##

# purely random process with mean 0 and standard deviation 1.5 (arbitrary choice)
Z <- rnorm(100, mean = 0, sd = 1.5)

# process simulation
X <- c()
for (i in 2:length(Z)) {
  X[i] <- Z[i] - 0.45*Z[i-1]
}
X

# process plotting
ts.plot(X, main = "Moving Average or order 1 process")

### time difference

x1 <- as.POSIXlt("2019-1-1 00:00:00")

x2 <- as.POSIXlt("2019-1-1 10:11:12")

x1 - x2
x2 - x1
x1
class(x1)

x = 11/60 + 12/(60*60)
x
