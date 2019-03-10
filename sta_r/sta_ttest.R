### 1.random number  2. t.test

## 1.random number 

# Generate a random number between 5.0 and 7.5

x2 <- runif(10, 5.0, 7.5)

# Generate a random integer between 1 and 10

x3 <- sample(1:10, 1)


runif(1)
#> [1] 0.09006613

# Get a vector of 4 numbers
runif(4)
#> [1] 0.6972299 0.9505426 0.8297167 0.9779939

# Get a vector of 3 numbers from 0 to 100
runif(3, min=0, max=100)
#> [1] 83.702278  3.062253  5.388360

# Get 3 integers from 0 to 100
# Use max=101 because it will never actually equal 101
floor(runif(3, min=0, max=101))
#> [1] 11 67  1

# This will do the same thing
sample(1:100, 3, replace=TRUE)
#> [1]  8 63 64

# To generate integers WITHOUT replacement:
sample(1:100, 3, replace=FALSE)
#> [1] 76 25 52

## 

rnorm(4)
#> [1] -2.3308287 -0.9073857 -0.7638332 -0.2193786

# Use a different mean and standard deviation
rnorm(4, mean=50, sd=10)
#> [1] 59.20927 40.12440 44.58840 41.97056

# To check that the distribution looks right, make a histogram of the numbers
x <- rnorm(400, mean=50, sd=10)
hist(x) 


## 2. t.test

set.seed(123)
x1 <- rnorm(10, mean=0, sd=1)
x2 <- x1+2
 
hist(x2) 

t.test(x1, x2)

## In statistics, Welch's t-test, or unequal variances t-test,

x1 <- rnorm(100, mean=0, sd=1)
x2 <- x1+2

par(mfrow=c(1, 2))

hist(x1) 
hist(x2) 

t.test(x1, x2)

