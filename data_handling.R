## data handling

rm(list = ls())

# vector 
x1 <- c(1,2,3)
x <- 1:9
x
x1 <- c(1: 9)
n1 <- rep(10,3)
n1 <- rep(1:3, 3)
n1
x1.n1 <- data.frame(x1, n1)
x1.n1



d1 <- data.frame(A=c(1,2),B=c(3,4),C=c(5,6),D=c(7,7),E=c(8,8),F=c(9,9)) 
d1
subset(d1, select=c("A", "B"))
 


# Generate a random number : ?runif / ?sample / ?.Random.seed

x1 <- runif(10, 5.0, 7.5)

x3 <- sample(1:10, 1)

x4 <- sample(1:10, 5, replace=T)

sample(state.name, 10)   # generate a random subset of any vector


# find dataset, package, variables : get a listing of pre-loaded data sets

data()

# Gives a list of attached packages (see library), and R objects, usually data.frames.

searchpaths()

attach()  # detach()
search()

attach(iris)
head(Sepal.Width)

with( )   # within( )

ls() # all objects
rm(list = ls())  # delete all

search()
data(package="psy")


# Existing local data
mydata <- read.csv("filename.txt")

mydata <- read.table("filename.txt", sep="\t", header=TRUE)

# function

rnorm_fixed = function(n, mu=0, sigma=1) {
    x = rnorm(n)  # from standard normal distribution
    x = sigma * x / sd(x)  # scale to desired SD
    x = x - mean(x) + mu  # center around desired mean
    return(x)
}


x = rnorm(n=20, mean=5, sd=10)
mean(x)  # is e.g. 6.813...
sd(x)  # is e.g. 10.222...

x = rnorm_fixed(n=20, mean=5, sd=10)
mean(x)  # is 5
sd(x)  # is 10


rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
r <- rnorm2(100,4,1)
mean(r)  ## 4
sd(r)    ## 1


runif() # By default, its range is from 0 to 1.

runif(3, min=0, max=100)

rnorm(4)
#> [1] -2.3308287 -0.9073857 -0.7638332 -0.2193786

# Use a different mean and standard deviation
rnorm(4, mean=50, sd=10)
#> [1] 59.20927 40.12440 44.58840 41.97056

# To check that the distribution looks right, make a histogram of the numbers
x <- rnorm(400, mean=50, sd=10)
hist(x)


# data select # extract 

d <- data.frame(Name = c("A", "B", "C", "D", "E"),Amount = c(150, 120, 175, 160, 120))
str(d)

subset(d, Amount==120)

d[Amount=150]

d[which.min(d$Amount), ]
#   Name Amount
# 2    B    120

d[which(d$Amount == min(d$Amount)), ]
#   Name Amount
# 2    B    120
# 5    E    120

# subset function
newdata <- subset(mydata, age >= 20 | age < 10,select=c(ID, Weight))


# package handling
d1 <- installed.packages()
str(d1)

d1 <- data.frame(d1)
str(d1)

attach(d1)

d2 <- d1[order(Package),]

head(d2)

sort(d1$Package)

head(d1)
install.packages("QuantPsyc")
library(lm.beta)
lm.beta(x1)


# matrix multiply 
rep(bias, each=2)



# counting : Combining the length() and which() commands gives a handy method of counting elements that meet particular criteria.

b <- c(7, 2, 4, 3, -1, -2, 3, 3, 6, 8, 12, 7, 3) 
b

# Let’s count the 3s in the vector b.

count3 <- length(which(b == 3)) 
count3  # 4

# In fact, you can count the number of elements that satisfy almost any given condition.

length(which(b < 7))  #  9

# Here is an alternative approach, also using the length() command, but also using square brackets for sub-setting:
    
length(b[ b < 7 ]) # 9



## 


x <- c(1:10)
x[2] # 2, n 번째 값 출력

x[-10] # n 번째 값 제외하고 출력

append(x, 999, after=3) # vector에 새 요소(999) 4번째 자리에 새로 추가 / # after=0 은 맨 앞 자리 
x1 <- 1:5
x2 <- 3:8
setdiff(x1, x2) # 1,2 vec1에 있는데 vec2에는 없는 요소만 출력

intersect(x1, x2) # 교집합

names(x) <- c("k", "l", "m") # 각 column 이름 지정

length(x) # 벡터의 전체 길이

sum(x) # 벡터의 합

NROW(x)# 배열 요소 수

nrow(x) # 행 수 (행렬만 가능)

3 %in% x # x 에 a가 있는지 검색(True/False)

x <- seq(1, 10, 2) 
x <- seq(1, 10, 3) 
x
x <- rep(1:5, 3)  

x <- rep(1:5, each=2)  

# matrix : variable data type 
 
x <- c(1:10)
x <- matrix(x, 2, 5) 
x
x <- matrix(c(1,2,3,4), nrow=2) 

x <- matrix(c(1,2,3,4), nrow=2, byrow=T) 

x[a,b]

colnames(x) <- c("A", "B") 
x

t(x) # 전치행렬

solve(x) # 역행렬

x1 <- matrix(1:4, nrow = 2)
x2 <- matrix(1:4, nrow = 2, byrow = T) 
x2
x1 * x2  # 행렬 각 요소끼리 곱

x1 %*% x2 # matmul 

x[2,, drop = FALSE] # 행렬 로 뽑아내기
x
rbind(x1, c(99,100))  # 행 추가

cbind(x1, c(11,12)) # 열 추가


# list :  


x <- list("a", "b", "c", "d")


### data.frame : vector, matrix, list  
v1 <- 1:3
v2 <- c("a","b","c")
m1 <- matrix(1:4, nrow = 2)

x <- data.frame(v1, v2) # , m1)
x
 
x$column1 = 9 # 해당 column의 data 조회 # 데이터프레임[행번호] 와 같은 결과
x
a <- data.frame(col1 = factor(2), col2 = numeric(3))
a
a <- edit(a) # edit() 으로 R 데이터 편집기 실행

x <- read.csv("C:/~~경로/filename.csv", header = TRUE)

summary()  
x <- edit(x)
x
mean(x$column1, na.rm=T) # na.rm = TRUE  
x

attach(x) #  

subset(x, v1<2) 

x[c(2, 3, 1), 1:2 ] 

ncol(x)
nrow(x)
names(x)
rownames(x) 

# 
c <- stack(list(v1=v1, v2=v2),x,drop = T)
c

#
l <- list(c)
l
l[["test"]] <- 1:3 # NULL
l[[3]] <- 2:5

l


getwd()


# renaming

# get column names
colnames(my_data)

# Rename column where names is "Sepal.Length"
names(my_data)[names(my_data) == "Sepal.Length"] <- "sepal_length"
names(my_data)[names(my_data) == "Sepal.Width"] <- "sepal_width"
my_data


## 

library(dplyr)

df1 %>%
    select(A, B, E)

select(df1, A, B, E)




##
df <- data.frame( c( 183, 85, 40), c( 175, 76, 35), c( 178, 79, 38 ))
names(df) <- c("Height", "Weight", "Age")

# All Rows and All Columns
df[,]
# First row and all columns
df[1,]
# First two rows and all columns
df[1:2,]
# First and third row and all columns
df[ c(1,3), ]
# First Row and 2nd and third column
df[1, 2:3]
# First, Second Row and Second and Third COlumn
df[1:2, 2:3]
# Just First Column with All rows
df[, 1]
# First and Third Column with All rows
df[,c(1,3)]



