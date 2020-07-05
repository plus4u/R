# install.packages("installr")

# library(installr)

# check.for.updates.R() 

# install.R()
# updateR() 

version
 
set.seed(1)


##

 
# install.packages("readxl")
# library(readxl)

# C:\Working\work_r\source

# x <- read_excel("C:/working/work_r/source/1_ttest.xls", col_names = TRUE)


x <- c(1,2,3, 4, 5)

x <- (1:10)

x

summary(x)

str(x)
summary(x)
nrow(x)

##


sample(1:10,4)

sample(1:10,10, replace=TRUE) #   7  6 10 10 10  4 10  6  5  3

sample(1:10,10, replace=FALSE)

x <- 1:20

x

## 

df1 =data.frame(d1, d2)
df1

##

names(df1) [2]
names(df1) [2] <- c("sales")

# 

install.packages("xlsx")

library(xlsx)

# C:\Working\work_r


getwd()

setwd("C:/Working/work_r")


x1 <- c(1, 2, 3)

x2 <- c("kim", "lee", "hong")
x3 <- c(100, 200, 300)

d1 <- data.frame(x1,x2, x3)

d1

##


sample(3, 10, replace=TRUE, prob=c(0.4, 0, 0.6)) 

sample(5, 10, replace=TRUE, prob=c(0.1, 0, 0.3, 0.6, 0)) 


x1 = sample(3, 100, replace=TRUE, prob=c(0.1, 0.2, 0.7)) 

x1 = sample(10, 100, replace=TRUE ) 

x2 = sample( 100:200, 100, replace=TRUE)

x2

x12 <- data.frame(x1,x2)

summary(x2)

##


write.xlsx(x12, "tempo.xlsx", row.names=F)




## graph


library('ggplot2')


ggplot(data=mtcars, aes(x=disp, y=mpg)) + geom_point(aes(size=hp, color=wt))

ggplot(data=x12, aes(x=x1, y=x2)) + geom_point(alpha = 0.9)

plot(x1, x2, data(x12))

p <- ggplot(data=mtcars, aes(x=disp, y=mpg, size=hp, color=wt)) + geom_point()

p

p + stat_smooth(color='black', fill='grey')  # 추세선 추가 

#

x1 <- sample(1:100)

x2 = x1*2

x3 <- sample(1:10, 100, replace=TRUE)

x4 = x2 + x3

x <- data.frame(x1, x4)

plot(x1, x4, data=x) 

ggplot(data=x, aes(x=x1, y=x4)) + geom_point(alpha = 0.9)+ stat_smooth(color='black', fill='grey')  # 추세선 추가 
