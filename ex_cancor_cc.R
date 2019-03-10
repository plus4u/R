### cancor

# pcaFa <- read_excel("work_r/source/pcaFa_data.xls", col_names = TRUE)
a <- read.csv("work_r/MVA+EX1-1.csv")

x <- a[,1:4]
y <- a[, 5:8]

cor(x)
cor(y)
cor(a)

# calculate jungjun correlation  


## CCA  # scores 

install.packages("CCA")

library(CCA)

x <- a[, 1:4]
y <- a[, 5:8]

xy <- cc(x,y)
xy

# matcor(x,y)
# xy$score

w1=xy$scores$xscore[,1]
v1=xy$scores$yscore[,1]

plot(w1,v1)
