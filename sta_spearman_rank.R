## spearman rank correlation analysis

# rank(x1)


x1 <- c(6,4,3,7,1,2,5)
x2 <- c(1,5,6,2,3,7,4)
m1 <- matrix(c(x1,x2),ncol = 2)
 
cor(m1, method="spearman")


x <- c(2,3,5.12,1.25,10,5.21,2.25)  

x3 <- rank(x)

y <- c(1, 0.37, 0.33, 0.87, 0.72, 0.3, 0.56)  

m2 <- matrix(c(x,y),ncol = 2)
m2

cor(m2, method="spearman")


