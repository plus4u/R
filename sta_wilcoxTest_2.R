## independent two population proportions test : prop.test()
##----------------------------------------------------------

##--- way 1 : y ~ Factor
library(MASS)

wilcox.test(Price ~ Origin, 
             data=Cars93, 
             alternative = c("two.sided"), 
             mu = 0, 
             conf.int = FALSE, 
             conf.level = 0.95)

##--- way 2. x, y numeric vectors

# x, y numeric vector indexing

 Price_USA <- Cars93[which(Cars93$Origin == c("USA")), c("Price")]
 Price_nonUSA <- Cars93[which(Cars93$Origin == c("non-USA")), c("Price")]
 wilcox.test(Price_USA, Price_nonUSA, 
             alternative = c("two.sided"), 
             mu = 0, 
             conf.int = FALSE, 
             conf.level = 0.95)
 
 wilcox.test(Price ~Origin, data=Cars93, alt="two.sided", conf.level=0.95 ) 
 
 
 ## 
 
 group <- c(rep("a",4),rep("b",3))
group 

x <- c(6,1,4,2,3,2,7) 

d1 <- data.frame(x,group)
d1$f <- as.factor(d1$group)
d1

wilcox.test(d1$x~d1$f, exact=T)

wilcox.test(d1$x~d1$f, alternative ='greater', paired=F)

a <- c(6,1,4,2)
b <- c(3,2,7,na)

d1 <- data.frame(a,b)

d1

wilcox.test(a, b, data=d1)
