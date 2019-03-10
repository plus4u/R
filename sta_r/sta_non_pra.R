### non parametric


## 1. one sample




## 2. paired 

# paired 10 sample of patient's blood sugar
x1 <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)
# wilcox signed rank test
wilcox.test(x1, x2, 
                +        alternative = c("greater"), 
                +        paired = TRUE, 
                +        conf.level = 0.95)

 

## 3. 2 sample


# 3.1 independent 2 samples : Wilcoxon rank sum test (È¤Àº Mann-Whitney U-test)

 wilcox.test()
  
  # Dataset
 library(MASS)
 str(Cars93)
  
 ##--- way 1 : y ~ Factor
 wilcox.test(Price ~ Origin, 
               +             data=Cars93, 
               +             alternative = c("two.sided"), 
               +             mu = 0, 
               +             conf.int = FALSE, 
               +             conf.level = 0.95)
 
 
 
  
# 3.2 -- way 2. x, y numeric vectors
 
 # x, y numeric vector indexing
 Price_USA <- Cars93[which(Cars93$Origin == c("USA")), c("Price")]
 Price_nonUSA <- Cars93[which(Cars93$Origin == c("non-USA")), c("Price")]

  wilcox.test(Price_USA, Price_nonUSA, 
                +             alternative = c("two.sided"), 
                +             mu = 0, 
                +             conf.int = FALSE, 
                +             conf.level = 0.95)
  
  
 
