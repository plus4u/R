### 1. one-way anova  2. two   3. manova

## 1. one-way anova : y ~ x1

# sample(x, size, replace = FALSE, prob = NULL)
#  <- runif(10, 5, 20)
# sample(1:10, 10, replace=T)
# sample(iris.Species, 10)
# <- rnorm( 10, mean=5, sd=1) 

y1 <- rnorm( 10, mean=45, sd=1)# group 1
y2 <- c(47.3, 47.3, 46.3, 47.3, 47.3, 47.3, 45.3, 47.3, 45.3, 47.3)  # group 2
y3 <- rnorm( 10, mean=47, sd=1)  # group 3

summary(y2)
sd(y2)

y <- c(y1, y2, y3)
n <- rep(10, 3)

group <- rep(1:3, n)
group

y_df <- data.frame(y, group)
 
summary(y1)  
summary(y2)   
summary(y3)  

fit <- aov(y ~ group, data = y_df)
summary(fit)   

bartlett.test(y ~ group, data = y_df) # to test equal variances  / sd =1 or .89
# Bartlett's K-squared = 0.22271, df = 2, p-value = 0.8946
sd(y2)  # 0.8498366

# transform to factor

sapply(y_df, class)

y_df <- transform(y_df, group=factor(group))
sapply(y_df, class)

fit <- aov(y ~ group, data = y_df)
summary(fit)  # group        2  18.84   9.421   15.33 3.56e-05 ***

bartlett.test(y ~ group, data = y_df)  # Bartlett's K-squared = 0.22271, df = 2, p-value = 0.8946

#exercise :  iris

str(iris)
 
## 2. two-way anova : y ~ x1 + x2,  y~x1*x2,  

# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)


## boxplot

search()  # 

attach(y_df) 

boxplot(y ~ group, 
         main = "Boxplot of y values by group condition 1/2/3", 
         xlab = "Factor Levels : group condition 1/2/3", 
         ylab = "y values")

# descriptive statistics by group

tapply(y, group, summary)

 
detach(y_df)


## 2. two way

## two-way anova


gender <- as.factor(c(rep("M", 9), rep("F", 9)))

class <- c("c_1", "c_1", "c_1", "c_2", "c_2", "c_2", "c_3", "c_3", "c_3")

class <- as.factor(c(rep(class, 2)))

s1 <- rnorm( 9, mean=5, sd=1) 
s2 <- rnorm( 9, mean=7, sd=1) 
total <- c(s1,s2)

# c(71, 77, 78, 76, 77, 78, 71, 70, 69, 76, 76, 80, 79, 78, 77, 71, 71, 70)

# summary statistics
score <- data.frame(gender, class, total)

# install.packages("doBy")

library(doBy)

summaryBy(total ~ gender, data=score, FUN = c(mean, sd, min, max))

summaryBy(total ~ class, data=score, FUN = c(mean, sd, min, max))

summary(total, data=score)


# box plot and interaction plot
par(mfrow = c(2, 2))

plot(total ~ gender, main="box plot by gender")
plot(total ~ class, main="box plot by class")
interaction.plot(gender, class, total, bty='l', main="interaction effect plot")
interaction.plot(class, gender, total, bty='l', main="interaction effect plot")

# two-way ANOVA : aov()

fit = aov( total ~ gender + class + gender:class)
summary(fit)

str(fit)




