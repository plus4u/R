### 1. one-way anova  2. two   3. manova

## 1. one-way anova 

y1 <- c(50.1, 52.1, 51.1, 52.1, 50.1, 51.1, 51.1, 52.1, 51.1, 50.1)
y2 <- c(47.3, 47.3, 46.3, 47.3, 47.3, 47.3, 45.3, 47.3, 45.3, 47.3)
y3 <- c(46.0, 47.1, 45.2, 47.3, 47.4, 46.5, 45.6, 47.7, 44.8, 46.9)

summary(y1)

aov(y1, y2, y3) # Error: $ operator is invalid for atomic vectors

y <- c(y1, y2, y3)
y 

n <- rep(10, 3)
n 

group <- rep(1:3, n)
group 

# combining into data.frame
group_df <- data.frame(y, group)
group_df

sapply(group_df, class) 

# transform from 'integer' to 'factor'
group_df <- transform(group_df, group = factor(group))
sapply(group_df, class) 


# boxplot
attach(group_df) 

boxplot(y ~ group, 
         main = "Boxplot of Daily Outcome by Temperature condition 1/2/3", 
         xlab = "Factor Levels : Temperature condition 1/2/3", 
         ylab = "Daily Outcome")

# descriptive statistics by group
 tapply(y, group, summary)

 
 search()
 attach(group_df) 
 search()
 
 detach(group_df)
 
 # one-wayANOVA
 aov(y ~ group, data = group_df)
 
 summary(aov(y ~ group, data = group_df))
 
 # Bartlett test to test the null hypothesis of equal group variances
 
 bartlett.test(y ~ group, data = group_df)
 
  
 



