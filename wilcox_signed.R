# 1. wilcox.test / paired = FALSE

A <- c(71, 78, 79, 71, 86, 81, 92)

B <- c(73, 79, 81, 74, 97, 88, 99)

wilcox.test(A, B, paired = FALSE, alternative = "two.sided", mu = 0.0, 
            exact = TRUE, correct = TRUE, conf.int = TRUE, conf.level = 0.95)

#  W = 17, p-value = 0.1848   //two.sided p-value = 0.3695


wilcox.test(A, B, paired = T, alternative = "two.sided", mu = 0.0, 
            exact = TRUE, correct = TRUE, conf.int = TRUE, conf.level = 0.95)

# less      -> V = 0, p-value = 0.01101
# two.sided -> V = 0, p-value = 0.02201


## 2. mann whitney 

x <- c(5.7 , 6.3 ,6.4, 6.5 ,7.8 , 8.6, 9.8, 10.2 )

y <- c(3.8 , 4.3 , 5.6 , 5.7, 6.3 , 6.6 , 6.8 , 7.1 , 8.4 )

wilcox.test(x, y, paired = F, alternative = "two.sided", conf.level = 0.95)

# two.sided   W = 53, p-value = 0.1119 / 

wilcox.test(x, y, correct=FALSE)   # W = 53, p-value = 0.1015


##

a <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7, 156.6, 174.5, 184,4, 165.2, 147,8, 177.8, 160.1, 160.5)

wilcox.test(a, mu=160) # V = 97, p-value = 0.6397

wilcox.test(a, alternative = "less", mu = 160.0,  exact = TRUE, correct = TRUE, conf.int = TRUE, conf.level = 0.95)  # two.sided p-value = 0.6397 / less p-value = 0.6953
 

a <- c(70, 60, 45, 90, 95, 80, 85)
b <- c(100, 95, 55, 65, 75, 75)
wilcox.test(a,b, alternative = "less")  # .44  # 


# PLOT

ab <- data.frame(a,b)

boxplot(a,b) 

 