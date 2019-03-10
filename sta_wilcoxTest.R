### wilcoxon signed-rank test

# 1. data-set

subject<-c(1:10)
before_w<-c(35,40,41,50,43,45,49,72,51,40)
after_w<-c(39,43,48,52,47,51,54,69,58,62)
dat<-data.frame(subject,before_w,after_w)
dat$diff<-dat$after_w-dat$before_w
head(dat)
dat

# 2. data exploratory 
summary(dat$before_w)
summary(dat$after_w)
summary(dat$diff)

# 3. normality 

shapiro.test(dat$diff)

# 4. Wilcoxon signed rank test
wilcox.test(after_w,before_w,paired=T)

 


subject<-c(1:12)
b<-c(37,41,45,51,45,49,53,72,59,60, 66, 70)
a<-c(39,43,48,52,47,51,54,69,58,62, 70, 80)
dat2<-data.frame(subject,b,a)
dat2$diff<-dat2$a-dat2$b

head(dat1)

# 
summary(dat1$b)
summary(dat1$a)
summary(dat1$diff)

# 
shapiro.test(dat2$diff)

#Wilcoxon signed rank test
wilcox.test(data=dat2, a,b,paired=TRUE)

wilcox.test(a,b,paired=TRUE)

wilcox.test(data=dat1, a,b,paired=TRUE)

shapiro.test(dat1$diff)

wilcox.test(data=dat1, a,b,paired=TRUE)

rm(dat)

wilcox.test(a,b,paired=TRUE)

shapiro.test(dat1$diff)

wilcox.test(a,b,paired=TRUE)
dat1
dat
wilcox.test(before_w,after_w,paired=TRUE)
wilcox.test(dat1$before_w,dat1$after_w,paired=TRUE)


## object a, b?? ???? ???ڰ? ??? ?־? ?????? ?޶???

x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(x, y, paired = TRUE, alternative = "greater")
wilcox.test(y - x, alternative = "less")    # The same.

a <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7, 156.6, 174.5, 184,4, 165.2, 147,8, 177.8, 160.1, 160.5)

wilcox.test(a, mu=160)


## wilcoxon rank sum test

# test
x <- c(17, 12, 13, 16, 9, 19, 21, 12, 18, 17)
y <- c(10, 6, 15, 9, 8, 11, 8, 16, 13, 7, 5, 14)

#  calculate the statistic TW by hand, rank(Xi)=156.5 

wilcox.test(x, y, correct = F)  # W = 101.5

# not through something like
dat <- data.frame(v = c(x, y), s = factor(rep(c("x", "y"), c(10, 12))))
dat$r <- rank(dat$v)
T.W <- sum(dat$r[dat$s == "x"])


## answer

a <- wilcox.test(x,y,correct=FALSE)
str(a) 

n1 <- length(x)
a$statistic <- a$statistic + n1*(n1+1)/2
names(a$statistic) <- "T.W"
a








