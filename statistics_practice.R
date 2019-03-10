### statistics practice ## 


##  normality test (3)

x <- sample(x=1:200, size = 50, replace = F)
qqnorm(x)
qqline(x, col=2)


shapiro.test(x)

# kolmogorov-smirnov test

ks.test(x, "pnorm", mean(x), sd(x))


## 

s1 <- c(sample(100:115, 30, replace = T))
s2 <- c(sample(110:125, 30, replace = T))
s3 <- c(sample(120:140, 30, replace = T))
s <- c(s1,s2,s3)

group <- as.factor(c(rep(1,30), rep(2,30), rep(3,30)))
group 

d1 <- data.frame(group, s)

str(d1)


# install.packages("lawstat")
library(lawstat)

levene.test(s, group)
bartlett.test(s~group,d1) 




## 

library(readxl)

## 1. t.test

t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)


boy <-  c(46,47,58,47,27,58,56,26,47,25)
girl <- c(78,57,31,28,67,77,36,57,36,57)
t.test(boy,girl, var.equal = T)
t.test(boy,girl)

 




# 2. anova  # One Way Anova (Completely Randomized Design)

fit <- aov(y ~ A, data=mydataframe)

bartlett.test(Sepal.Width~Species, data=iris)
levene

one.anova <- read_excel("c:/R/work_r/source/oneway_anova.xlsx", col_names = TRUE)

# Rename column where names is "Sepal.Length"
names(one.anova)[names(one.anova) == "만족도"] <- "satisfaction"
names(one.anova)[names(one.anova) == "편의점"] <- "constore"

head(one.anova,3)
 
bartlett.test(satisfaction ~ constore, data=one.anova)
 
library(car)

# Levene's test with one independent variable

str(one.anova)

one.anova$constore <- as.factor(one.anova$constore) # x.var = factor 

leveneTest(satisfaction ~ constore, data=one.anova, center=mean)

fit <- aov(satisfaction ~ constore, data=one.anova)

summary(fit)
fit

TukeyHSD(fit)
Scheffe(fit)


# Randomized Block Design (B is the blocking factor) 
fit <- aov(y ~ A + B, data=mydataframe)

# Two Way Factorial Design 
fit <- aov(y ~ A + B + A:B, data=mydataframe)
fit <- aov(y ~ A*B, data=mydataframe) # same thing
# Analysis of Covariance 
fit <- aov(y ~ A + x, data=mydataframe)
aov(formula, data = NULL, projections = FALSE, qr = TRUE,
    contrasts = NULL, ...)
aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)

## 2.2

two.anova <- read_excel("c:/R/work_r/source/twoway_anova.xlsx", col_names = TRUE) 

str(two.anova) 
two.anova$smoking_yn <- as.factor(two.anova$smoking_yn)
two.anova$location <- as.factor(two.anova$location)

fit <- aov(revenue ~ smoking_yn + location, data = two.anova)

summary(fit)

fit1 <- aov(revenue ~ smoking_yn * location, data = two.anova) # the same below

fit <- aov(revenue ~ smoking_yn + location + smoking_yn:location, data = two.anova)

summary(fit)

## post-hoc test : Since Tukey’s test is a post-hoc test, we must first fit a linear regression model and perform ANOVA on the data. ANOVA in this example is done using the aov() function.

fit.lm <- lm(revenue ~ smoking_yn + location, data = two.anova)

fit2 <- aov (fit.lm)
summary(fit2)

summary(fit)

two.tukey.test <- TukeyHSD(fit2) # TukeyHSD() function is available in base R
two.tukey.test <- TukeyHSD(fit)

two.tukey.test
summary(two.tukey.test)
# 

layout(matrix(c(1,2,3,4),2,2))# optional layout 
plot(fit) # diagnostic plots

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests

# Tukey Honestly Significant Differences
TukeyHSD(fit) # where fit comes from aov()

# Two-way Interaction Plot 
attach(mtcars)
gears <- factor(gears)
cyl <- factor(cyl)
interaction.plot(cyl, gear, mpg, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Number of Cylinders", 
                 ylab="Mean Miles Per Gallon", 
                 main="Interaction Plot")



# MANOVA test

res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)



# 3. pca

x1 <- (1:10)
y1 <- c(1, 3, 5, 8, 9, 13, 15, 19, 21, 25)

x2 <- cor(x1,y1)
x2
eigen(x2)

x2


## compare fa, factanal, prcomp : ref factanal.r ##

# fa : fa {psych}	R Documentation / Exploratory Factor analysis using MinRes (minimum residual) as well as EFA by Principal Axis, Weighted Least Squares or Maximum Likelihood

# rotate : none, varimax ~

find.package("psych")
# installed.packages()
# install.packages("psych")

library(psych)

f1 <- fa(x, 4, fm = pa)


# maximum likelyhood : factanal {stats}	R Documentation : Factor Analysis , Description : Perform maximum-likelihood factor analysis on a covariance matrix or data matrix.

# rotate : character. "none" or the name of a function to be used to rotate the factors: it will be called with first argument the loadings matrix, and should return a list with component loadings giving the rotated loadings, or just the rotated loadings.

# Usage : factanal(x, factors, data = NULL, covmat = NULL, n.obs = NA, subset, na.action, start = NULL, scores = c("none", "regression", "Bartlett"), rotation = "varimax", control = NULL, ...) 

factanal(x, rotation = "oblimin", control = NULL, ...) 


## ~ ## 


# correlation

var(x, y = NULL, na.rm = FALSE, use)

cov(x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

cor(x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

cov2cor(V)

x <- sample(1:100, 10)
x
y <- sample(100:1000, 10)
y

cov(x, y) #  cor(x,y)  / plot(x,y)


x = rnorm(n=10, mean=2, sd=10)

y = x + runif(1)
y1 = x + runif(10, min = 1, max = 30)
    
cor(x,y1, method = 'pearson')  # spearman

plot(x,y1)
y1

x2 <- data.frame(x,y1)
x2

x2$d = x-y1
x2

cor(x2[,1], x2[,2])

cor.test(x2[,1], x2[,2])

# linear regression / Standardized Regression Coefficients : Computes the standardized regression coeffients (beta) from an object of class (lm)

install.packages("QuantPsyc")

lm.beta(MOD)

# MOD is object from lm with the form y ~ x1 + x2 + ... 


## fa 

library(psych)

fit <-fa(pcaFa, 5 , fm = "pa" ) # wls 잔차제곱합
fa.diagram (fit)
plot (fit)


##  Basic ScatterPlot Matrix

pairs(~mpg+disp+drat+wt,data=mtcars, 
      main="Simple Scatterplot Matrix")


# kmo

x2 <- cor(pcaFa)

str(x2)
KMO(x2)


## 4. reliability : use result of factor analysis

pca.factor <- read_excel("c:/R/work_r/source/pca_factor.xls", sheet="raw_data", col_names = TRUE) 

pairs(pca.factor, panel=panel.smooth)

head(pca.factor, 3)


# install.packages("psy")
library(psy)

q1 <- pca.factor[2:4]  # data.frame(df$A,df$B,df$E)
     
q1                       
                   

cronbach(q1) 



# 5. correlation

x <- pca.factor[2:4]
y <- pca.factor[5:7]
x
cor(x)
cor(x, y)


cor(mtcars, use="complete.obs", method="kendall") 
cov(mtcars, use="complete.obs")

# Correlation matrix from mtcars
# with mpg, cyl, and disp as rows 
# and hp, drat, and wt as columns 
x <- mtcars[1:3]
y <- mtcars[4:6]
cor(x, y)

## 6. linear regression
# Multiple Linear Regression Example 
fit <- lm(y ~ x1 + x2 + x3, data=mydata)
summary(fit) # show results

## 7. mixed anova  : need reasonable dataset 
library(tidyr)

# Convert it to long format
data_long <- gather(data, time, value, before:after)
# Look at first few rows
head(data_long)


# This is what the original (wide) data looks like
# subject sex   age before after

aov_age_time <- aov(value ~ age*time + Error(subject/time), data=data_long)
summary(aov_age_time)




# 8. ANCOVA
# Get the dataset.
input <- mtcars

# Create the regression models.
result1 <- aov(mpg~hp*am,data = input)
result2 <- aov(mpg~hp+am,data = input)

# Compare the two models.
print(anova(result1,result2))




## 9. Discriminant analysis
# Linear Discriminant Analysis with Jacknifed Prediction 
library(MASS)
fit <- lda(G ~ x1 + x2 + x3, data=mydata, 
           na.action="na.omit", CV=TRUE)
fit # show results



## 10. cluster
# procedure 1. data preparation  2. Partitioning  3. Hierarchical Agglomerative   4. model  5. plotting 6. Validating cluster solutions

# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables


# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)


# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)


# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model


# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)



# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)



    



