### 1. principal  2. 

## 1. Principal - screeplot ?? , 2. KMO, Bartlett, /  prcomp-screeplot (ok)
# http://faculty.missouri.edu/huangf/data/mvnotes/pca_in_r_2.html 
# Manually running a pca

# 1. 




# 1-2 : 2. Using a function for running a pca


library(psych)

# 
dat = pca_xls[2:16]

p <- ncol(dat) #no of variables

p

 
R <- cor(dat) # 1) correlation matrix

cov(dat$q1,dat$q2) # 1.210247
sd(dat$q1) # 1.170612
sd(dat$q2) # 1.194225  -> rho = 1.210247/(1.170612*1.194225) = 0.8657148844233068

cortest.bartlett(R, n=nrow(dat)) # 2) 
cortest.bartlett(R, n=15, diag = T)  #  why does it different from spss ?

bartlett.test(dat, n=15, diag = T) 
KMO(R)


###

bart<-function(dat){ #dat is your raw data
  R<-cor(dat)
  p<-ncol(dat)
  n<-nrow(dat)
  chi2<- -((n-1)-((2*p)+5)/6 ) * log(det(R)) #this is the formula
  df<-(p*(p-1)/2)
  crit<-qchisq(.95,df) #critical value
  p<-pchisq(chi2,df,lower.tail=F) #pvalue
  cat("Bartlett's test of sphericity: X2(",
      df,")=",chi2,", p=",
      round(p,3),sep="" )   
}

bart(dat)



###




# 2) 

e <- eigen(R) #solving for the eigenvalues and eigenvectors from the correlation matrix

L <- e$values # from manually 
 
plot(L,main="Scree Plot",ylab="Eigenvalues",xlab="Component number",type='b')
abline(h=1, lty=2)

pca <- principal(dat, nfactor=p, rotate="none") #  

pca <- principal(dat, nfactor=p, rotate="varimax") 

pca <- principal(dat, nfactor=5, rotate="varimax") ## 3) check of result doc doc.

pca # 4) interpreting results

str(pca) 

pca$loadings # 
pca$ weights

pca$ Structure
pca$Vaccounted
pca$rot.mat
pca$r.scores
pca$residual
pca$STATISTIC
pca$communality
pca$criteria



### pca : 1: basic, 2 : sample -> train + test : 
### pca : 3 : excel pca dataset 

getwd()

## case 3-1 : 3 step -> tol ?? , 3-2 : principle (varimax)  3-3 : spss


library(readxl)

pca_xls <- read_excel("source/pca.xls", col_names = T) # import 

pca_prc <- prcomp(pca_xls[2:16], center = T, scale. = T)

str(pca_prc)  # list of 5

pca_prc$rotation

pca_prc$x  # 66 row x pc15
pca_prc$sdev

summary(pca_prc)

pca_load <- as.data.frame(pca_prc$rotation[, 1:5]) # 


# screeplot

screeplot(pca_prc, type = "lines")

summary(pca_prc)

# pca_2 <- prcomp(pca_prc, tol = .75)  # tol 
# pca_3 <- varimax(pca_2$rotation) 

pca_3 <- varimax(pca_prc$rotation) 

pca_prc$rotation

summary(pca_3)

str(pca_3)

pca_3$lodings


## case  3-2 :

# Varimax Rotated Principal Components  / # retaining 5 components 
library(psych)

fit  <- principal( pca_xls, nfactors=5, rotate="varimax")

summary(fit)

fit 


## case 3-3 : GOOD !
#For comparison with SPSS  (contributed by Gottfried Helms)
# https://www.personality-project.org/r/html/principal.html 

fit <- principal( pca_xls[2:16], 5, rotate="varimax", normalize=F) # T, F ?

print(fit, digits=7)

summary(fit)
 
p <- print(fit, digits=7)
p

round(p$Vaccounted,2)   # the amount of variance accounted for is returned as an object of print

# 

screeplot(pca_3, type = "lines") # error 

plot(pca_3)

fviz_eig(pca_3)  # need library(factoextra)



# error below 
screeplot(pca_3, npcs = 24, type = "lines")


require(graphics)

screeplot(pca_3, type = "l")

## e

# https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r
#
# varimax : http://r.789695.n4.nabble.com/prcomp-and-varimax-td4663303.html
install.packages("psych")
library(psych)

ncomp <- 5
#

pca_xl_rotated <- psych::principal(pca_xls, rotate="varimax", nfactors=ncomp, scores=TRUE)
print(pca_xl_rotated$scores[1:5,])  # Scores returned by principal()

#
pca_xl_prc      <- prcomp(pca_xls, center=T, scale=T)
rawLoadings     <- pca_xl_prc$rotation[,1:ncomp] %*% diag(pca_xl_prc$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(pca_xls) %*% invLoadings
print(scores[1:5,])                   # Scores computed via rotated loadings

scores <- scale(pca_xl_prc$x[,1:2]) %*% varimax(rawLoadings)$rotmat
print(scores[1:5,])






## case 1 : 

#
cor(iris[1:4])

# 
log_ir <- log(iris[,1:4])

ir_species <- iris[,5]

#

ir_pca <- prcomp(log_ir,center = T, scale. = T)



summary(ir_pca)
 

# 

plot(ir_pca, type="l")


#
summary(ir_pca) 


# check with lm

PRC <- as.matrix(log_ir) %*% ir_pca$rotation

head(ir_pca$rotation)


#
train1 <- cbind(ir_species,as.data.frame(PRC))

train1[,1] <- as.factor(train1[,1])

colnames(train1)[1] <- "label"

# lm with pc1, pc2

fit1 <- lm(label~PC1+PC2, data=train1) 


# predict

fit1_pred <- predict(fit1,newdata = train1)

head(fit1_pred)

fit1_pred[101]

str(fit1_pred)

# round : how to know 1~3 

b <- round(fit1_pred)

head(b)

b[101]

str(b)

b[b==0 | b==1] <- "setosa"

b[b==2] <- "Versicolor"

b[b==3] <- "Virginica"


## case 2: 

sample <- iris[sample(1:nrow(mydata), 50, replace=FALSE),]

str(sample)

## A.   
str(iris)
dt <- iris[, -5]


# 범주형 변수
dt_group <- iris[, 5]

# 데이터가 한쪽에 편향되어 있기 때문에 log변환 여기선 skip  
# log_ir <- log(iris[,1:4])
# center = T는 중앙을 0으로, scale.=T 는 분산을 1로

pca_dt <- prcomp(dt, center = T, scale. = T)


print(pca_dt) # 변수 중 계수인 rotation 변수값이 조회

pca_dt$sdev


# log변환 기준: PC1 =  0.5038236 * Sepal.Length -0.3023682 * Sepal.Width + 0.5767881 * Petal.Length + 0.5674952 * Petal.Width 의 선형식을 가지는 변수라고 해석


str(pca_dt)

pca_dt$x


# Proportion of variance 출력
plot(pca_dt, type = "l")

summary(pca_dt) # 값을 확인하여 주성분 요인을 선택 

PRC <- as.matrix(log.ir) %*% ir.pca$rotation # 원래 데이터와 선형계수를 메트릭스 곱으로 선형조합 : skip

head(pca_dt$x, n=10)

train1 <- cbind(dt_group,as.data.frame(PRC)) # 회귀분석 위해 y변수를 합치고 factor처리 후 보기 좋게 이름에 label

train1 <- cbind(dt_group, as.data.frame(pca_dt$x)) 

train1 

train1[,1] <- as.factor(train1[,1])

colnames(train1)[1] <- "label"

str(train1)

fit1 <- lm(label~PC1+PC2, data=train1)  # 주성분 분석을 통해 선택한 PC1과 PC2를 가지고 회귀분석 


fit1_pred <- predict(fit1,newdata = train1) # 모델의 예측력이 좋은지 확인위해 predict 함수로 예측



b <- round(fit1_pred)

b[b==0 | b==1] <- "setosa"

b[b==2] <- "Versicolor"

b[b==3] <- "Virginica"

# table함수로 비교 
 

table(b,dt_group)


# log 변환과 비교 : https://m.blog.naver.com/leedk1110/220783514855



## sample  

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mtcars))

head(smp_size,10) 

smp_size

## set the seed to make your partition reproducible
# 1. 
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

train_ind 

# 2. 
set.seed(123)
train_ind <- sample(nrow(mtcars), size = smp_size)

train_ind 
# [1] 10 25 13 26 27  2 14 23 29 11 22 32 24 31 28 16  4  1  5 30 19  8  7  9

## 3. 
set.seed(123)
train_ind <- sample(mtcars, size = smp_size)

train_ind 

# train_ind
# [1] 10 25 13 26 27  2 14 23 29 11 22 32 24 31 28 16  4  1  5 30 19  8  7  9



## B. train + test 

dt_ir <- iris[, -5]

# train, test dataset 

# 75% of the sample size
smp_size <- floor(0.75 * nrow(iris))


# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(nrow(iris), size = smp_size, replace = FALSE)

train <- iris[train_ind, 1:4]
head(train, n=10)

test <- iris[-train_ind, 1:4]
head(test, n=10)

dt_group_tr  <- iris[train_ind, 5]
dt_group_te  <- iris[-train_ind, 5]

str(dt_group_te)
dt_group_te
             
pca_dt <- prcomp(train, center = T, scale. = T)

pca_dt_te <- prcomp(test, center = T, scale. = T)

print(pca_dt) # 변수 중 계수인 rotation 변수값이 조회

# Proportion of variance 출력
plot(pca_dt, type = "l")

summary(pca_dt) # 값을 확인하여 주성분 요인을 선택 

 
train_tr <- cbind(dt_group_tr, as.data.frame(pca_dt$x)) 
 
train_tr[,1] <- as.factor(train_tr[,1])

colnames(train_tr)[1] <- "label" 


train_te <- cbind(dt_group_te, as.data.frame(pca_dt_te$x)) 

train_te[,1] <- as.factor(train_te[,1])

colnames(train_te)[1] <- "label" 


# check

lm_fit <- lm(label~PC1+PC2, data=train_tr)  # 주성분 분석을 통해 선택한 PC1과 PC2를 가지고 회귀분석 

str(lm_fit)

lm_fit_pred <- predict(lm_fit, newdata = train_tr) # the same dataset
str(lm_fit_pred)

b <- round(lm_fit_pred)

b[b==0 | b==1] <- "setosa"

b[b==2] <- "Versicolor"

b[b==3] <- "Virginica"

# table함수로 비교 
b

table(b, dt_group_tr)


# performance check with other dataset 

lm_fit_pred_te <- predict(lm_fit, newdata = train_te) # 모델의 예측력이 좋은지 확인위해 predict 함수로 예측
 

c <- round(lm_fit_pred_te)

c[c==0 | c==1] <- "setosa"

c[c==2] <- "Versicolor"
 
c[c==3] <- "Virginica"

# table함수로 비교 

table(c, dt_group_te)
b

## end of ## B


## error below ############################################################################

# 라이브러리 설치 및 불러오기
# github의 R 소스를 설치할 때, 다음과 같이 devtools 패키지를 설치하고, devtools::install_github를 사용한다.

# .libPaths()

install.packages("devtools")

require(devtools)

devtools::install_github('prestodb/RPresto') 
require(RPresto)

 
install_github("devtools")


install_github("ggbiplot", "vqv")

library(ggbiplot)
  
  
  # 시각화하기
  g <- ggbiplot(pca_dt,
                choices = c(1, 2),
                obs.scale = 1,
                var.scale = 1,
                groups = dt_group,
                ellipse = TRUE,
                circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

 ################################################################################################

## 회귀분석으로 확인 
# 회귀분석을 하기 위해 주성분을 처리 / 원래 데이터와 선형계수를 메트릭스 곱으로 선형조합에 맞게 만듬

m_pca_dt <- as.matrix(dt) %*% pca_dt$rotation


tr <- cbind(dt_group, as.data.frame(m_pca_dt))


str(m_pca_dt)

colnames(tr)[1]

tr[,1] <- as.factor(tr[,1])

colnames(tr)[1] <- "label"

head(tr)

  

#### 종속변수가 범주형이라 ??

fit <- lm(label ~ PC1+PC2, data=tr) 

str(fit)

summary(fit)

fit_pred <- predict(fit,newdata = tr)

## 

b <- round(fit_pred)



b[b==0 | b==1] <- "setosa"

b[b==2] <- "Versicolor"

b[b==3] <- "Virginica"


table(b,dt_group)

head(tr)


############### 종속변수가 3개 이상

ind <- sample(1:nrow(iris), nrow(iris)*0.7, replace = F)

tr <- iris[ind,] 
te <- iris[-ind,]

library(nnet)

 m <- multinom(Species ~. , data=tr)  # glm or multinom

 summary(m)
 
 
 pred <- predict(m, newdata=te, type='class')

 table(pred, te$Species)
 
 te

 ######################## 