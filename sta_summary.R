## summary

# 1. t.test

x1 <- rnorm(30, mean = 5, sd=1)
x2 <- rnorm(30, mean = 5.1, sd=1)
a <- data.frame(x1)
a$factor="a"
b <- data.frame(x2)
b$factor="b"


ab <- cbind(score=as.numeric(c(x1,x2)), factor=c("a","b"))  # *
ab <- data.frame(score=c(x1,x2), factor=c("a","b"))  # *
ab
str(ab)
 

x3 <- sample(1:200, size = 30, replace = T)

##
qqnorm(x3)
qqline(x3, col=2)

shapiro.test(x3)

shapiro.test(x1)
# kolmogorov-smirnov
ks.test(x1, "pnorm", mean(x1), sd(x1))


##

library(car)
 

leveneTest(score~ factor, ab)
bartlett.test(score~ factor, ab)

# levene.test(x1,x2) #install.packages("lawstat")


## 2. anova

x1 <- rnorm(10, mean = 5, sd=1)
x2 <- rnorm(10, mean = 5.1, sd=1)
x3 <- rnorm(10, mean = 7, sd=1)

xx <- data.frame(score=c(x1, x2, x3), factor=c(rep("a", 10),rep("b",10),rep("c",10)))

 
# xx[order(xx$factor),]

# names(x3)[1]="score"

# 
fit <- aov(score ~ factor, xx)

summary(fit) # summary.aov(fit)

TukeyHSD(fit, which = "factor")

summary (fit1 <- aov(score ~ factor, data = xx))
TukeyHSD(fit1, "factor", ordered = TRUE)


## 3. pca / fa

#

xx=c(0,1,2,1,2,3)
yy=c(0,1,1,2,2,3)
zz=data.frame(xx,yy)
rownames(zz) = c("A","B","C","D","E","F")

#

ggplot(data=zz, mapping=aes(x=xx,y=yy)) + 
    geom_point() +
    geom_text(aes(label=rownames(zz), vjust=1.1,hjust=2), check_overlap = T) +  # 알파벳 텍스트 추가
    geom_line(data=zz[c(1,2,5,6),], mapping=aes(x=xx,y=yy), color='red') +  # 빨간색 대각선 추가
    guides(fill="none") 


zz_cov = cov(zz)
zz_cov

zz_eigen = eigen(zz_cov)
zz_eigen


zz_eigen$vectors %*% diag(zz_eigen$values) %*% t(zz_eigen$vectors)



# 

zz_pca = princomp(zz, cor=F)

zz_pca$loading


# validate 

cbind(xx-mean(xx), yy-mean(yy)) %*% zz_eigen$vectors



## cbind(xx-mean(xx), yy-mean(yy)) %*% zz_pca$loading도 동일한 결과 산출

zz_pca_scores=data.frame(zz_pca$scores)
 colnames(zz_pca_scores) = c("pca1","pca2")
rownames(zz_pca_scores) = c("A","B","C","D","E","F")

ggplot(data=zz_pca_scores, mapping=aes(x=pca1, y=pca2)) + 
    geom_point() +
    geom_text(aes(label=rownames(zz_pca_scores), vjust=1.1,hjust=2), check_overlap = T) +  # 알파벳 텍스트 추가
    geom_line(data=zz_pca_scores[c(1,2,5,6),], mapping=aes(x=pca1,y=pca2), color='green') +  # 빨간색 대각선 추가
    guides(fill="none")  # 범례 미표시


var(zz_pca_scores)

##

x <- matrix(1:9,3,3)

x1 <- cor(x)
x2 <- cov(x)
x1
x2

x1_e <- eigen(x1)
x2_e <- eigen(x2)
 

x1_e$vectors %*% diag(x1_e$values) %*% t(x1_e$vectors)  # 
x2_e$vectors %*% diag(x2_e$values) %*% t(x2_e$vectors)

x
x[4, ] <- c(4,4,4)  # error

x <- rbind(x, c(4,4,4))
x <- cbind(x, c(4,7,8, 9))

x1 <- cor(x)
x2 <- cov(x)
x1
x2

x1_e <- eigen(x1)
x2_e <- eigen(x2)
x_e <- eigen(x)
x_e 

x1_e$vectors %*% diag(x1_e$values) %*% t(x1_e$vectors)  # the same with cor 
x2_e$vectors %*% diag(x2_e$values) %*% t(x2_e$vectors)  # the same with cov
x_e$vectors %*% diag(x_e$values) %*% t(x_e$vectors)


## 4. 교차분석

setwd ()
read.csv()

result <- data.frame(level = , pass = )

table(result)

# library (gmodel) 
CrossTable(result$level, result$pass)
 
colSums(is.na(zz))

## 5. correlation

str(iris)

cor(iris$Sepal.Length, iris$Sepal.Width, method = "pearson" )
cor(iris$Sepal.Length, iris$Sepal.Width, method = "spearman" )



## 6. linear regression

attach(iris) iris <- iris[, -5]

attach(iris)
iris[, -5] 

lm(Sepal.Length ~ Sepal.Width)  # 1

res <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width)  

res
str(res)
t <- names(res)

t

aov(res)  # need summary() for p-value
summary(aov(res))

anova(res) #2  :  equal to aov

res


res$coef

deviance(res)


vcov(res) 


confint(res)


summary(res)

 
# Call:


# Residuals: 이상적으로 회귀식의 잔차는 정규분포를 따른다.

# Coefficients: Estimate열에는 OLS법으로 계산한 회귀 계수들의 추정치, Std. ERROR열은 추정된 계수의 표준오차

# Residual standard error:     잔차의 표준오차, 즉 잔차의 표본 표준편차

# Multiple R^2, Adjusted R^2: R^2은 회귀 모형에 의해 설명이 되는 종속변수의 변동량의 비율, 변수가 많을시에는 오버피팅문제가 발생할 수 있는데 회귀분석의 경우 결정계수를 구하는 과정에서 변수가 증가하면 결정계수가 커지게된다. 변수만 많이 집어넣으면 설명력이 높아지는 결과를 갖고온다. 이를 수정해주기위해 변수의 개수도 고려한 Adj R^2를 사용

# 절편이 없는 선형회귀


lm(Sepal.Length ~ Sepal.Width + 0)

plot(res)

abline(res)

# 상호작용이 있는 선형회귀


lm(Sepal.Length ~ Sepal.Width + Petal.Length * Petal.Width)


lm(Sepal.Length ~ Sepal.Width + (Petal.Length + Petal.Width)^2)


# 후진 단계별 회귀(Backward stepwise regression) 후진 제거법(Backward elimination) 으로 많이 알려져 있다.

attach(quakes) res2 <- lm(mag ~ lat + long + depth + stations) res3 <- lm(mag ~ 1)

 
step(res2, direction = "backward")


# 전진 단계별 회귀(Forward stepwise regression)

step(res3, direction = "forward", scope=(~lat+long+depth+stations))

# 단계적 회귀(Stepwise regression)

step(res3, direction = "both", scope=(~lat+long+depth+stations))

# 부분집합에 대해 선형모형을 적합시킬 때

lm(mag ~ lat +long , subset = 1:100)

# 다항식 회귀분석
lm(mag ~ lat+long)


lm(mag ~ I(lat+long)) # y = b0 + b1*(x1 + x2)


# 최적의 거듭제곱 변환 찾기

library("MASS") boxcox <- boxcox(lm_iris)

lambda <- boxcox$x[which.max(boxcox$y)] bc_iris <- lm(I(Sepal.Length^lambda) ~ Petal.Length) summary(bc_iris)


# 선형회귀 진단하기
par(mfrow = c(2,2)) 
plot(res) 
library(car)

outlierTest(res)

par(mfrow = c(1,1))


# 영향력이 큰 변수
influence.measures(lm_iris)

# 잔차의 자기상관 D-W test

durbinWatsonTest(res)


# 예측
df <- data.frame(Sepal.Width = c(5, 6, 7),
                 Petal.Length = c(1, 2, 3),
                 Petal.Width = c(0.5, 1, 1.5))
predict(res, df)


## 6. plot 
library(ggplot2) 
library(dplyr)  # 가장 최신 데이터프레임 조작 팩키지

ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
    geom_point()

# 그래프 플롯 생성
surveys_plot <- ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length))

# 그래프 화면 출력
surveys_plot + geom_point()

str(iris)

library(ggplot2)
ggplot(data = iris[1:10,], aes(x = Petal.Width, y = Sepal.Length+Sepal.Width)) +
    geom_point(alpha = 0.1, aes(color=Species))

head(iris,10)


# 산점도 / 산포도 geom_point( )

#선 그래프 geom_line( )

# 박스플롯 geom_boxplot( )

# 히스토그램 geom_histogram( )

# 막대 그래프 geom_bar( )





# ggplot은 ggplot( ) + geom_point( ) #와 같이 + 기호로 연결해 구문을 작성

ggplot(data = iris,
       mapping = aes(x = Sepal.Length,
                     y = Sepal.Width)) + geom_point(colour = "red",
                                                    pch = 2,
                                                    size = 2)
ggplot(data = iris,
       mapping = aes(x = Sepal.Length,
                     y = Sepal.Width)) + geom_point(colour = c("purple", "blue", "green")[iris$Species],
                                                    pch = c(0, 2, 20)[iris$Species],
                                                    size = c(1, 1.5, 2)[iris$Species])


 
levels(iris$Species)
 
# colour = "색상"
# pch = 모양
# size = 크기

 # annotate( ) 함수는 geom 옵션으로 어떤 도형을 그릴 것인지를 결정하고 그에 따라서 세부 시각화 옵션
 
g + annotate(geom = "rect",
             xmin = start_x,
             xmax = end_x,
             ymin = start_y,
             ymax = end_y,
             fill = "red",
             alpha = 0.2,
             colour = "black",
             lty = 2)

# geom = "도형종류"
# xmin = 왼쪽 아래 x좌표
# xmax = 오른쪽 위 x좌표
# ymin = 왼쪽 아래 y좌표
# ymax = 오른쪽 위 y좌표
# fill = "채우기 색"
# alpha = 투명도
# colour = "선색"
# lty = 선종류

 
## plot( y ~ x) vs plot (x, y) + with(dataname, plot(x,y))

g <- lm(Sepal.Length ~ Sepal.Width, data = iris)
str(g)
g
plot(Sepal.Length ~ Sepal.Width, data = iris)
abline(g)


## 7. logistics 

setwd("C:/R/work_r/source")


#UCLA 데이터

# data <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

# head(data,10)



# 지원자의 학교 등급을 나타내는 rank변수는 범주형이므로 factor처리 - 안하면 ? 
data$rank <-as.factor(data$rank)

# 

model <- glm(admit ~ gre + gpa + rank, data =train, family = "binomial")  

summary(model)








## clustering




## discriminant



## non-parametric 










## lda


library(ISLR)



