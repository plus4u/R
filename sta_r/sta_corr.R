### correlation / 교차분석 / 적합도 분석 

# 1. 
aq <- airquality[,c(1:4)]


cor(aq)

aq2 <- na.omit(aq)

cor(aq2)

plot(aq2)

pairs(aq2, panel=panel.smooth)


# 2. 관리자 권한으로 실행하여 error 해결  install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)


chart.Correlation(aq2, histogram=TRUE, pch=19)


# 3. install.packages("corrplot")

library(corrplot)

aq.cor <- cor(aq2)
aq.cor


corrplot(aq.cor, method="number")


## 적합도 분석 
# 적합도 검정(goodness of fit test)은 k개의 범주 (혹은 계급)을 가지는 한 개의 요인(factor)에 대해서 어떤 이론적 분포를 따르고 있는지를 검정하는 방법입니다. 

# 기본 원리는, 도수분포의 각 구간에 있는 관측도수를 O1, O2, ..., Ok 라 하고, 각 범주 ( 혹은 계급)가 일어날 확률을 p1, p2, ..., pk 라고 할 때 기대되는 관측도수 E1, E2, ..., Ek 를 계산하여 실제 관측도수와 기대 관측도수의 차이를 카이제곱 검정 통계량(Chi-squared statistics)을 활용하여 가정한 확률모형에 적합한지를 평가하게 됩니다. 만약 귀무가설 H0가 맞다면 관측도수와 기대도수가 별 차이가 없을 것이므로 검정통계량 X0^2 값이 작을 것이며, 반대로 대립가설 H1이 맞다면 관측도수와 기대도수의 차이가 클 것이므로 검정통계량 X0^2 값이 커질 것입니다.



# 1. goodness of fit test : chisq.test()

obs <- c(19, 40, 35)

prob <- c(2/10, 3/10, 5/10)

chisq.test(obs, p=prob)

## R로 통계분석을 하면 콘솔 창에 보여지는 내용 말고도 실제로 다양한 통계량이 계산이 되어 list 형태로 
## 메모리상에 가지고 있으며 단지 눈에 보이지 않을 뿐인데, 
## indexing 기법을 활용하면 실행한 후에 다양한 통계량들을 선별해서 볼 수도 있고, 
## 다른 분석 혹은 애플리케이션에 input으로 넣어 재활용할 수도 있다.

# # chisq.test() of data from data frame

# chisq.test() of data from data frame

 data(Cars93, package="MASS")

Car_Type <- table(Cars93$Type)

Car_Type

# Compact   Large Midsize   Small  Sporty     Van 
# 16      11      22      21      14       9 

Car_Type_Prob <- c(0.2, 0.1, 0.2, 0.2, 0.2, 0.1)

 chisq.test(x=Car_Type, p=Car_Type_Prob)

 
 
 ## b. 독립성 검정(test of independence) ;  두 개의 범주형 변수/요인(2 factors)이 서로 연관성이 있는지, 
 # 상관이 있는지, 독립적인지를 카이제곱 검정(chisquared test)을 통해 통계적으로 판단하는 방법
 # 두 변수가 양적변수(qualitative variable)인 경우 두 변수 간 상관관계 분석을 위해서는 공분산 분석, 상관계수 분석, 회귀분석 등을 활용합니다.
 # 범주형 자료분석의 경우 두 변수의 관련성을 보려면 분할표를 만들어서 카이제곱 검정을 하게된다.
  
 # data key-in way 1 : rbind()
 
 row_1 <- c(7, 13, 9, 12)
 row_2 <- c(13, 21, 10, 19)
 row_3 <- c(11, 18, 12, 13)
  
data_rbind <- rbind(row_1, row_2, row_3)
data_rbind
 
# [,1] [,2] [,3] [,4]
# row_1    7   13    9   12
# row_2   13   21   10   19
# row_3   11   18   12   13

 
 # data key-in way 2 : matrix()
 raw_data <- c(7, 13, 9, 12, 13, 21, 10, 19, 11, 18, 12, 13)
dt <- matrix(raw_data, byrow=TRUE, nrow=3)
dt

# [,1] [,2] [,3] [,4]
# [1,]    7   13    9   12
# [2,]   13   21   10   19
# [3,]   11   18   12   13

# giving names to the rows and columns of the data table : dimnames()

dimnames(dt) # null

dimnames(dt) <- list("Class" = c("Class_1", "Class_2", "Class_3"), 
                                "Score" = c("Score_H", "Score_M", "Score_L", "Fail"))

dt
# Score
# Class     Score_H Score_M Score_L Fail
# Class_1       7      13       9   12
# Class_2      13      21      10   19
# Class_3      11      18      12   13

 
 ## exploratory data analysis
# marginal distribution : addmargins()
addmargins(dt) 
  
# proportional distribution : prop.table()
 
 prop.table(dt)
 
 # Score
 # Class        Score_H    Score_M    Score_L       Fail
 # Class_1 0.04430380 0.08227848 0.05696203 0.07594937
 # Class_2 0.08227848 0.13291139 0.06329114 0.12025316
 # Class_3 0.06962025 0.11392405 0.07594937 0.08227848
 

 addmargins(prop.table(dt))
 
 # bar plot : barplot()
 
barplot(t(dt), beside=TRUE, legend=TRUE, 
               ylim=c(0, 30), 
           ylab="Observed frequencies in sample", 
          main="Frequency of math score by class")
  
# indexing statistics of chisq.test()

fit <- chisq.test(dt)

fit$observed # observed frequency

fit$expected # expected frequeycy

fit$residuals # residual between observed and expected frequecy

fit$statistic # chi-squared statistics

fit$parameter # degrees of freedomfit

fit$p.value # P-value / 학급과 수학성적 간에는 서로 관련성이 없다. 독립적이다
 
 
 ## c. 동질성 검정 (test of homogeneity) : 관측값들이 정해진 범주 내에서 서로 비슷하게 나타나고 있는지를 검정. 속성 A, B를 가진 부모집단(subpopulation) 각각으로부터 정해진 표본의 크기만큼 자료를 추출하는 경우에 분할표에서 부모집단의 비율이 동일한가를 검정. 두 개의 요인을 대상으로 함.  

## r개의 행과 c개의 열을 가진 두 변수 X와 Y로부터 작성된 분할표의 각 열분포에서 행들이 균일한 값을 가지는지
## 즉, 각 열에서 행들의 동질성(homegeneity)를 검정하는 것으로, 독립성 검정과 차이점은 개념상의 차이일 뿐이며 검정을 하는 방법은 카이제곱 검정을 이용해서 동일합니다.
## 독립성 검정은 하나의 모집단에서 표본을 무작위로 추출한 후 추출된 표본을 두가지 속성(변수)에 따라 분류
## 하지만, 동질성 검정은 부모집단(subpopulation)을 먼저 설정한 후 각 부모집단으로부터 정해진 표본의 크기만큼
## 무작위로 추출하여 분할표에서 ** 부모집단의 비율이 동일한가를 검정하게 됩니다.  

# H0 : 성별로 정당 지지는 동일하다 
 
raw_data <- c(10, 20, 30, 10, 40, 30)
dt <- matrix(raw_data, byrow=TRUE, nrow=2)
dt

# giving names : dimnames()
dimnames(dt) <- list("Gender" = c("f", "m"), "par" = c("aa", "bb", "cc"))

## exploratory data analysis

 # marginal distribution : addmargins()
addmargins(dt)

# proportional distribution : prop.table()
 prop.table(dt)

 addmargins(prop.table(dt))
 
# bar plot : barplot()
barplot(t(dt), beside=TRUE, legend=TRUE, ylim=c(0, 120), 
        ylab="Observed frequencies ", 
        main="Parament preferences by gender")

## chisquared test : chisq.test()
chisq.test(dt)

# Pearson's Chi-squared test
# indexing statistics of chisq.test()
fit <- chisq.test(dt) 

fit$observed # observed frequency

fit$expected # expected frequeycy

fit$residuals # residual between observed and expected frequecy

fit$statistic # chi-squared statistics

fit$parameter # degrees of freedomfit
 
fit$p.value 

mode(dt)
