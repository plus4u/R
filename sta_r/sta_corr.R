### correlation / �����м� / ���յ� �м� 

# 1. 
aq <- airquality[,c(1:4)]


cor(aq)

aq2 <- na.omit(aq)

cor(aq2)

plot(aq2)

pairs(aq2, panel=panel.smooth)


# 2. ������ �������� �����Ͽ� error �ذ�  install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)


chart.Correlation(aq2, histogram=TRUE, pch=19)


# 3. install.packages("corrplot")

library(corrplot)

aq.cor <- cor(aq2)
aq.cor


corrplot(aq.cor, method="number")


## ���յ� �м� 
# ���յ� ����(goodness of fit test)�� k���� ���� (Ȥ�� ���)�� ������ �� ���� ����(factor)�� ���ؼ� � �̷��� ������ ������ �ִ����� �����ϴ� ����Դϴ�. 

# �⺻ ������, ���������� �� ������ �ִ� ���������� O1, O2, ..., Ok �� �ϰ�, �� ���� ( Ȥ�� ���)�� �Ͼ Ȯ���� p1, p2, ..., pk ��� �� �� ���Ǵ� �������� E1, E2, ..., Ek �� ����Ͽ� ���� ���������� ��� ���������� ���̸� ī������ ���� ��跮(Chi-squared statistics)�� Ȱ���Ͽ� ������ Ȯ�������� ���������� ���ϰ� �˴ϴ�. ���� �͹����� H0�� �´ٸ� ���������� ��뵵���� �� ���̰� ���� ���̹Ƿ� ������跮 X0^2 ���� ���� ���̸�, �ݴ�� �븳���� H1�� �´ٸ� ���������� ��뵵���� ���̰� Ŭ ���̹Ƿ� ������跮 X0^2 ���� Ŀ�� ���Դϴ�.



# 1. goodness of fit test : chisq.test()

obs <- c(19, 40, 35)

prob <- c(2/10, 3/10, 5/10)

chisq.test(obs, p=prob)

## R�� ���м��� �ϸ� �ܼ� â�� �������� ���� ������ ������ �پ��� ��跮�� ����� �Ǿ� list ���·� 
## �޸𸮻� ������ ������ ���� ���� ������ ���� ���ε�, 
## indexing ����� Ȱ���ϸ� ������ �Ŀ� �پ��� ��跮���� �����ؼ� �� ���� �ְ�, 
## �ٸ� �м� Ȥ�� ���ø����̼ǿ� input���� �־� ��Ȱ���� ���� �ִ�.

# # chisq.test() of data from data frame

# chisq.test() of data from data frame

 data(Cars93, package="MASS")

Car_Type <- table(Cars93$Type)

Car_Type

# Compact   Large Midsize   Small  Sporty     Van 
# 16      11      22      21      14       9 

Car_Type_Prob <- c(0.2, 0.1, 0.2, 0.2, 0.2, 0.1)

 chisq.test(x=Car_Type, p=Car_Type_Prob)

 
 
 ## b. ������ ����(test of independence) ;  �� ���� ������ ����/����(2 factors)�� ���� �������� �ִ���, 
 # ����� �ִ���, ������������ ī������ ����(chisquared test)�� ���� ��������� �Ǵ��ϴ� ���
 # �� ������ ��������(qualitative variable)�� ��� �� ���� �� ������� �м��� ���ؼ��� ���л� �м�, ������ �м�, ȸ�ͺм� ���� Ȱ���մϴ�.
 # ������ �ڷ�м��� ��� �� ������ ���ü��� ������ ����ǥ�� ���� ī������ ������ �ϰԵȴ�.
  
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

fit$p.value # P-value / �бް� ���м��� ������ ���� ���ü��� ����. �������̴�
 
 
 ## c. ������ ���� (test of homogeneity) : ���������� ������ ���� ������ ���� ����ϰ� ��Ÿ���� �ִ����� ����. �Ӽ� A, B�� ���� �θ�����(subpopulation) �������κ��� ������ ǥ���� ũ�⸸ŭ �ڷḦ �����ϴ� ��쿡 ����ǥ���� �θ������� ������ �����Ѱ��� ����. �� ���� ������ ������� ��.  

## r���� ��� c���� ���� ���� �� ���� X�� Y�κ��� �ۼ��� ����ǥ�� �� ���������� ����� ������ ���� ��������
## ��, �� ������ ����� ������(homegeneity)�� �����ϴ� ������, ������ ������ �������� ������� ������ ���̸� ������ �ϴ� ����� ī������ ������ �̿��ؼ� �����մϴ�.
## ������ ������ �ϳ��� �����ܿ��� ǥ���� �������� ������ �� ����� ǥ���� �ΰ��� �Ӽ�(����)�� ���� �з�
## ������, ������ ������ �θ�����(subpopulation)�� ���� ������ �� �� �θ��������κ��� ������ ǥ���� ũ�⸸ŭ
## �������� �����Ͽ� ����ǥ���� ** �θ������� ������ �����Ѱ��� �����ϰ� �˴ϴ�.  

# H0 : ������ ���� ������ �����ϴ� 
 
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