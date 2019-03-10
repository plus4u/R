### logistics regression

# ready : xtabs :  

d <- data.frame(x=c("1", "2", "2", "1"),
                y=c("A", "B", "A", "B"),
                num=c(3, 5, 8, 7))
d

xtabs(num ~ x + y, data=d)


# 1.  
library(ggplot2) 

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv") 


# 변수별 표준편차 확인 

sapply(mydata, sd) 

# contingency table : xtabs(~ x + y, data) 

xtabs(~admit+rank, data=mydata)


# factor 

mydata$rank <- factor(mydata$rank)

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

summary(mylogit)

 
## aod 패키지의 walt.test를 이용, rank의 overall effect를 파악 [b:회귀계수, Sigma:error term의 공분산행렬, Terms:rank변수가 있는 열] wald.test는 순서를 통해 해당 범주형 변수의 위치를 파악하기 때문에 순서를 잘 신경 써야 한다.

# install.packages("aod")
library(aod)
 
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

 # OR의 증분 : gpa가 1증가하면 admission의 non-admission에 대한 OR이 2.23배 증가  

exp(coef(mylogit))

# (Intercept)         gre         gpa       rank2       rank3       rank4 
# 0.0185001   1.0022670   2.2345448   0.5089310   0.2617923   0.2119375 


## 예측하기:  타 변수들을 고정하고 rank가 변할 때 예측값의 변화를 보기

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

newdata1

# rank, gpa를 고정한 후, gre의 효과 보기

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
newdata2

## newdata3의 예측값 시각화하기



# rank를 고정시켜놓고 gre가 admission 확률에 어떤 영향을 주는지를 시각화 

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link", 
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
 
ggplot(newdata3, aes(x = gre, y = PredictedProb),
       geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank),alpha = 0.2), geom_line(aes(colour = rank)))

ggplot(newdata3, aes(x = gre, y = PredictedProb))

##      https://stats.idre.ucla.edu/r/dae/logit-regression/
                                                                                                                      