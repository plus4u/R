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


# ������ ǥ������ Ȯ�� 

sapply(mydata, sd) 

# contingency table : xtabs(~ x + y, data) 

xtabs(~admit+rank, data=mydata)


# factor 

mydata$rank <- factor(mydata$rank)

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

summary(mylogit)

 
## aod ��Ű���� walt.test�� �̿�, rank�� overall effect�� �ľ� [b:ȸ�Ͱ��, Sigma:error term�� ���л����, Terms:rank������ �ִ� ��] wald.test�� ������ ���� �ش� ������ ������ ��ġ�� �ľ��ϱ� ������ ������ �� �Ű� ��� �Ѵ�.

# install.packages("aod")
library(aod)
 
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

 # OR�� ���� : gpa�� 1�����ϸ� admission�� non-admission�� ���� OR�� 2.23�� ����  

exp(coef(mylogit))

# (Intercept)         gre         gpa       rank2       rank3       rank4 
# 0.0185001   1.0022670   2.2345448   0.5089310   0.2617923   0.2119375 


## �����ϱ�:  Ÿ �������� �����ϰ� rank�� ���� �� �������� ��ȭ�� ����

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

newdata1

# rank, gpa�� ������ ��, gre�� ȿ�� ����

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
newdata2

## newdata3�� ������ �ð�ȭ�ϱ�



# rank�� �������ѳ��� gre�� admission Ȯ���� � ������ �ִ����� �ð�ȭ 

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
                                                                                                                      