## linear model, multicollinearity 


# 1.  linear model

x1 <- (58:72) # weight 
x2 <- c(115,117,120,123,126,129,132,135,139,142,146,150,154,159,164) # height 

fit <- lm(x2~x1)
# fit <- lm(x2~x1, data=mydata)
summary(fit)

plot(x2~x1)
abline(fit,col="blue")

install.packages("lm.beta")
library(lm.beta)

lm.beta(fit)  # excel : 


# 2. Polynomial regression


mydata <- data.frame(x1, x2, row.names = T)

str(mydata)

fit=lm(x1~x2+I(x2^2), data=mydata)

summary(fit)

plot(x2~x1,data=mydata)

lines(mydata$x2,fitted(fit),col="blue")


# 3. multicollinearity 

state.x77

head(state.x77)

colnames(state.x77)

# new data frame

states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])

head(states)

# 1. linear regression

fit <- lm(Murder~Population+Illiteracy+Income+Frost,data = states) 

fit

# the same method

fit <- lm(Murder~.,data = states) 

fit


plot(Murder ~., data = states)

# 2. plot check

plot(fit)

# 3. summary - p-value check  

summary(fit)


# 4. lm with significant variables & summary  > all is significant 

fit2 <- lm(Murder~Population + Illiteracy, data = states)

summary(fit2)

# 5. dummy variables

rich <- NA
rich
tmp <- cbind(states,rich)
head(tmp)
summary(tmp$Income) 


# 6. rich =1 / is.na  & as.factor 

tmp$rich[tmp$Income>4436] <- 1
tmp$rich[is.na(tmp$rich)] <- 0

head(tmp)

str(tmp)

tmp$rich <- as.factor(tmp$rich)

# 7. lm with dummy

fit <- lm(Murder~.,data = tmp) 
summary(fit)

## 8. multicollinearity >> near -1 or 1 

library(psych)
library(car)

pairs.panels(tmp[names(tmp)])   ## 

# 9. Variance Inflation Factor) = 1/ (1-r^2)
car::vif(fit) # = vif(fit)

# 10. vif() > 10  or sqrt(car::vif(fit)) > 2 

vif(fit) > 10

# sqrt(car::vif(fit)) > 2    # square root

## method to select variable 


# 11. forward

fit.con <- lm(Murder~1,data=tmp) 
fit.con

fit.forward <- step(fit.con,scope=list(lower=fit.con,upper=fit),
                    direction = "forward") 

# 12. display of variables selected  >> illiteracy , population

fit.forward 


# 13. backward = eliminate one by one

fit.backward <- step(fit,scope=list(lower=fit.con,upper=fit),
                     direction = "backward")

fit.backward   # 4 variables are significant , but 

# 14. summary  >> final verification >> 2 is 

summary(fit.backward)


# 15. stepwise


fit.both <- step(fit.con,scope=list(lower=fit.con,upper=fit), direction = "both")

fit.both

summary(fit.both)

## 16. final prediction with new model

pre_murder <- predict(fit.both, newdata = tmp) 
pre_murder <- as.data.frame(pre_murder)
pre_murder 

# 17. interval estimation

pre_murder <- predict(fit.both, newdata = tmp, interval = "confidence") 
pre_murder <- as.data.frame(pre_murder)
pre_murder


## 18. compare prediction to real data (tmp$Murder)

ttmp <- cbind(pre_murder,tmp$Murder) 

ttmp


### 19. 회귀모형의 적절성 평가 방법 

fit=lm(Murder~Population+Illiteracy+Income+Frost,data=states)

confint(fit) # 회귀계수의 95% 신뢰구간

## 결과 해석 : 문맹률(Illiteracy)이 1% 변하면 인구 10만 명당 살인사건 발생률이 4.14(2.38에서 5.90) 변화한다고 95%의 확신을 가지고 이야기 할 수 있으며, 1 년중 기온이 0도 이하로 떨어지는 Frost의 경우 95% 신뢰구간이 0을 지나므로 다른 변수들이 일정하다면 온도의 변화는 살인사건의 발생률과 관계가 없다고 결론내릴 수 있다. 


## 20. 화면을 4개로 나누고
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

## 정규성, 독립성, 선형성, 등분산성

qqPlot(fit,labels=row.names(states),id.method="identify",simulate=TRUE,main="Q-Q_ plot")

