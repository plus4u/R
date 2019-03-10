### 주요한 library 
# https://statkclee.github.io/data-science/data-science-library.html


### 판별분석

# 1. dataset 생성

set.seed(123)

aa_length <- sample(seq(15,22.5,by=0.5), 50, replace=T)
aa_weight <- sample(seq(0.2,0.8,by=0.05), 50, replace=T)
bb_length <- sample(seq(46,61,by=0.5), 50, replace=T)
bb_weight <- sample(seq(1.36,3.2,by=0.5), 50, replace=T)
cc_length <- sample(seq(30,75.5,by=1), 50, replace=T)
cc_weight <- sample(seq(0.2,3.5,by=0.1), 50, replace=T)
dd_length <- sample(seq(25,38,by=0.5), 50, replace=T)
dd_weight <- sample(seq(0.4,0.54,by=0.01), 50, replace=T)
ee_length <- sample(seq(22,55,by=0.5), 50, replace=T)
ee_weight <- sample(seq(0.68,1.8,by=0.01), 50, replace=T)

length <- c(aa_length, bb_length, cc_length, dd_length, ee_length)
weight <- c(aa_weight, bb_weight, cc_weight, dd_weight, ee_weight)
speed <- rnorm(50*5, 7.2, sd=1.8)
fish <- c(rep("aa",50), rep("bb", 50), rep("cc", 50), rep("dd", 50), rep("ee", 50))
fish_data <- data.frame(length, weight, speed, fish)

## 2. lda 

library(MASS)
str(fish_data)

fish_lda <- lda(fish ~., data=fish_data, prior=c(1,1,1,1,1)/5)
fish.lda

# 

fish.lda$counts


# 새로운 데이터셋 분류 결과 및 클래스별 사후확률: predict() 함수

# fish_data에 100마리를 선택해 선형판별방법을 학습(train)


set.seed(123)
train100 <- sample(1:nrow(fish_data),100)
table(fish_data$fish[train100])



fish100_lda <- lda(fish ~., data=fish_data, prior=c(1,1,1,1,1)/5, subset=train100)


## predict() 함수를 사용해 표본에 포함된 100마리에 대한 분류 결과에 대한 정보를 얻어보자.

predict_fish100 <- predict(fish100_lda)
table(fish_data$fish[train100], predict_fish100$class)


# ggplot() 함수를 가지고 시각화하자.
library(ggplot2)

p <- ggplot(as.data.frame(predict_fish100$x), aes(x=LD1, y=LD2, col-fish_data$fish[train100]))
p <- p + geom_point() + geom_text(aes(label=as.character(predict_fish100$class)))
# Adjust legend size
p <- p + theme(legend.title=element_blank(), legend.text=element_text(size=20, face="bold"))
# Adjust axis labels
p <- p + theme(axis.title=element_text(face="bold", size=20), axis.text=element_text(size=18))
# Display plot
p


## 분류결과(class), 사후확률(posterior), 오분류율(misclassficaiotn rate)를 계산해보자.
predict_new <- predict(fish100_lda, newdata=fish_data[-train100,])
table(fish_data$fish[-train100], predict_new$class)


## 

tab <- table(fish_data$fish[-train100], predict_new$class)
tab

## error rate 

tab_sum <- 1-sum(diag(tab))/sum(tab)
tab_sum

sum(diag(tab))
sum(tab)





