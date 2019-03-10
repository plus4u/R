
getwd()

setwd("C:/Users/beomc/OneDrive/바탕 화면/SPSS_DATA")


data <- read.csv(file = "drama_genre.csv")

# 데이터 필터링

data <- subset(data, genre %in% c("막장드라마", "멜로드라마"))

# factor의 레벨을 3에서 2로 바꾸어 주기 위함줄여주기 위함
data$genre <- as.factor(as.character(data$genre))

# 색 투명도 설정을 위한 alpha 함수를 사용하기 위해 scales 패키지를 설치하고 라이브러리 불러오기
install.packages("scales")
library(scales)

#1. 산점도 그리기
plot(formula = sum_age_mainactors ~ avg_slap_face,
     data = data,
     col = alpha(c("blue", "green"), 0.8)[data$genre],
     xlab = "회당 뺨 맞는 횟수",
     ylab = "주연배우 나이 합계",
     main = "드라마 장르 분포")

# 범례 그리기
legend("topleft",
       legend = levels(data$genre),
       pch = 1,
       col = alpha(c("blue", "green"), 0.8),
       cex = 0.9,
       bty = "n")



# 2.  재현성을 위한 seed 설정

set.seed(9876)

# idx 설정 (6 : 4)
idx <- sample(x = c("train_valid", "test"),
              size = nrow(data),
              replace = TRUE,
              prob = c(6, 4))

# idx에 따라 데이터 나누기
train_valid <- data[idx == "train_valid", ]
test <- data[idx == "test", ]

# test 데이터 설명변수/반응변수 나누기
test_x <- test[, -3]
test_y <- test[, 3]


# alpha 함수를 사용하려면 미리 scales 라이브러리를 불러와야 한다.
library(scales)

# train_valid 산점도 그리기
plot(formula = sum_age_mainactors ~ avg_slap_face,
     data = train_valid,
     col = alpha(c("blue", "green"), 0.8)[train_valid$genre],
     pch = 0,
     main = "드라마 장르 분포",
     xlab = "회당 뺨 맞는 횟수",
     ylab = "주연배우 나이 합계")

# test 데이터 표시하기
points(formula = sum_age_mainactors ~ avg_slap_face,
       data = test,
       pch = 16,
       cex = 1.2,
       col = alpha(c("blue", "green"), 0.5)[test$genre])

# 범례 그리기
legend("topleft",
       c(paste0("train_valid ", levels(train_valid$genre)), paste0("test ", levels(test$genre))),
       pch = c(0, 0, 16, 16),
       col = c(alpha(c("blue", "green"), 0.8), alpha(c("blue", "green"), 0.5)),
       cex = 0.9,
       bty = "n")
 

##

result <- data.frame(fold = rep(c(1, 2, 3), each = 82),
                     mdl = rep(c("full", "step"), 41),
                     i = rep(seq(0, 1, length.out = 41), 6),
                     accuracy = rep(rep(NA, 41), 6))


## 

idx <- sample(x = c(1:3), size = nrow(data), replace = TRUE, prob = c(1, 1, 1))

for(k in c(1, 2, 3)){
  
  # idx에 따라 train vs. valid 데이터 나누기
  valid <- train_valid[idx == k, ]
  train <- train_valid[idx != k, ]
  
  # valid 데이터 설명변수/반응변수 나누기
  valid_x <- valid[, -3]
  valid_y <- valid[, 3]
}
  
  
 ### 3. 
# 로지스틱 회귀분석 모델 생성

full <- glm(formula = genre ~ .,
            data = train,
            family = "binomial")

# Stepwise
step <- step(object = full,
             trace = F)

# full과 step 모델별 확률 예측
full_pred_p <- as.numeric(predict(object = full,
                                  newdata = valid_x,
                                  type = "response"))
step_pred_p <- as.numeric(predict(object = step,
                                  newdata = valid_x,
                                  type = "response"))

# # 4. 분류 정확도의 분모


l <- length(valid_y)

for(i in unlist(unique(result$i))){
  
  # i를 기준으로 0 또는 1로 분류
  full_pred_class <- ifelse(full_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
  step_pred_class <- ifelse(step_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
  
  # 분류 정확도 계산
  full_accuracy <- sum(full_pred_class == valid_y) / l
  step_accuracy <- sum(step_pred_class == valid_y) / l
  
  # result 테이블에 분류 정확도 입력
  result[result$fold == k
         & result$mdl == "full"
         & result$i == i, "accuracy"] <- full_accuracy
  result[result$fold == k
         & result$mdl == "step"
         & result$i == i, "accuracy"] <- step_accuracy
}

 ##

# Stepwise
step <- step(object = full,
             trace = F)

 


# full과 step 모델별 확률 예측
full_pred_p <- as.numeric(predict(object = full,
                                  newdata = valid_x,
                                  type = "response"))
step_pred_p <- as.numeric(predict(object = step,
                                  newdata = valid_x,
                                  type = "response"))

 
# 분류 정확도의 분모
l <- length(valid_y)

for(i in unlist(unique(result$i))){
  
  # i를 기준으로 0 또는 1로 분류
  full_pred_class <- ifelse(full_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
  step_pred_class <- ifelse(step_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
  
  # 분류 정확도 계산
  full_accuracy <- sum(full_pred_class == valid_y) / l
  step_accuracy <- sum(step_pred_class == valid_y) / l
  
  # result 테이블에 분류 정확도 입력
  result[result$fold == k
         & result$mdl == "full"
         & result$i == i, "accuracy"] <- full_accuracy
  result[result$fold == k
         & result$mdl == "step"
         & result$i == i, "accuracy"] <- step_accuracy
}

##

# full 모델 k-Fold = 1
plot(accuracy ~ i,
     result[result$mdl == "full" & result$fold == 1, ],
     type = "l",
     col = alpha("purple", 0.4),
     ylim = c(0.3, 1),
     xlab = "임계치",
     ylab = "분류 정확도",
     main = "분류 정확도 in full/step 3-Fold CV")

# full 모델 k-Fold = 2
lines(accuracy ~ i,
      result[result$mdl == "full" & result$fold == 2, ],
      col = alpha("orange", 0.4))

# full 모델 k-Fold = 3
lines(accuracy ~ i,
      result[result$mdl == "full" & result$fold == 3, ],
      col = alpha("green", 0.4))

# step 모델 k-Fold = 1
lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 1, ],
      col = alpha("purple", 0.5),
      lty = 2)

# step 모델 k-Fold = 2
lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 2, ],
      col = alpha("orange", 0.5),
      lty = 2)

# step 모델 k-Fold = 3
lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 3, ],
      col = alpha("green", 0.5),
      lty = 2)

# 범례 그리기
legend("topleft",
       c("full k=1", "full k=2", "full k=3",
         "step k=1", "step k=2", "step k=3"),
       col = c(alpha(c("purple", "orange", "green"), 0.4),
               alpha(c("purple", "orange", "green"), 0.5)),
       lty = rep(c(1, 2), each = 3),
       bty = "n",
       cex = 0.9)


##

# 그룹핑을 위해 plyr 패키지를 설치하고 라이브러리 불러오기
install.packages("plyr")
library(plyr)

# 모델별 임계치별 평균 분류 정확도 계산하기
tmp <- ddply(result, .(mdl, i), summarise, avg_accuracy = mean(accuracy))


# full 모델
lines(avg_accuracy ~ i,
      tmp[tmp$mdl == "full", ],
      col = alpha("red", 0.7),
      lty = 1,
      type = "o",
      pch = 20)

# step 모델
lines(avg_accuracy ~ i,
      tmp[tmp$mdl == "step", ],
      col = alpha("red", 0.7),
      lty = 2,
      type = "o",
      pch = 20)

# <span style="font-size: 14.6667px;">
  
  # 범례 그리기
  legend("topright",
         c("full avg accuracy", "step avg accuracy"),
         pch = 20,
         col = alpha("red", 0.7),
         lty = rep(c(1, 2)),
         bty = "n",
         cex = 0.9)
  # </span>
  
  
  tmp[tmp$avg_accuracy == max(tmp$avg_accuracy), ]
  
   
  # test_x가 멜로드라마일 확률을 step 모델로 구하기
  test_p <- as.numeric(predict(object = step,
                               newdata = test_x,
                               type = "response"))
  
  # 임계치 0.45를 기준으로 막장드라마와 멜로드라마 분류하기
  test_class <- ifelse(test_p < 0.45, levels(data$genre)[1], levels(data$genre)[2])
  
  # 분류 정확도 계산하기
  sum(test_class == test_y) / length(test_y)
  
  
  ##
  
  # 정답/오답 만들기 (pch 옵션을 위해)
  ox <- as.factor(ifelse(test_class == test_y, "O", "X"))
  
  # train_valid 산점도 그리기
  plot(formula = sum_age_mainactors ~ avg_slap_face,
       data = train_valid,
       col = alpha(c("blue", "green"), 0.8)[train_valid$genre],
       xlab = "회당 뺨 맞는 횟수",
       ylab = "주연배우 나이 합계",
       main = "드라마 장르 분포")
  
  # test 산점도 그리기
  points(formula = sum_age_mainactors ~ avg_slap_face,
         data = test,
         col = alpha(c("blue", "green"), 0.6)[test$genre],
         pch = c(19, 17)[ox])
  
  # 범례 그리기
  legend("topleft",
         legend = c(paste0("train_valid ", levels(train_valid$genre)),
                    paste0("test ", levels(test$genre), " 정답"),
                    paste0("test ", levels(test$genre), " 오답")),
         pch = c(1, 1, 19, 19, 17, 17),
         col = c(alpha(c("blue", "green"), 0.8),
                 alpha(c("blue", "green"), 0.6),
                 alpha(c("blue", "green"), 0.6)),
         cex = 0.9,
         bty = "n")
  
   