
getwd()

setwd("C:/Users/beomc/OneDrive/���� ȭ��/SPSS_DATA")


data <- read.csv(file = "drama_genre.csv")

# ������ ���͸�

data <- subset(data, genre %in% c("������", "��ε��"))

# factor�� ������ 3���� 2�� �ٲپ� �ֱ� �����ٿ��ֱ� ����
data$genre <- as.factor(as.character(data$genre))

# �� ������ ������ ���� alpha �Լ��� ����ϱ� ���� scales ��Ű���� ��ġ�ϰ� ���̺귯�� �ҷ�����
install.packages("scales")
library(scales)

#1. ������ �׸���
plot(formula = sum_age_mainactors ~ avg_slap_face,
     data = data,
     col = alpha(c("blue", "green"), 0.8)[data$genre],
     xlab = "ȸ�� �� �´� Ƚ��",
     ylab = "�ֿ���� ���� �հ�",
     main = "��� �帣 ����")

# ���� �׸���
legend("topleft",
       legend = levels(data$genre),
       pch = 1,
       col = alpha(c("blue", "green"), 0.8),
       cex = 0.9,
       bty = "n")



# 2.  �������� ���� seed ����

set.seed(9876)

# idx ���� (6 : 4)
idx <- sample(x = c("train_valid", "test"),
              size = nrow(data),
              replace = TRUE,
              prob = c(6, 4))

# idx�� ���� ������ ������
train_valid <- data[idx == "train_valid", ]
test <- data[idx == "test", ]

# test ������ ��������/�������� ������
test_x <- test[, -3]
test_y <- test[, 3]


# alpha �Լ��� ����Ϸ��� �̸� scales ���̺귯���� �ҷ��;� �Ѵ�.
library(scales)

# train_valid ������ �׸���
plot(formula = sum_age_mainactors ~ avg_slap_face,
     data = train_valid,
     col = alpha(c("blue", "green"), 0.8)[train_valid$genre],
     pch = 0,
     main = "��� �帣 ����",
     xlab = "ȸ�� �� �´� Ƚ��",
     ylab = "�ֿ���� ���� �հ�")

# test ������ ǥ���ϱ�
points(formula = sum_age_mainactors ~ avg_slap_face,
       data = test,
       pch = 16,
       cex = 1.2,
       col = alpha(c("blue", "green"), 0.5)[test$genre])

# ���� �׸���
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
  
  # idx�� ���� train vs. valid ������ ������
  valid <- train_valid[idx == k, ]
  train <- train_valid[idx != k, ]
  
  # valid ������ ��������/�������� ������
  valid_x <- valid[, -3]
  valid_y <- valid[, 3]
}
  
  
 ### 3. 
# ������ƽ ȸ�ͺм� �� ����

full <- glm(formula = genre ~ .,
            data = train,
            family = "binomial")

# Stepwise
step <- step(object = full,
             trace = F)

# full�� step �𵨺� Ȯ�� ����
full_pred_p <- as.numeric(predict(object = full,
                                  newdata = valid_x,
                                  type = "response"))
step_pred_p <- as.numeric(predict(object = step,
                                  newdata = valid_x,
                                  type = "response"))

# # 4. �з� ��Ȯ���� �и�


l <- length(valid_y)

for(i in unlist(unique(result$i))){
  
  # i�� �������� 0 �Ǵ� 1�� �з�
  full_pred_class <- ifelse(full_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
  step_pred_class <- ifelse(step_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
  
  # �з� ��Ȯ�� ���
  full_accuracy <- sum(full_pred_class == valid_y) / l
  step_accuracy <- sum(step_pred_class == valid_y) / l
  
  # result ���̺��� �з� ��Ȯ�� �Է�
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

 


# full�� step �𵨺� Ȯ�� ����
full_pred_p <- as.numeric(predict(object = full,
                                  newdata = valid_x,
                                  type = "response"))
step_pred_p <- as.numeric(predict(object = step,
                                  newdata = valid_x,
                                  type = "response"))

 
# �з� ��Ȯ���� �и�
l <- length(valid_y)

for(i in unlist(unique(result$i))){
  
  # i�� �������� 0 �Ǵ� 1�� �з�
  full_pred_class <- ifelse(full_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
  step_pred_class <- ifelse(step_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
  
  # �з� ��Ȯ�� ���
  full_accuracy <- sum(full_pred_class == valid_y) / l
  step_accuracy <- sum(step_pred_class == valid_y) / l
  
  # result ���̺��� �з� ��Ȯ�� �Է�
  result[result$fold == k
         & result$mdl == "full"
         & result$i == i, "accuracy"] <- full_accuracy
  result[result$fold == k
         & result$mdl == "step"
         & result$i == i, "accuracy"] <- step_accuracy
}

##

# full �� k-Fold = 1
plot(accuracy ~ i,
     result[result$mdl == "full" & result$fold == 1, ],
     type = "l",
     col = alpha("purple", 0.4),
     ylim = c(0.3, 1),
     xlab = "�Ӱ�ġ",
     ylab = "�з� ��Ȯ��",
     main = "�з� ��Ȯ�� in full/step 3-Fold CV")

# full �� k-Fold = 2
lines(accuracy ~ i,
      result[result$mdl == "full" & result$fold == 2, ],
      col = alpha("orange", 0.4))

# full �� k-Fold = 3
lines(accuracy ~ i,
      result[result$mdl == "full" & result$fold == 3, ],
      col = alpha("green", 0.4))

# step �� k-Fold = 1
lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 1, ],
      col = alpha("purple", 0.5),
      lty = 2)

# step �� k-Fold = 2
lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 2, ],
      col = alpha("orange", 0.5),
      lty = 2)

# step �� k-Fold = 3
lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 3, ],
      col = alpha("green", 0.5),
      lty = 2)

# ���� �׸���
legend("topleft",
       c("full k=1", "full k=2", "full k=3",
         "step k=1", "step k=2", "step k=3"),
       col = c(alpha(c("purple", "orange", "green"), 0.4),
               alpha(c("purple", "orange", "green"), 0.5)),
       lty = rep(c(1, 2), each = 3),
       bty = "n",
       cex = 0.9)


##

# �׷����� ���� plyr ��Ű���� ��ġ�ϰ� ���̺귯�� �ҷ�����
install.packages("plyr")
library(plyr)

# �𵨺� �Ӱ�ġ�� ��� �з� ��Ȯ�� ����ϱ�
tmp <- ddply(result, .(mdl, i), summarise, avg_accuracy = mean(accuracy))


# full ��
lines(avg_accuracy ~ i,
      tmp[tmp$mdl == "full", ],
      col = alpha("red", 0.7),
      lty = 1,
      type = "o",
      pch = 20)

# step ��
lines(avg_accuracy ~ i,
      tmp[tmp$mdl == "step", ],
      col = alpha("red", 0.7),
      lty = 2,
      type = "o",
      pch = 20)

# <span style="font-size: 14.6667px;">
  
  # ���� �׸���
  legend("topright",
         c("full avg accuracy", "step avg accuracy"),
         pch = 20,
         col = alpha("red", 0.7),
         lty = rep(c(1, 2)),
         bty = "n",
         cex = 0.9)
  # </span>
  
  
  tmp[tmp$avg_accuracy == max(tmp$avg_accuracy), ]
  
   
  # test_x�� ��ε���� Ȯ���� step �𵨷� ���ϱ�
  test_p <- as.numeric(predict(object = step,
                               newdata = test_x,
                               type = "response"))
  
  # �Ӱ�ġ 0.45�� �������� �����󸶿� ��ε�� �з��ϱ�
  test_class <- ifelse(test_p < 0.45, levels(data$genre)[1], levels(data$genre)[2])
  
  # �з� ��Ȯ�� ����ϱ�
  sum(test_class == test_y) / length(test_y)
  
  
  ##
  
  # ����/���� ����� (pch �ɼ��� ����)
  ox <- as.factor(ifelse(test_class == test_y, "O", "X"))
  
  # train_valid ������ �׸���
  plot(formula = sum_age_mainactors ~ avg_slap_face,
       data = train_valid,
       col = alpha(c("blue", "green"), 0.8)[train_valid$genre],
       xlab = "ȸ�� �� �´� Ƚ��",
       ylab = "�ֿ���� ���� �հ�",
       main = "��� �帣 ����")
  
  # test ������ �׸���
  points(formula = sum_age_mainactors ~ avg_slap_face,
         data = test,
         col = alpha(c("blue", "green"), 0.6)[test$genre],
         pch = c(19, 17)[ox])
  
  # ���� �׸���
  legend("topleft",
         legend = c(paste0("train_valid ", levels(train_valid$genre)),
                    paste0("test ", levels(test$genre), " ����"),
                    paste0("test ", levels(test$genre), " ����")),
         pch = c(1, 1, 19, 19, 17, 17),
         col = c(alpha(c("blue", "green"), 0.8),
                 alpha(c("blue", "green"), 0.6),
                 alpha(c("blue", "green"), 0.6)),
         cex = 0.9,
         bty = "n")
  
   