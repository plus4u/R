install.packages("gmodels")
library(gmodels)
x <- c(rep("a", 100),rep("b",100))
x
y <- c(rep("pass",70),rep("fail",30),rep("pass",40),rep("fail",60))
xy <- data.frame(area=x, result=y)
xy

CrossTable(xy$area, xy$result)

chisq.test(xy$area, xy$result)




str(iris)
head(iris,100)

set.seed(123)
index <- sample(1:nrow(iris), 5)
index 
iris[index, ]


##


set.seed(1999)
grp <- rep(c(9,8,3,4), 25)
noisy <- numeric(0)
for(x in grp){
  noisy <- rbind(noisy, rnorm(100, x, 1.5 * x))
}

head(grp,10)
head(noisy,10)


## pca
noisy.pca <- prcomp(noisy)
pairs(noisy.pca$x[,1:5], col= as.integer(as.factor(grp)))



# 

require(MASS)


set.seed(1999)

data <- matrix(runif(10000, 0, 10), nrow=100)
data

data.grp <- transform(data.frame(data), grp = as.factor(rep(1:4, 25)))

str(data.grp)
data.grp$X1


data.lda <- lda(grp ~., data.grp)
plot(data.lda)




str(iris)

# 

tr <- sample(1:150, 120)

train <- as.data.frame(iris[tr, 1:5])
str(train)

# train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
# test <- rbind(iris3[-tr,,1], iris3[-tr,,2], iris3[-tr,,3])
test <- rbind(iris[-tr, 1:4])
test.c <- as.data.frame(iris[-tr, 1:4])
test.d <- as.data.frame(iris[-tr, 1:5])

z <- lda(Species~ ., data=train)

z
pre_c <- predict(z, test.c)

mean(pre_c$class==test.d$Species)

head(pre_c,10)

head(test.d,20)

test.e <- transform(test.d, calc = Sepal.Length*0.9213148 + Sepal.Width*1.6278908 + Petal.Length* (-2.3018360) + Petal.Width * (-2.8047808))

test.e <- cbind(test.e, pre_c$x)

test.e$calc2 = test.e$LD1-test.e$calc

test.f <- transform(test.d, calc3 = Sepal.Length * 0.3629888 +
                             Sepal.Width   * (-2.3453176) + Petal.Length *  0.6916524 + Petal.Width * ( -2.5974560))

test.e =cbind(test.e, test.f)

test.e$clc4 = test.e$LD2 - test.e$calc3

test.e

#           test.e$calc2=(test.d$LD1 - test.e$calc))


test.e
pre_c
z

# setosa         4.951351    3.400000     1.470270   0.2351351
# versicolor     5.997436    2.810256     4.282051   1.3435897
# virginica      6.577273    2.968182     5.543182   2.0136364

4.951351*0.9213148 +  3.40 *1.6278908 + 1.47027 * (-2.3018360) + 0.2351351 * (-2.8047808)  # 6.052759

5.997436*0.9213148 +  2.810256*1.6278908 + 4.282051* (-2.3018360) + 1.3435897 * (-2.8047808)  # -3.524737

6.577273 *0.9213148 +  2.968182 *1.6278908 + 5.543182 * (-2.3018360) + 2.0136364 * (-2.8047808)  # -7.515689

# 

x <- sample(1:100, 10)
x

str(iris)

train <- sample(1:150, 120)
d.train <- iris[train,]
d.test <- iris[-train,]
d.test
str(d.test)

str(z)

### data 

data <- iris

str(data)


# 데이터 필터링
data <- subset(data, Species %in% c("setosa", "versicolor"))

# factor의 레벨을 3에서 2로 바꾸어 주기 위함줄여주기 위함
# data$genre <- as.factor(as.character(data$Species))

 

# 재현성을 위한 seed 설정
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
test_x <- test[, -4]
test_y <- test[, 4]




## examples of glm
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
outcome
gl(3,3)


print(d.AD <- data.frame(treatment, outcome, counts))

glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
glm.D93
anova(glm.D93)
summary(glm.D93)




### tes

data <- iris

if (any(data$Species=="versicolor")) data$y=1

data

glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, family = "binomial")
