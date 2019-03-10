### 1.sample - setdiff     # 2. floor 


## 1. 

# Random sample indexes

train_index <- sample(1:nrow(iris), 0.7 * nrow(iris))
test_index <- setdiff(1:nrow(iris), train_index)

iris_train <- iris[train_index, ]
iris_test <- iris[test_index, ]

write.csv(iris_train, file = "iris_train.csv") # row.names = T 
write.csv(iris_test, file = "iris_test.csv")


## 2. 

v1 <- c(1:10)
v2 <- c(11:20)
v3 <- rep(1:5, 2)

df <- data.frame(v1,v2,v3)

df

set.seed(123)

train_index <- sample(1:nrow(df), 0.7 * nrow(df))

train_index

test_index <- setdiff(1:nrow(df), train_index)



df[train_index, ]

bound <- floor((nrow(df)/4)*3)         #define % of training and test set

df_s <- df[sample(nrow(df)), ]           #sample rows 

df_train <- df[1:bound, ]              #get training set
df_test <- df[(bound+1):nrow(df), ]    #get test set

df_train

df_test


