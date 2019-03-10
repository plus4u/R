## 

y1 <- c(1:9)
y2 <- c(11:19)
y3 <- c(101:109)

y <- c(y1,y2,y3)
y

n <- rep(1:3, 3)

group <- rep(1:3, 3)

group_df <- data.frame(y, group)
group_df

sapply(group_df, class)

group_df1 <- transform(group_df, group=factor(group))
str(group_df1)
