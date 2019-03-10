### easy princomp

library(ggplot2)

x =c(0,2,3)
y =c(0,1,2)
z =data.frame(x,y)
cov(z)

eigen(cov(z))

xx=c(0,1,2,1,2,3)
yy=c(0,1,1,2,2,3)
zz=data.frame(xx,yy)
rownames(zz) = c("A","B","C","D","E","F") 


ggplot(data=zz, mapping=aes(x=xx,y=yy)) + 
    geom_point() +
    geom_text(aes(label=rownames(zz), vjust=1.1,hjust=2), check_overlap = T) +  # 알파벳 텍스트 추가
    geom_line(data=zz[c(1,2,5,6),], mapping=aes(x=xx,y=yy), color='red') +  # 빨간색 대각선 추가
    guides(fill="none")  #  


# cov 

zz_cov = cov(zz)
zz_cov

# eigen value 

zz_eigen = eigen(zz_cov)
 
zz_eigen

zz_eigen$vectors %*% diag(zz_eigen$values) %*% t(zz_eigen$vectors) ## review 

zz_eigen$vectors

## 1. pca

zz_pca = princomp(zz, cor=F) 

zz_pca


zz_pca$loading


zz_pca$scores

data.frame(zz_pca$scores)

## 2. check the result --> to make certain the procedure


cbind(xx-mean(xx), yy-mean(yy)) %*% zz_eigen$vectors 

cbind(xx-mean(xx), yy-mean(yy)) %*% zz_pca$loading # the same  


zz_pca_scores=data.frame(zz_pca$scores)

colnames(zz_pca_scores) = c("pca1","pca2")
rownames(zz_pca_scores) = c("A","B","C","D","E","F")

ggplot(data=zz_pca_scores, mapping=aes(x=pca1, y=pca2)) + 
    geom_point() +
    geom_text(aes(label=rownames(zz_pca_scores), vjust=1.1,hjust=2), check_overlap = T) +  # 알파벳 텍스트 추가
    geom_line(data=zz_pca_scores[c(1,2,5,6),], mapping=aes(x=pca1,y=pca2), color='green') +  # 빨간색 대각선 추가
    guides(fill="none")  #  


var(zz_pca_scores)  # the same with eigen value 
