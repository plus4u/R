# pca 


## case - princomp

fit <- princomp(pcaFa, cor=TRUE)  

summary(fit) # print variance accounted for 

loadings(fit) # pc loadings 

plot(fit,type="lines") # scree plot 

fit$scores # the principal components

biplot(fit_prc)



# case - prcomp


fit_prc <- prcomp(pcaFa, scale = FALSE )  

summary(fit_prc) # print variance accounted for 

loadings(fit_prc) # pc loadings 

plot(fit_prc,type="lines") # scree plot 

fit_prc$scores # the principal components

biplot(fit_prc)

