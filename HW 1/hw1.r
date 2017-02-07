data1 = read.table("USTATES-data-1.txt", header=TRUE)
data = as.matrix(data1)
data = data[-40,]
pc1 = prcomp(data, center = TRUE)
summary(pc1)$importance[,1:5]
screeplot(pc1)
plot(pc1$x[,1],pc1$x[,2] , xlab="PC1",ylab="PC2",type="n",lwd=2)
text(pc1$x[,1],pc1$x[,2],labels=abbreviate(row.names(data1)),cex=0.7,lwd=2)
mat1 = t(as.matrix(data1))
matplot(mat1,type = "l")

sum(pc1$sdev[1:2]^2)/sum(pc1$sdev^2)
library(xtable)
xtable(summary(pc1))

### this is the bootstrap

set.seed(12345)
niter = 1000
propexp = matrix(0,1000,1);
for(i in 1:1000){
    pc = prcomp(data[sample(nrow(data),size=51,replace=TRUE),])
    propexp[i,1] = sum(pc$sdev[1:2]^2)/sum(pc$sdev^2)
}
hist(propexp)
mean(propexp)


data2 = read.csv("wine.csv", header=TRUE)
data20 = (as.matrix(data2))
matplot(t(data20),type = "l", xlab = "Variables", ylab = "Wines")
data20 = log(as.matrix(data2))
pc21 = prcomp(data20, center = TRUE, scale = TRUE)
summary(pc21)$importance[,1:4]
screeplot(pc21)
biplot(pc21)
names(summary(pc21))
summary(pc21)$rotation[,1:4]
