setwd("/Users/syedrahman/Documents/Spring2016/Multivariatedata/unfiled/Homeworks/HW\ 4")
library("smacof")
crabs = read.table("crabs_data.txt", header = TRUE)
head(crabs)
newcrabs = scale(as.matrix(crabs), center = TRUE, scale = TRUE)

###some sort of scaling might need to happen to adjust mm in first two columns to cm. I just scaled the data.

stress.scale = matrix(0,nrow = 3, ncol = 1)

names(crabs.mds)

for(k in 1:3){
    crabs.mds = smacofSym(dist(newcrabs), ndim = k)
    stress.scale[k] = crabs.mds$stress
}


stress = matrix(0,nrow = 3, ncol = 1)

for(k in 1:3){
    crabs.mds = smacofSym(dist(as.matrix(crabs)), ndim = k)
    stress[k] = crabs.mds$stress
}


###equivalence of PCA and metric MDS should be mentioned

### scaled and centered plots show this equvalence
crabs.mds = smacofSym(dist(crabs), ndim = 2)
pdf(file='mdsplotcrabs.pdf')
plot(crabs.mds)
dev.off()

pc1 = prcomp(crabs)
pdf(file='pcaplotcrabs.pdf')
plot(pc1$x[,1],pc1$x[,2] , xlab="PC1",ylab="PC2",type="n",lwd=2)
text(pc1$x[,1],pc1$x[,2],labels=abbreviate(row.names(crabs)),cex=0.7,lwd=2)
dev.off()
