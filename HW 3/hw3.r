library(xtable)
library(vegan)
library(cluster)
library("caret")
install.packages()
library("gplots")
library("kernlab")
olive.train = read.table("olive_train.txt",header=TRUE)
d<-dist(olive.train[,3:10])
linkage<-hclust(d, method="complete", members = NULL)
hcd = as.dendrogram(linkage)

plot(linkage, labels = NULL, hang = 0.1, check =
TRUE,axes = TRUE, frame.plot = FALSE, ann = TRUE,main
     = "Dendrogram for Olive Data",sub = NULL, ylab = "Height")


silmat = matrix(0,11,10)
for(k in 2:10){
    olive.hcl = cutree(linkage, k = k)
    sil<-silhouette(olive.hcl,d)
    silmat[1:k,k]<-summary(sil)$clus.avg.widths
    silmat[11,k]<-summary(sil)$avg.width
}

pdf(file="hclsil.pdf")
heatmap.2(round(silmat,2), Rowv=FALSE, Colv=FALSE, dendrogram='none', cellnote=round(silmat,2), notecol="black", trace='none', key=FALSE,lwid = c(.01,.99),lhei = c(.01,.99),margins = c(5,15 ))
dev.off()

x <- daisy(olive.train, metric = c("gower"),
    stand = FALSE, type = list())

z <- agnes(x, diss = inherits(x, "dist"), metric = "euclidean",
      stand = FALSE, method = "single", par.method,
                          trace.lev = 0, keep.diss = TRUE)

plot(z,  main="plotit", which.plot = 2)

silmat = matrix(0,11,10)
for(k in 2:10){
    olive.agnes = cutree(z, k = k)
    sil<-silhouette(olive.agnes,d)
    silmat[1:k,k]<-summary(sil)$clus.avg.widths
    silmat[11,k]<-summary(sil)$avg.width
}

pdf(file="agnessil.pdf")
heatmap.2(round(silmat,2), Rowv=FALSE, Colv=FALSE, dendrogram='none', cellnote=round(silmat,2), notecol="black", trace='none', key=FALSE,lwid = c(.01,.99),lhei = c(.01,.99),margins = c(5,15 ))
dev.off()


plot(olive.agnes)

silmat = matrix(0,11,10)
for(k in 2:10){
    km<-kmeans(olive.train[,3:10],k,algorithm="MacQueen",iter.max=100,nstart=1000)
    sil<-silhouette(km$cluster,d)
    silmat[1:k,k]<-summary(sil)$clus.avg.widths
    silmat[11,k]<-summary(sil)$avg.width
}

pdf(file="kmeanssil.pdf")
heatmap.2(round(silmat,2), Rowv=FALSE, Colv=FALSE, dendrogram='none', cellnote=round(silmat,2), notecol="black", trace='none', key=FALSE,lwid = c(.01,.99),lhei = c(.01,.99),margins = c(5,15 ))
dev.off()

silmat = matrix(0,11,10)
for(k in 2:10){
    sc <- specc(as.matrix(olive.train[,3:10]), centers=k)
    clusters <- sc@.Data
    sil<-silhouette(clusters,d)
    silmat[1:k,k]<-summary(sil)$clus.avg.widths
    silmat[11,k]<-summary(sil)$avg.width
}

pdf(file="specsil.pdf")
heatmap.2(round(silmat,2), Rowv=FALSE, Colv=FALSE, dendrogram='none', cellnote=round(silmat,2), notecol="black", trace='none', key=FALSE,lwid = c(.01,.99),lhei = c(.01,.99),margins = c(5,15 ))
dev.off()


k = 10
sc <- specc(as.matrix(olive.train[,3:10]), centers=k)
clusters <- sc@.Data
print(xtable(table(clusters, olive.train[,2])))


sc <- specc(as.matrix(olive.train[,3:10]), centers=2)
clusters <- sc@.Data
rf3 = train(olive.train[,3:10],as.factor(clusters),method = "rf",trControl=trainControl(method="cv",number=10))
lda3 = train(olive.train[,3:10],as.factor(clusters),method = "lda",trControl=trainControl(method="cv",number=10))
qda3 = train(olive.train[,3:10],as.factor(clusters),method = "qda",trControl=trainControl(method="cv",number=10))
max(rf3$results$Accuracy)
lda3$results$Accuracy
qda3$results$Accuracy

