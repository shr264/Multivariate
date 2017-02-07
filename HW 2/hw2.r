library(caret)
library(xtable)

### problem 1
setwd("/Users/syedrahman/Documents/Spring2016/Multivariatedata/unfiled/Homeworks/HW 2")
wine = read.table("Winedataset.txt",header=TRUE)
table1 = matrix(0,ncol = 1, nrow = 5)
row.names(table1) = c("RANDOM FORESTS", "LDA", "QDA", "KNN", "DECISION TREES")
rf = train(wine[,2:14],wine[,1],method = "rf",trControl=trainControl(method="cv",number=10))
lda = train(wine[,2:14],wine[,1],method = "lda",trControl=trainControl(method="cv",number=10))
qda = train(wine[,2:14],wine[,1],method = "qda",trControl=trainControl(method="cv",number=10))
knnGrid <- expand.grid(.k=c(1:50))
knn = train(wine[,2:14],wine[,1],method = "knn",trControl=trainControl(method="cv",number=10),tuneGrid = knnGrid)
dt = train(wine[,2:14],wine[,1],method = "rpart",trControl=trainControl(method="cv",number=10),tuneLength=30)
table1[1,1] = max(rf$results$Accuracy)
table1[2,1] = lda$results$Accuracy
table1[3,1] = qda$results$Accuracy
table1[4,1] = max(knn$results$Accuracy)
table1[5,1] = max(dt$results$Accuracy)
xtable(table1)


###problem 2

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

olive.train = read.table("olive_train.txt",header=TRUE)
table2 = matrix(0,ncol = 1, nrow = 5)
row.names(table2) = c("RANDOM FORESTS", "LDA", "QDA", "KNN", "DECISION TREES")
colnames(table2) = c("Accuracy")
###region
rf2 = train(olive.train[,3:10],as.factor(olive.train[,1]),method = "rf",trControl=trainControl(method="cv",number=10))
lda2 = train(olive.train[,3:10],as.factor(olive.train[,1]),method = "lda",trControl=trainControl(method="cv",number=10))
qda2 = train(olive.train[,3:10],as.factor(olive.train[,1]),method = "qda",trControl=trainControl(method="cv",number=10))
knnGrid <- expand.grid(.k=c(1:50))
knn2 = train(olive.train[,3:10],as.factor(olive.train[,1]),method = "knn",trControl=trainControl(method="cv",number=10),tuneGrid = knnGrid)
dt2 = train(olive.train[,3:10],as.factor(olive.train[,1]),method = "rpart",trControl=trainControl(method="cv",number=10),tuneLength=30)
table2[1,1] = max(rf2$results$Accuracy)
table2[2,1] = lda2$results$Accuracy
table2[3,1] = qda2$results$Accuracy
table2[4,1] = max(knn2$results$Accuracy)
table2[5,1] = max(dt2$results$Accuracy)
xtable(table2)

table3 = matrix(0,ncol = 1, nrow = 5)
row.names(table3) = c("RANDOM FORESTS", "LDA", "QDA", "KNN", "DECISION TREES")
colnames(table3) = c("Accuracy")
###region
rf3 = train(olive.train[,3:10],as.factor(olive.train[,2]),method = "rf",trControl=trainControl(method="cv",number=10))
lda3 = train(olive.train[,3:10],as.factor(olive.train[,2]),method = "lda",trControl=trainControl(method="cv",number=10))
qda3 = train(olive.train[,3:10],as.factor(olive.train[,2]),method = "qda",trControl=trainControl(method="cv",number=10))
knnGrid <- expand.grid(.k=c(1:50))
knn3 = train(olive.train[,3:10],as.factor(olive.train[,2]),method = "knn",trControl=trainControl(method="cv",number=10),tuneGrid = knnGrid)
dt3 = train(olive.train[,3:10],as.factor(olive.train[,2]),method = "rpart",trControl=trainControl(method="cv",number=10),tuneLength=30)
table3[1,1] = max(rf3$results$Accuracy)
table3[2,1] = lda3$results$Accuracy
table3[3,1] = qda3$results$Accuracy
table3[4,1] = max(knn3$results$Accuracy)
table3[5,1] = max(dt3$results$Accuracy)
xtable(table3)

Models <- list(rf = rf2, lda = lda2, qda = qda2, knn = knn2,
               tree = dt2)
Models2 <- list(rf = rf3, lda = lda3, qda = qda3, knn = knn3,
                   tree = dt3)
newdata = read.table("olive_test.txt")
names(newdata) = names(olive.train)[3:10]
region.pred = predict(Models,newdata = newdata)
area.pred = predict(Models2,newdata = newdata)
region = matrix(0,nrow=6,ncol = 20)
row.names(region) = c("RANDOM FORESTS", "LDA", "QDA", "KNN", "DECISION TREES","MODE")
region[1,] = region.pred$rf
region[2,] = region.pred$lda
region[3,] = region.pred$qda
region[4,] = region.pred$knn
region[5,] = region.pred$tree
region[6,] = apply(region,2,getmode)
xtable(region)

area = matrix(0,nrow=6,ncol = 20)
row.names(area) = c("RANDOM FORESTS", "LDA", "QDA", "KNN", "DECISION TREES","MODE")
area[1,] = area.pred$rf
area[2,] = area.pred$lda
area[3,] = area.pred$qda
area[4,] = area.pred$knn
area[5,] = area.pred$tree
area[6,] = apply(area,2,getmode)
xtable(area)
