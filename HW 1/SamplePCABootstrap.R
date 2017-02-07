## Sample bootstrapped PCA

library(lattice)                                  # Lattice graphics package

setwd('c:\\RES\\TTU\\Courses\\MultivariateStats\\R')
rm(list=ls())                                     # Clear all variables
graphics.off()                                    # Close any open graphics windows

## Read data into data frame

sample.data = read.table('MultivariateData.csv', header=TRUE, sep=",")
sample.data

attach(sample.data)                               # Make contents available

X = cbind(X1,X2,X3,X4,X5)                         # Concatenate variables into data matrix
X

#X = log(X)                                   # Optionally transform variables (if needed)

n.iterations = 1000                               # Number of bootstrap iterations
ci.level = 0.95                                   # Confidence level, as probability
useCorr = TRUE;                                   # Set to use correlation or covariance matrix

# --------------------------------------------------------------------------------------------

nObs = nrow(X)                                    # Size of matrix X
nVars = ncol(X)

## PCA results for original data

if (useCorr)
  {
  X.C = cor(X)
  X.pca = prcomp(X, scale=TRUE)
  } else {
  X.C = cov(X)
  X.pca = prcomp(X, scale=FALSE)
  }

pca.eigenvalues = X.pca$sdev^2                  # Isolate eigenvalues, loadings, scores
pca.percentVar = 100*pca.eigenvalues / sum(pca.eigenvalues)
pca.cumVar = cumsum(pca.percentVar)
pca.loadings = X.pca$rotation
pca.scores = predict(X.pca)

## Bootstrapped PCA results

distrib.corrs = matrix(0, nrow=n.iterations, ncol=nVars*nVars)  # Allocate vectors for sampling distributions
distrib.eigenvalues = matrix(0, nrow=n.iterations, ncol=nVars) 
distrib.percentVar = matrix(0, nrow=n.iterations, ncol=nVars)
distrib.loadings = matrix(0, nrow=n.iterations, ncol=nVars*nVars)

for (iter in 1:n.iterations)                    # Bootstrap iterations
  {
  i = sample(1:nObs, replace=TRUE)                # Get bootstrapped subscripts
  X.boot = X[i,]                                  # Bootstrapped sample of rows of X
  
  if (useCorr)
    {
    X.boot.C = cor(X.boot)
    X.boot.pca = prcomp(X.boot, scale=TRUE)
    } else {
    X.boot.C = cov(X.boot)
    X.boot.pca = prcomp(X.boot, scale=FALSE)
    }

  boot.C = X.boot.C                               # Isolate corrs, eigenvalues, loadings
  boot.eigenvalues = X.boot.pca$sdev^2
  boot.percentVar = 100*boot.eigenvalues / sum(boot.eigenvalues)
  boot.loadings = X.boot.pca$rotation
  boot.scores = predict(X.boot.pca)
  
  for (v in 1:nVars)                              # Reverse signs of loadings if negatively correlated
    {                                             #   with original loadings
    c = cor(cbind(pca.loadings[,v],boot.loadings[,v]))
    if (c[2,1]<0) boot.loadings[,v] = -boot.loadings[,v]
    }
  
  distrib.corrs[iter,] = c(boot.C)                # Stash results into bootstrap distributions
  distrib.eigenvalues[iter,] = c(boot.eigenvalues)
  distrib.percentVar[iter,] = c(boot.percentVar)
  distrib.loadings[iter,] = c(boot.loadings)
  }

prob.low = (1-ci.level)/2                       # Low probability bound for CI
prob.high = 1-prob.low                          # High probability bound for CI

ci.low.eigenvalues =  matrix(0, nrow=1, ncol=nVars)    # Allocate low & high confidence bounds
ci.high.eigenvalues = matrix(0, nrow=1, ncol=nVars)
ci.low.percentVar =   matrix(0, nrow=1, ncol=nVars)
ci.high.percentVar =  matrix(0, nrow=1, ncol=nVars)

ci.low.corrs =        matrix(0, nrow=nVars, ncol=nVars)
ci.high.corrs =       matrix(0, nrow=nVars, ncol=nVars)
ci.low.loadings =     matrix(0, nrow=nVars, ncol=nVars)
ci.high.loadings =    matrix(0, nrow=nVars, ncol=nVars)

v = 0;
for (vc in 1:nVars)                             # Find and stash confidence bounds
  {
  ci.low.eigenvalues[,vc] = quantile(distrib.eigenvalues[,vc], probs=prob.low)
  ci.high.eigenvalues[,vc] = quantile(distrib.eigenvalues[,vc], probs=prob.high)
  ci.low.percentVar[,vc] = quantile(distrib.percentVar[,vc], probs=prob.low)
  ci.high.percentVar[,vc] = quantile(distrib.percentVar[,vc], probs=prob.high)
  for (vr in 1:nVars)
    {
    v = v+1
    ci.low.corrs[vr,vc] = quantile(distrib.corrs[,v], probs=prob.low)
    ci.high.corrs[vr,vc] = quantile(distrib.corrs[,v], probs=prob.high)
    ci.low.loadings[vr,vc] = quantile(distrib.loadings[,v], probs=prob.low)
    ci.high.loadings[vr,vc] = quantile(distrib.loadings[,v], probs=prob.high)
    }
  }
  

## Display original values with low & high confidence bounds

X.C                                           
ci.low.corrs
ci.high.corrs

pca.eigenvalues                                 
ci.low.eigenvalues                              
ci.high.eigenvalues

pca.percentVar
ci.low.percentVar
ci.high.percentVar

pca.loadings
ci.low.loadings
ci.high.loadings


## Selected histograms

#windows()
hist(distrib.corrs[,2], col='blue', xlab='Corr(X1,X2)', xlim=c(0,1),
  main='Bootstrapped distribution of Corr(X1,X2)')

#windows()
hist(distrib.eigenvalues[,1], col='blue', xlab='Eigenvalue 1', 
  xlim=c(0,1.05*max(distrib.eigenvalues[,1])),
  main='Bootstrapped distribution of Eigenvalue 1')

#windows()
hist(distrib.percentVar[,1], col='blue', xlab='Percent variance for PC 1', xlim=c(0,100),
  main='Bootstrapped distribution of Percent Variance for PC1')

