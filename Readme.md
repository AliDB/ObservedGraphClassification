
Observed Graph Classification
=============================

In this file, it is shown that how we can use graph based theories to have a better classification. In this problem, firstly we are trying to use linear regression to predict the title of papers (either AI/ML) based on the words that has been used in their introduction. However, the number of misclassification is a bit high; therefore, we use some other informations in terms of graph. We use a file called cite.txt to relate each paper to another one based on the similar citations used in their paper. By doing this, we reach to a better classification method which is shown hereunder.

This file is part of my project for Advanced Data Mining course with Prof. Culp.

``` r
library(MASS)

YCLASS=read.table("class.txt",header=TRUE)
XCLASS=read.table("title.txt",header=TRUE)
WCLASS=read.table("cite.txt",header=TRUE)
Y=as.numeric((as.matrix(YCLASS))=="AI")
X=as.matrix(XCLASS)
W=as.matrix(WCLASS)

# Predicting the title with Linear Regression 
g<-lm(Y~X)
Yhat2<-(g$fitted.values>0.5)
misclassification <- t(Yhat2-Y)%*%(Yhat2-Y)
print(misclassification)
```

    ##      [,1]
    ## [1,]  156

``` r
# Observed Class using data in a citation file
# This file inlcudes data about the same citations of papers

One <- matrix(1,dim(W)[1],dim(W)[2])
D <- diag(diag(W%*%One))
Yhat1 <- ((ginv(D)%*%t(W)%*%Y)>0.5)
misclassification2 <- t(Yhat1-Y)%*%(Yhat1-Y)
print(misclassification2)
```

    ##      [,1]
    ## [1,]   38
