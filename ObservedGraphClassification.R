

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


# Observed Class using data in a citation file
# This file inlcudes data about the same citations of papers

One <- matrix(1,dim(W)[1],dim(W)[2])
D <- diag(diag(W%*%One))
Yhat1 <- ((ginv(D)%*%t(W)%*%Y)>0.5)
misclassification2 <- t(Yhat1-Y)%*%(Yhat1-Y)
print(misclassification)