require(ISLR)
load("5.R.RData")
attach(Xy)
model1 <- lm(y~X1+X2,data=Xy)
summary(model1)
#Quiz Q1
#Download the file 5.R.RData and load it into R using load("5.R.RData").
#Consider the linear regression model of y on X1 and X2. What is the standard error for ?
#0.026

#Q2
matplot(Xy,type="l",label)
#Our estimate of B1 is too low

# Q3 Now, use the (standard) bootstrap to estimate s.e.(β^1). To within 10%, what do you get?
require(boot)
alpha = function(x,y){
  vx = var(x)
  vy = var(y)
  cxy= cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Xy$X1,Xy$y)
#[1] 0.4167192
alpha.fn = function(data,index){
  with(data[index,],alpha(Xy$X1,Xy$y))
}

alpha.fn<-function(data, index) {
  fit1<-lm(y~., data=Xy[index,])
  coefficients(fit1)[['X1']]
}

set.seed(1)
alpha.fn (Xy,sample(1:100,100,replace=TRUE))
#[1] 0.1059068

boot.out=boot(Xy,alpha.fn,R=1000)
boot.out
#Bootstrap Statistics :
#original       bias    std. error
#t1* 0.1453263 0.0001885914  0.02873965
