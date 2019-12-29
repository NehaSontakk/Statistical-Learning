## Bootstrap Sampling
# Minimum risk investment - 5.2 in the book
require(ISLR)
require(boot)
#optimal formula for risk investment is
alpha = function(x,y){
  vx = var(x)
  vy = var(y)
  cxy = cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)
# [1] 0.5758321

#finding standard error for alpha
#making a wrapper for alpha function to work
#index shows which observations can be repeated
alpha.fn = function(data,index){
  with(data[index,],alpha(X,Y))
}
alpha.fn(Portfolio,1:100)
#[1] 0.5758321
#we want reproducible results
set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))
#[1] 0.4115359
?boot
boot.out = boot(Portfolio,alpha.fn,R=1000)
boot.out
#Bootstrap Statistics :
#original        bias    std. error
#     t1* 0.5758321 -7.315422e-05  0.08861826
plot(boot.out)
