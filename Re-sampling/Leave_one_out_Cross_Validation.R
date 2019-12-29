require(ISLR)
#Bootstrap resampling
require(boot)
?boot
#Cross validation for generalised linear models
?cv.glm
#car data
plot(mpg~horsepower,data=Auto)
#fit glm model
glm.fit=glm(mpg~horsepower,data=Auto)
#cross validation
#delta is the predicted error after cross alidation
cv.glm(Auto,glm.fit)$delta

#   [1] leave_one_out error = 24.23151, bias corrected error= 24.23114
#slow because it's brute force

## A function to do cross validation ourselves ##
loocv = function(fit){
#extracts element hii from fit = diagonal elements
  hii = lm.influence(fit)$h
  mean((residuals(fit)/(1-hii))^2)
  }

loocv(glm.fit)
# [1] 24.23151 yay it matches the library method.

cv.error=rep(0,5)
degree=1:5
#vector for collecting errors
for(d in degree){
  #fitting to polynomial kernel
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")
#higher the degree lower the error


# K fold Cross validation
#10 fold
cv.error10 = rep(0,5)
for(d in degree){
  #fitting to polynomial kernel
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(glm.fit,data=Auto,K=10)$delta[1]
}
plot(degree,cv.error10,type="b")
lines(degree,cv.error10,type='b',col="red")
