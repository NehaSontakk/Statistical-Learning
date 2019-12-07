#library for support functions and datasets
library(MASS)
#Library of datasets
library(ISLR)
library(ggplot2)

### Simple Linear Regression ###

names(Boston)
?Boston
#Plot median value of owner occupied homes vs lower status of the population (percent).
plot(medv~lstat,Boston,
main="Median value of owner occupied homes \n vs the lower status of population")
# As lower status decreases median value increases

#fitting a linear model to the data
fit1 = lm(medv~lstat,data=Boston)
fit1
summary(fit1)
## Coefficient is negative because of the inverse relationship
abline(fit1,col="red")
#components of the line
names(fit1)
# [1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values" "assign"        "qr"           
#[8] "df.residual"   "xlevels"       "call"          "terms"         "model"    

#gives confidence interval on the fit line
confint(fit1)
#                2.5 %     97.5 %
#(Intercept) 33.448457 35.6592247
#lstat       -1.026148 -0.8739505

# Predicting for new points with confidence intervals
predict(fit1,data.frame(lstat=c(65,2,41)),interval="confidence")

#         fit        lwr        upr
#1 -27.199367 -31.219732 -23.179003
#2  32.653742  31.678068  33.629416
#3  -4.398183  -6.622617  -2.173748

#We can see that for high values of lstat the medv predicted is super low
#the confidence bands give a range of possible outputs


## Multiple Linear regression
fit2 =  lm(medv~lstat+age,data=Boston)
fit2
# Coefficients:
# (Intercept)        lstat          age  
# 33.22276     -1.03207      0.03454  

summary(fit2)

#age is not as significant as lstat

fit3 = lm(medv~.,data=Boston)
fit3
summary(fit3)
#compared to other variables age is not very significant

#multiple plot using R
par(mfrow=c(2,2))
plot(fit3)

#updated plot after removing age and indus(non retail businesses) (insignificant variables)

fit4 = update(fit3,~.-age-indus)
fit4
summary(fit4)



### Non linearities and Interactions

#Here we can add non linearities in the data
fit5 = lm(medv~lstat*age, data=Boston)
fit5
summary(fit5)
## we can see that we get an additional effect for lstat:age also but it's only slightly significant

fit6 = lm(medv~lstat+I(lstat^2),Boston)
summary(fit6)


attach(Boston)
par(mfrow=c(1,1))
plot(lstat,medv)
#we want to include the quadratic fit
points(lstat,fitted(fit6),col="blue",pch=20)
text(15,30,"Quadratic fit",col="blue")
#straight line fit
abline(fit1,col="red",pch=10)
text(15,10,"Straight fit",col="red")

#plotting a 4th degree polynomial

fit7 = lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="green",pch=20)
#plot(1:20,1:20,pch=1:20,cex=2)
text(35,20,"Polynomial fit",col="green")


#### Qualitative variables ####
#using car seats dataset

names(Carseats)
#childrens data for carseats
summary(Carseats)
#everything in the frame but sales
#plus some interactions
fit11 = lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit11)
## Label codes done by R : Dummy encoding scheme
contrasts(Carseats$ShelveLoc)

#function with unnamed arguements
regplot=function(x,y,...){
  fit = lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}

attach(Carseats)
regplot(Price,Sales)

regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)
