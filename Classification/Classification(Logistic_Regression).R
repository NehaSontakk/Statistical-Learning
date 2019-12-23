require(ISLR)
names(Smarket)
?Smarket
summary(Smarket)
## Using the stock market dataset
?pairs
#matrix of scatter plots
pairs(Smarket,col=Smarket$Direction)
# Logistic Regression
#family = binomial tells glm to fit logistic regression method
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial)
summary(glm.fit)
#nothing is significant, if it was people would make a ton of money  
glm.probs=predict(glm.fit,type="response")
#predict the response and look at first 5
glm.probs[1:5]
#they're very close to 50 
glm.preds=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
# Decision boundary will be 50 percent
table(glm.preds,Direction)
#         Direction
#glm.preds Down  Up
#    Down  145 141
#    Up    457 5073
mean(glm.preds==Direction)
## 0.5216 = accuracy

#Division into training and test set
#all observations below year 2005 are training
train = Year<2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata = Smarket[!train,],type="response")
glm.preds =ifelse(glm.probs>0.5,"Up","Down")
Direction.2005 = Smarket$Direction[!train]
table(glm.preds,Direction.2005)
# Direction.2005
# glm.preds Down Up
#     Down   77 97
#     Up     34 44

mean(glm.preds==Direction.2005)
#0.48  = worse than before
#Overfitting issues because we are using too many variables
#should use just one or two

# JUST Lag1 and Lag2
train = Year<2005
glm.fit = glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata = Smarket[!train,],type="response")
glm.preds =ifelse(glm.probs>0.5,"Up","Down")
Direction.2005 = Smarket$Direction[!train]
table(glm.preds,Direction.2005)
#Direction.2005
#glm.preds Down  Up
#    Down   35  35
#    Up     76 106
mean(glm.preds==Direction.2005)
#0.60
#better than before