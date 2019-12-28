## K nearest neighbors
library(class)
?knn
#attach attaches the variables' names' from the dataframe
attach(Smarket)
#making a matrix
Xlag = cbind(Lag1,Lag2)
knn.preds=knn(Xlag[(Year==2004),],Xlag[(Year==2004),],Direction[(Year==2004)],k=2)
table(knn.preds,Direction[(Year==2004)])
#knn.preds Down  Up
#   Down   73  32
#   Up     39 108

mean(knn.preds==Direction[(Year==2004)])
#0.718254