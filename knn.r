# k nearest neighbors

library(class)
?knn

attach(Smarket)
xlag=cbind(Lag1, Lag2)
train=Year<2005

# 1 nearest neighbor
prediction=knn(xlag[train,],xlag[!train,],Direction[train],k=1)

table(prediction,Direction[!train])
mean(prediction==Direction[!train])

# 3 nearest neighbor
prediction=knn(xlag[train,],xlag[!train,],Direction[train],k=3)

table(prediction,Direction[!train])
mean(prediction==Direction[!train])

# 5 nearest neighbor
prediction=knn(xlag[train,],xlag[!train,],Direction[train],k=5)

table(prediction,Direction[!train])
mean(prediction==Direction[!train])

print ("Accuracy increases from k=1 to k=3, but it decreases when k=5 (overfitting)")