# random forest

require(randomForest)
require(MASS)
set.seed(7)
dim(Boston)
train=sample(1:nrow(Boston),300)

model=randomForest(medv~.,data=Boston,subset = train)
model

# trying different values of mtry
oob.error=double(13)
test.error=double(13)
for (mtry in 1:13)
{
  model=randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
  oob.error[mtry]=model$mse[400]
  prediction=predict(model,Boston[-train,])
  test.error[mtry]=with(Boston[-train,],mean((medv-prediction)^2))
  cat(mtry,"")
}

matplot(1:mtry,cbind(test.error,oob.error),pch=19,col=c("red","blue"),type="b",ylab="MSE")
legend("topright",legend=c("test","oob"),pch=19,col=c("red","blue"))

#########boosting############
require(gbm)
model=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(model)
plot(model,i="lstat")
plot(model,i="rm")


# compute test error as a fxn of trees
n.trees=seq(from=100,to=10000,by=100)
prediction=predict(model,newdata=Boston[-train,],n.trees=n.trees)
dim(prediction)
error=with(Boston[-train,],apply((prediction-medv)^2,2,mean))
plot(n.trees,error,pch=19,ylab="mse",xlab="number of trees",main="boostig test error")






