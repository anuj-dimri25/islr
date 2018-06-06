# forward subset selection

# model selection
library(ISLR)
library(leaps)
summary(Hitters)

# omitting na values
Hitters=na.omit(Hitters)
# checking if all na's have been removed
with(Hitters,sum(is.na(Salary)))

model=regsubsets(Salary~.,data=Hitters, nvmax=19, method="forward")
summary(model)
plot(model,scale="Cp")

# model selection using validation set
dim(Hitters)
set.seed(1)
train=sample(seq(263),180,replace=FALSE)
train
model=regsubsets(Salary~.,data=Hitters[train,],nvmax=19,method="forward")

errors=rep(NA,19)
test=model.matrix(Salary~.,data=Hitters[-train,])

for (i in 1:19)
{
  coefi=coef(model,id=i)
  prediction=test[,names(coefi)]%*%coefi
  errors[i]=mean((Hitters$Salary[-train]-prediction)^2)
  
}

plot(sqrt(errors),ylab="rmse",ylim=c(250,400),pch=20,type="b")
points(sqrt(model$rss[-1]/180),col="red",pch=20,type="b")
legend("topright",legend=c("training","validation"),col=c("red","black"),pch=20)



###### model selection using cross validation

set.seed(11)
folds=sample(rep(1:10,length=nrow(Hitters)))
table(folds)
errors=matrix(NA,10,19)


#### fxn for prediction from regsubsets
predict=function(object,testdata,id,...)
{
  form=as.formula(object$call[[2]])
  matrix=model.matrix(form,testdata)
  coefi=coef(object,id=id)
  matrix[,names(coefi)]%*%coefi
}

for (k in 1:10)
{
  model=regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forward")
  for (i in 1:19)
  {
    prediction=predict(model,Hitters[folds==k,],id=i)
    errors[k,i]=mean((Hitters$Salary[folds==k]-prediction)^2)
  }
}

rmse=sqrt(apply(errors,2,mean))
plot(rmse,pch=20,type="b")