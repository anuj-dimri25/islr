# ridge regression and lasso
library(glmnet)
library(ISLR)
library(leaps)

# omitting na values
Hitters=na.omit(Hitters)
# checking if all na's have been removed
with(Hitters,sum(is.na(Salary)))

x=model.matrix(Salary~.-1,data=Hitters)
y=Hitters$Salary

# alpha=0 is ridge and alpha=1 is lasso
ridge=glmnet(x,y,alpha=0)
plot(ridge,xvar="lambda",label=TRUE)

# with cross validation
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

#### lasso regression, alpha=1
lasso=glmnet(x,y)
plot(lasso, xvar="lambda",label=TRUE)
plot(lasso, xvar="dev",label=TRUE)

# with cross-validation
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)

coef(cv.lasso)




## if we want to select lambda using train/ validation approach
set.seed(1)
train=sample(seq(263),180,replace=FALSE)
train

lasso=glmnet(x[train,],y[train])
lasso
prediction=predict(lasso,x[-train,])
dim(prediction)

rmse=sqrt(apply((y[-train]-prediction)^2,2,mean))
plot(log(lasso$lambda),rmse,type="b",xlab="log(lambda)")

lam.best=lasso$lambda[order(rmse)[1]]
lam.best
coef(lasso,s=lam.best)
