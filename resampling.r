# leave one out cross validation
require(ISLR)
require(boot)

?cv.glm()
plot(mpg~horsepower,data=Auto)

model=glm(mpg~horsepower, data=Auto)

# delta is crossvalidation prediction error
cv.glm(Auto,model)$delta


loocv=function(fit)
{
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

loocv(model)

# trying out different degrees of model
# array of 5 indexes... all 0's
cv.error=rep(0,5)
degree=1:5
for (d in degree)
{
  model=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(model)
}
plot(degree,cv.error,type="b")

# 10 fold cross validation
cv.error10=rep(0,5)
for (d in degree)
{
  model=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,model,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

### Bootstrap
# min risk investment - section 5.2

alpha=function(x,y)
{
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy) # last computed value is the return value
}

alpha(Portfolio$X,Portfolio$Y)

# implementing bootstrap
alpha.fn=function(data,index)
{
  with(data[index,],alpha(X,Y)) # using "with" we get X and Y present in data frame at the corresponding index number
}

alpha.fn(Portfolio,1:100)

# bootstrap involves random sample, therefore use seed for replication
set.seed(7)
# sample selects 100 numbers between 1:100 with replacement
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

# using boot function
boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)
