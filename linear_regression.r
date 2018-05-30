library(MASS)
install.packages("ISLR")
library(ISLR)
names(Boston)
plot(medv~lstat,Boston)
plot(lstat~medv,Boston)

# single linear reg
linear_fit=lm(medv~lstat, data=Boston)
summary(linear_fit)
abline(linear_fit,col="red")

predict(linear_fit,data.frame(lstat=c(15,20,25)), interval = "confidence")

par(mfrow=c(2,2))
plot(linear_fit)

# multiple linear regression
linear_fit2=lm(medv~lstat+age,data=Boston)
summary(linear_fit2)
par(mfrow=c(2,2))
plot(linear_fit2)

linear_fit3=lm(medv~., data=Boston)
summary(linear_fit3)
par(mfrow=c(2,2))
plot(linear_fit3)

# removing columns with high p-value
linear_fit4=update(linear_fit3,~.-age-indus)
summary(linear_fit4)

# non linear terms and interactions (*)
linear_fit5=lm(medv~lstat*age,Boston)
summary(linear_fit5)

linear_fit6=lm(medv~lstat+I(lstat^2),Boston)
summary(linear_fit6)

# making boston dataset a default one
attach (Boston)

par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(linear_fit6),col="blue",pch=22)

linear_fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(linear_fit7),col="green",pch=24)