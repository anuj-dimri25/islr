require(ISLR)
require(MASS)

#linear discriminant analysis
model=lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
model
plot(model)

#test data
Smarket.2005=subset(Smarket,Year==2005)

prediction=predict(model,Smarket.2005)
class(prediction)

# convenient way of looking the list is through data frame
data.frame(prediction)[1:5,]

# confusion matrix
table(prediction$class,Smarket.2005$Direction)

# correct mean
mean(prediction$class==Smarket.2005$Direction)