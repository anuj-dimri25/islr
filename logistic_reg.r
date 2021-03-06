require(ISLR) # similar to library
names(Smarket)

# predicting direction -- binary response

# plotting all variables
pairs(Smarket,col=Smarket$Direction)

#logistic regression
model=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
          data=Smarket, family=binomial)

summary(model)
prediction=predict(model,type="response")
prediction[1:10]
final_predictions=ifelse(prediction>0.5,"Up","Down")\

attach(Smarket)

# table of training prediction
table(final_predictions,Direction)
mean(final_predictions==Direction)


###### training and test set
train=Year<2005
model2=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
          data=Smarket, family=binomial, subset=train)

summary(model2)
prediction2=predict(model2,newdata=Smarket[!train,], type="response")
prediction2[1:10]

final_predictions2=ifelse(prediction2>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]

# table of training prediction
table(final_predictions2,Direction.2005)
mean(final_predictions2==Direction.2005)
## output shows we are overfitting


# lets use a smaller model
model3=glm(Direction~Lag1+Lag2,
           data=Smarket, family=binomial, subset=train)

summary(model3)
prediction3=predict(model3,newdata=Smarket[!train,], type="response")
prediction3[1:10]

final_predictions3=ifelse(prediction3>0.5,"Up","Down")

# table of training prediction
table(final_predictions3,Direction.2005)
mean(final_predictions3==Direction.2005)


