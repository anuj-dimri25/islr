# non linear models

require(ISLR)
attach(Wage)

# polynomial regression
model=lm(wage~poly(age,4),data=Wage)
summary(model)
# plot of fitted function
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
prediction=predict(model,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(prediction$fit+2*prediction$se,prediction$fit-2*prediction$se)
plot(age,wage,col="red")
lines(age.grid,prediction$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,col="blue",lty=2)


# alternative way
model2=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(model2)
# the plot shows both are fitted models are same as the plot is along the diagonal
plot(fitted(model),fitted(model2))

# using anova function to test nested sequence of models
fita=lm(wage~education,data=Wage)
fitb=lm(wage~education+age,data=Wage)
fitc=lm(wage~education+poly(age,2),data=Wage)
fitd=lm(wage~education+poly(age,3),data=Wage)
anova(fita,fitb,fitc,fitd)


## polynomial logistic regression
fit=glm(I(wage>250)~poly(age,3),data=Wage, family=binomial)
summary(fit)
prediction=predict(fit,list(age=age.grid),se=T)
se.bands=prediction$fit+cbind(fit=0,lower=-2*prediction$se,upper=2*prediction$se)
se.bands[1:5,]
# we have done the computations on the logit scale. To transform to probabilities we need to inverse the logit mapping
prob.bands=exp(se.bands)/(1+exp(se.bands))
matplot(age.grid,prob.bands,col="blue",lwd=c(2,1,1),lty=c(1,2,2),type="l",ylim=c(0,.1))
points(jitter(age),I(wage>250)/10,pch="I",cex=0.5)
