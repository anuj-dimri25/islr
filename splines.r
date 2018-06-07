## splines-- more flexible than polynomials

require(splines)
require(ISLR)
attach(Wage)

model=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
plot(age,wage,col="darkgrey")

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
lines(age.grid,predict(model,list(age=age.grid)),col="darkgreen",lwd=2)
# plotting knots (place of discontinuity)
abline(v=c(25,40,60),lty=2,col="darkgreen")


# smoothing splines
model=smooth.spline(age,wage,df=16)
lines(model,col="red",lwd=2)

# LOOCV for smoothing parameter
model=smooth.spline(age,wage,cv=TRUE)
lines(model,col="purple",lwd=2)


#### generalized additive models
require(gam)
gam1=gam(wage~s(age,df=4)+s(year,df=4)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam1,se=T)

gam2=gam(I(wage>250)~s(age,df=4)+s(year,df=4)+education,data=Wage, family = binomial)
plot(gam2)

### checking whether we need a non linear term for education
gam2a=gam(I(wage>250)~s(age,df=4)+year+education,data=Wage, family = binomial)
anova(gam2a,gam2,test="Chisq")
# high p value means we do not need a non linear term for education

#########
par(mfrow=c(1,3))
lm1=lm(wage~ns(age,df=4)+ns(year,df=4)+education,data=Wage)
plot.Gam(lm1,se=T)
