# model selection
library(ISLR)
summary(Hitters)

# omitting na values
Hitters=na.omit(Hitters)
# checking if all na's have been removed
with(Hitters,sum(is.na(Salary)))

# best subset regression
library(leaps)
model=regsubsets(Salary~.,data=Hitters)
summary(model)

# by default it only goes to subset of size 8.
# but we have 19 variables

model19=regsubsets(Salary~.,data=Hitters,nvmax=19)
summary(model19)
names(summary(model19))

# plotting
plot(summary(model19)$cp,xlab="number of variables",yla ="Cp statistic")
# using model with lowest Cp
which.min(summary(model19)$cp)
points(10,summary(model19)$cp[10],pch=20,col="red")

# plotting all models with Cp
plot(model19,scale="Cp")
coef(model19,10)


