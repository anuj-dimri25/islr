# decision trees
require(ISLR)
require(tree)
attach(Carseats)

hist(Sales)
high=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,high)

# model 
model=tree(high~.-Sales,data=Carseats)
summary(model)
plot(model)
text(model,pretty=1)

# detailed model
model

set.seed(10)
train=sample(1:nrow(Carseats),250)
model=tree(high~.-Sales,Carseats,subset=train)
plot(model)
text(model,pretty=1)

# prediction
predictions=predict(model,Carseats[-train,],type="class")
with(Carseats[-train,],table(predictions,high))

#accuracy
(73+43)/150

# using CV to prune tree
cv.model=cv.tree(model,FUN=prune.misclass)
cv.model
plot(cv.model) # min value is at 13

prune.model=prune.misclass(model,best=13)
plot(prune.model)
text(prune.model,pretty=1)

predictions=predict(prune.model,Carseats[-train,],type="class")
with(Carseats[-train,],table(predictions,high))

#accuracy after pruning
(76+40)/150

