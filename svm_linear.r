### svm ####

set.seed(1021)
x=matrix(rnorm(40),20,2)
y=rep(c(-1,1),c(10,10))

x[y==1,]=x[y==1,]+1
plot(x,col=y+3,pch=19)

library(e1071)
input=data.frame(x,y=as.factor(y))
model=svm(y~.,data=input,kernel="linear",cost=10,scale=FALSE)
print(model)

plot(model,input)

# making a better plot
make.grid=function(x,n=75)
{
  gridrange=apply(x,2,range)
  x1=seq(from=gridrange[1,1],to=gridrange[2,1],length=n)
  x2=seq(from=gridrange[1,2],to=gridrange[2,2],length=n)
  expand.grid(X1=x1,X2=x2)
}

xgrid=make.grid(x)
ygrid=predict(model,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=19,cex=0.1)
points(x,col=y+3,pch=19)
#support vectors
points(x[model$index,],pch=5,cex=2)

