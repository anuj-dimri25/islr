k nearest neighbor
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

``` r
# k nearest neighbors
library(MASS)
#install.packages("ISLR")
library(ISLR)
library(class)
?knn
attach(Smarket)
xlag=cbind(Lag1, Lag2)
train=Year<2005
```

``` r
# 1 nearest neighbor
prediction=knn(xlag[train,],xlag[!train,],Direction[train],k=1)

table(prediction,Direction[!train])
```

    ##           
    ## prediction Down Up
    ##       Down   43 58
    ##       Up     68 83

``` r
mean(prediction==Direction[!train])
```

    ## [1] 0.5

``` r
# 3 nearest neighbor
prediction=knn(xlag[train,],xlag[!train,],Direction[train],k=3)

table(prediction,Direction[!train])
```

    ##           
    ## prediction Down Up
    ##       Down   48 56
    ##       Up     63 85

``` r
mean(prediction==Direction[!train])
```

    ## [1] 0.5277778

``` r
# 5 nearest neighbor
prediction=knn(xlag[train,],xlag[!train,],Direction[train],k=5)

table(prediction,Direction[!train])
```

    ##           
    ## prediction Down Up
    ##       Down   40 59
    ##       Up     71 82

``` r
mean(prediction==Direction[!train])
```

    ## [1] 0.484127

``` r
print ("Accuracy increases from k=1 to k=3, but it decreases when k=5 (overfitting)")
```

    ## [1] "Accuracy increases from k=1 to k=3, but it decreases when k=5 (overfitting)"

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file).
