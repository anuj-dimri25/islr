linear\_reg
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

``` r
library(MASS)
#install.packages("ISLR")
library(ISLR)
```

``` r
names(Boston)
```

    ##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
    ##  [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"

``` r
plot(medv~lstat,Boston)
```

![](linear_reg_notebook_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
plot(lstat~medv,Boston)
```

![](linear_reg_notebook_files/figure-markdown_github/unnamed-chunk-2-2.png)

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file).
