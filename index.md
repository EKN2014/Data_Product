---
title       : Exploring the relationship between miles per gallon (MPG) and predictor variables 
subtitle    : Analysis of mtcars data
author      : Elizabeth
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

Executive Summary

The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models). 
Source: Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391-411. 


```r
library(datasets);library(knitr)
```

Determine the structure of the data

```r
str(mtcars)
```

```
## 'data.frame':	32 obs. of  11 variables:
##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
##  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
##  $ disp: num  160 160 108 258 360 ...
##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
##  $ qsec: num  16.5 17 18.6 19.4 17 ...
##  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
##  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
##  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
##  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
```

---  

EXPLORE THE DATA

First get descriptive statistics of all the variable. For numeric variable, the mean is calculated and for factor variable, the frequency is displayed


```r
library(plyr)
sum <- ddply(mtcars, c("am"), summarise,
               N    = length(mpg),
               mean = mean(mpg),
               sd   = sd(mpg),
               se   = sd / sqrt(N) )
sum
```

```
##   am  N     mean       sd        se
## 1  0 19 17.14737 3.833966 0.8795722
## 2  1 13 24.39231 6.166504 1.7102804
```

--- 
 
FITTING A MULTIPLE REGRESSION

First do residual analysis

```r
data(mtcars)
y <- mtcars$mpg; x <- mtcars$am; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
```

```
## [1] 4.840572e-14
```

Secondly quantify the uncertainty and perform Confidence interval


```r
fit <- lm(y ~ x);
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
```

```
## [1] 14.85062 19.44411
```

--- 

Fitting the model

```r
fitmodel = summary(lm(mpg~ am+wt+qsec, data=mtcars))
fitmodel
```

INTERPRETATION OF RESULTS

After fitting different models, the results show only weight(wt),qsec and transmission design have a signficant contribution to mileage per gallon. wt and qsec are significant at less than 0.001. The coefficient of weight(wt) is negative. This indicates increase weight by one more pound in 1000 will reduce mileage per gallon by 3.9165. This means more fuel is consumed when the weight is higher. qsec has a positive estimate indicating a positive relation with fuel consumption.
