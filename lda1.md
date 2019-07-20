
<h2 style="text-align:center">Longitudinal Data Analysis Homework 

Week 3

</h2>

### Problem 1
> 1. (5pts) Using the dental study data, we assume a homogeneous compound symmetric covariance with variance different by gender. We consider linear, quadratic, and cubic polynomial models, a linear spline with a single knot at age=10, and a restricted cubic spline, as in class. List the degrees of freedom, AICs and BICs for these models. Which model does AIC prefer? Which model does BIC prefer?

* Result
```
              Model df      AIC      BIC    logLik   Test   L.Ratio p-value
lme.lin           1  7 430.6521 449.4270 -208.3261                         
lme.quad          2  9 433.5072 457.6464 -207.7536 1 vs 2 1.1449323  0.5641
lme.cube          3 11 436.9947 466.4982 -207.4974 2 vs 3 0.5124447  0.7740
lme.linspline     4  9 433.0664 457.2056 -207.5332 3 vs 4 0.0716891  0.9648
lme.rcsp          5  9 433.5072 457.6464 -207.7536 
```
* Conclusion  

According to the result, AIC prefer **the linear model** and BIC prefer **the linear model** as well.

* Rcode
```{r 1.1}
# load package 
library(splines)
library(nlme)
# read data
dental <- read.table(file='dent.txt', header=T)
id <- dental$id
male <- as.numeric(dental$gender < 2)
female <- 1-male
maletime <- male*dental$time
femtime <- female*dental$time
dist <- dental$dist
time <- dental$time
```

```{r 1.1}
# a homogeneous compound symmetric covariance with variance different by gender
# linear
mtime <- male*time
lme.lin <- gls(dist~male+time+mtime, method="ML", correlation=corCompSymm(form=~1 | id),  weights = varIdent(form=~1 | male))

# quadratic
time2 <- time*time
mtime2 <- male*time2
lme.quad <- gls(dist~male+time+mtime+time2+mtime2, method="ML", correlation=corCompSymm(form=~1 | id),  weights = varIdent(form=~1 | male))

# cubic polynomial models
time3 <- time*time2
mtime3 <- male*time3
lme.cube <- gls(dist~male+time+mtime+time2+mtime2+time3+mtime3, method="ML", correlation=corCompSymm(form=~1 | id),  weights = varIdent(form=~1 | male))

# a linear spline with a single knot at age=10
timesp <- (time-10)*(time>10)
mtimesp <- male*timesp
lme.linspline <- gls(dist~male+time+timesp+mtime+mtimesp, method="ML", correlation=corCompSymm(form=~1 | id),  weights = varIdent(form=~1 | male))

# a restricted cubic spline
basis <- ns(time,df=2)
timercsp1 <- basis[,1]
timercsp2 <- basis[,2]
mtimercsp1 <- male*timercsp1
mtimercsp2 <- male*timercsp2
lme.rcsp <- gls(dist~male+timercsp1+timercsp2+mtimercsp1+mtimercsp2, method="ML", correlation=corCompSymm(form=~1 | id),  weights = varIdent(form=~1 | male))
anova(lme.lin,lme.quad,lme.cube,lme.linspline,lme.rcsp)
```

-----

### Problem 2
> 2. (5pts) Consider the restricted cubic spline model fitted in the previous problem. We are interested in the gender effect in the model. Please clearly write down the mean model, and the null hypothesis for the gender effect. Use what you learned from the class to do the test. Report the result for the statistical test.

* The mean model is 
```math
E(Yi|Xi) = \beta _0 + \beta _1 male+ \beta _2 timercsp1+\beta _3 timercsp2 + \beta _4 mtimercsp1 + \beta _5 mtimercsp2
```
* Null hypothesis for the gender effect(including the gender interaction) is
```math
H_0: \beta _1 = \beta _4 = \beta _5 = 0
```
* Result
```
                 Model df      AIC      BIC    logLik   Test  L.Ratio p-value
lme.rcsp             1  9 433.5072 457.6464 -207.7536                        
lme.rcsp.reduced     2  6 443.8316 459.9244 -215.9158 1 vs 2 16.32441   0.001
```
* Conclusion  

According to the result(p-value=0.001), we should reject H0 which means gender effect can not be ignored in the restricted cubic spline model.

* Rcode
```{r 1.2}
lme.rcsp.reduced <- gls(dist~timercsp1+timercsp2, method="ML", correlation=corCompSymm(form=~1 | id),  weights = varIdent(form=~1 | male))
anova(lme.rcsp,lme.rcsp.reduced)
```