<h2 style="text-align:center">Longitudinal Data Analysis Homework 

Week 4

</h2>

### Problem 1
> (4pts) Using the dental study data, in class we fitted two models. One uses the common variance of the diagonal with-in child covariance matrix for both boys and girls; the other uses difference variances for boys and girls. We are just wondering which one is better. Please design a test to solve the problem. Please clearly write down the mean model, covariance model, and the null hypothesis. Use what you learned from the class to do the test. Report the result for the statistical test and the p value.

* Model a
```math
Y_{ij} = \beta_0 + \beta_1 male_{i} + \beta_2 time_{ij} + \beta_3 male_{i}time_{ij} + b_{1G}time_{ij} + b_{1B}time_{ij} + b_{0i} + \epsilon_{ij}

E(Y_{ij}) = \beta_0 + \beta_1 male_{i} + \beta_2 time_{ij} + \beta_3 male_{i}time_{ij}

\Sigma = \sigma ^2 I_n=
\left(
 \begin{matrix}
   \sigma ^2 & 0 & ... & 0\\
   0 & \sigma ^2 & ... & 0 \\
   ... & ... & ... & ... \\
   0 & 0 & ... & \sigma ^2
  \end{matrix} 
\right)
```



* Model b
```math
Y_{ij} = \beta_0 + \beta_1 male_{i} + \beta_2 time_{ij} + \beta_3 male_{i}time_{ij} + b_{1G}time_{ij} + b_{1B}time_{ij} + b_{0i} + \epsilon_{ij}

E(Y_{ij}) = \beta_0 + \beta_1 male_{i} + \beta_2 time_{ij} + \beta_3 male_{i}time_{ij}  

\Sigma = 
\left\{\begin{matrix}
 & \sigma^2_B I_{n},~ for~~boys\\
 & \sigma^2_G I_{n},~ for~~girls
\end{matrix}\right.
```
null hypothesis: `$ \beta_0 = \beta_1 = \beta_2 = \beta_3 = 0$`

* Result
```
             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
dental.lme.a     1  8 443.8060 465.2630 -213.9030                        
dental.lme.b     2  9 424.0424 448.1816 -203.0212 1 vs 2 21.76356  <.0001
```


According to the  AIC and BIC, **model b** is better.

* Rcode
```{r 1.1}
# load package 
library(nlme)
dental = read.table(file='dent.txt', header=T)
id <- factor(dental$id)
#outcome
dist <- dental$dist
male <- as.numeric(dental$gender < 2)
time <- dental$time
mtime <- male*time

#  (a) Common D matrix for both genders, default diagonal within-child                               
#      covariance matrix R_i with same variance sigma^2 for each gender.
dental.lme.a <- lme(dist ~ male + time + mtime,
              random = ~ time | id, method="ML")

#  (b) Common D matrix for both genders, diagonal within-child                    
#      covariance matrix R_i with different variance for each gender
dental.lme.b <- lme(dist ~ male + time + mtime,
                    random = ~ time | id, weights = varIdent(form = ~ 1 | male),
					method = "ML")

anova(dental.lme.a,dental.lme.b)
```

-----

### Problem 2
>  (6pts) Still using the dental study data, we consider the following mixed effect model

```math
Y_{ij} = \beta_0 + \beta_1 * male_i + \beta_2 * time_{ij} + \beta_3 * male_i * time_{ij} + b_{0i} + b_{1i} * time_{ij} + \epsilon_{ij}
```
> where `$\epsilon_i = (\epsilon_{i1}, \epsilon_{i2}, \epsilon_{i3}, \epsilon_{i4}) ～ N(0,R)$` with R in AR(1) structure, and `$b_i = (b_{0i},b_{1i}) ～ N(0,D)$` with D unstructured. We want to test if the random slope `$b_2$` is needed. Please report the result for the statistical test and the p-value.


* Result  

null hypothesis `$b_2$` = 0:
```
0.8331072<5.14
p = 0.5103454
```
According to the result(p-value>0.05), we should not reject the null  hypothesis. We concluded that the random slope is not needed.

* Rcode
```{r 1.2}
dental.lme.full <- lme(dist ~ male + time + mtime,
              random = ~ time | id, correlation=corAR1(form = ~ time | id),
			  method = "ML")
dental.lme.reduced <- lme(dist ~ male + time + mtime,
              random = ~ 1 | id, correlation=corAR1(form = ~ time | id),
			  method = "ML")

# likelihood ratio test
G2 <- 2*as.numeric(dental.lme.full$logLik - dental.lme.reduced$logLik)
print(G2)
## critical value is 5.14 for q=1

## For testing random effects, G2 statistic follows a 50:50 mixture of
## chisq(q) and chisq(q+1)
pvalue = 0.5*(1-pchisq(G2,1))+0.5*(1-pchisq(G2,2))
print(pvalue)
```