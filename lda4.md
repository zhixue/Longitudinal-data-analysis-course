<h2 style="text-align:center">Longitudinal Data Analysis Homework 4

Week 6

</h2>

### Problem 1
>  (2pts) Let Y be a Poisson random variable with mean `$\lambda$`,
```math
P(y ; \lambda)= \frac{\lambda ^y e^{-\lambda}}{y!}, y=0, 1, 2, ….
```
>Verify that it is from the exponential family. Derive the mean and variance of Y using the formula learned from the class, `$E(Y) = b^{'}(\theta)$` and `$Var(Y)=a(\phi)b^{''}(\theta)$`.

In the exponential family
```math
P(y ; \lambda) = exp\{Ylog(\lambda)-\lambda-log(Y!)\}
```
so that
```math
\theta = log(\lambda), b(\theta) = \lambda, a(\phi) = 1, c(Y,\phi) = log(Y!)
```
and
```math
E(Y) = b^{'}(\theta) = \frac{\partial \lambda}{\partial log(\lambda)} = {(\frac{\partial log(\lambda)}{\partial \lambda})}^{-1} = \lambda

Var(Y)=a(\phi)b^{''}(\theta) = \frac{\partial ^{2} \lambda}{\partial (log(\lambda))^2} = \frac{\partial \lambda}{\partial log(\lambda)} = \lambda
```
--------
### Problem 2
> (8pts) An experiment analyzes imperfection rates for two processes used to fabricate silicon wafers for computer chips. For treatment A applied to 10 wafers, the numbers of imperfections are 8, 7, 6, 6, 3, 4, 7, 2, 3, 4. Treatment B applied to 10 other wafers has 9, 9, 8, 14, 8, 13, 11, 5, 7, 6 imperfections. Treat the counts as independent Poisson variates having means `$\mu_A$` and `$\mu_B$`.  
a. Fit the model `$log(\mu) = \beta _1 + \beta _2 x$`, where x = 1 for treatment B and x = 0 for treatment A. Show that `$exp(\beta_2) = \mu_B / \mu_A$`, and interpret its estimate.  
b. Test `$H_0: \mu_A = \mu_B$` against `$H_0: \mu_A \neq \mu_B$`. Interpret.  
c. Construct a 95% confidence interval for `$\mu_B / \mu_A$`.  
d. Show that if Y1 and Y2 are independent Poisson variates with means `$\mu_1$` and `$\mu_2$`, then `$(Y_1|Y_1 + Y_2)$` is binomial with `$n = Y_1 + Y_2$` and `$\pi = \mu_1 / (\mu_1 + \mu_2)$`.    
(Can use the property that `$Y_1 + Y_2$` is Poisson with mean `$\mu_1 + \mu_2$`  
e. Test `$H_0: \mu_A = \mu_B$` based on the result in part d. (hint: if `$Y_1$`, `$Y_2$`, …, and `$Y_{10}$` are iid Poisson variates having mean `$\mu$`, then `$Y_1 + Y_2 +⋯+ Y_{10}$` is Poisson with mean `$10\mu$`.)

#### a

```math
log(\mu _A) = \beta _1, log(\mu _B) = \beta _1 + \beta _2 x

\beta _2 = log \frac{\mu _B}{\mu _A}

exp(\beta _2) = \frac{\mu _B}{\mu _A}
```

Interpret its estimate
```math
\widehat{\beta}_1 = 1.6094, \widehat{\beta}_2 = 0.5878
```
#### b

```math
\beta _2 \neq 0 (P<0.05)

\mu _A \neq \mu _B
```

#### c
95% confidence interval of 
```math
\frac{\mu_B}{\mu_A} \epsilon (1.280063, 2.560228)
```
#### d
let `$Z = Y_1 + Y_2$`
```math
P_{Y_1|Z=n}(k) = \frac{P(Y_1 = k, Z = n)}{P(Z = n)} 

= \frac{P(Y_1 = k, Y_2 = n - Y_1)}{P(Z = n)} 

= \frac{P(Y_1 = k) P(Y_2 = n - k)}{P(Z = n)} 

= \frac{\frac{\mu_1 ^ {k} e^{-\mu_1}}{k!} \frac{\mu_2 ^ {n-k} e^{-\mu_2}}{(n-k)!}}{\frac{(\mu_{1} + \mu_{2})^{n} e^{-(\mu_1+\mu_2)}}{n!}}

= C_n^k (\frac{\mu_1}{\mu_1 + \mu_2})^k (\frac{\mu_2}{\mu_1 + \mu_2})^{n-k}
```
So `$(Y_1|Y_1+Y_2)$` is bionomial with `$n=Y_1+Y_2$` and `$\pi=\mu_1/(\mu_1+\mu_2)$`

#### e
`$P<0.01$`,so `$\mu_A \neq \mu_B$`

----------
#### R code
```
dat = data.frame(cbind(c(8,7,6,6,3,4,7,2,3,4,9,9,8,14,8,13,11,5,7,6),c(rep(0,10),rep(1,10))))
colnames(dat) = c('u','x')

m1 = glm(u~x,family = poisson(link='log'), data = dat)
summary(m1)

```
```
##Call:
##glm(formula = u ~ x, family = poisson(link = "log"), data = dat)
##
##Deviance Residuals: 
##    Min       1Q   Median       3Q      Max  
##-1.5280  -0.7622  -0.1699   0.6938   1.5399  
##
##Coefficients:
##            Estimate Std. Error z value Pr(>|z|)    
##(Intercept)   1.6094     0.1414  11.380  < 2e-16 ***
##x             0.5878     0.1764   3.332 0.000861 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##(Dispersion parameter for poisson family taken to be 1)
##
##    Null deviance: 27.857  on 19  degrees of freedom
##Residual deviance: 16.268  on 18  degrees of freedom
##AIC: 94.349
##
##Number of Fisher Scoring iterations: 4

confint(m1)

##Waiting for profiling to be done...
##                2.5 %    97.5 %
##(Intercept) 1.3188383 1.8743819
##x           0.2469096 0.9400962

exp(confint(m1)[2,])
##   2.5 %   97.5 % 
##1.280063 2.560228

binom.test(sum(dat$u[1:10]), sum(dat$u), 0.5)
##	Exact binomial test
##
##data:  sum(dat$u[1:10]) and sum(dat$u)
##number of successes = 50, number of trials = 140, p-value = 0.0009131
##alternative hypothesis: true probability of success is not equal to 0.5
##95 percent confidence interval:
## 0.2780159 0.4424545
##sample estimates:
##probability of success 
##            0.3571429 
```