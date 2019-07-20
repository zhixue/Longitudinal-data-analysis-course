<h2 style="text-align:center">Longitudinal Data Analysis Homework 3

Week 5

</h2>

> You are given a hypothetical dataset which mimics a randomized trial for schizophrenia. In this pilot data, 53 patients received drug therapy and the rest 53 patients received placebo. Measurements at weeks 0, 1, 2, 4 are taken. The outcome is severity of illness.

> The data is contained in dat.RDS. You can import the data by dat = readRDS(file = "dat.RDS").

> Consider model M1 that

```math
y_{ij}=\beta_0+\beta_1 t_{ij}+\beta_2 z_i t_{ij}+b_{0i}+b_{1i}t_{ij}+\epsilon _{ij}
```
> for subject i=1,…,N and times j=1,…,J, where `$\epsilon_i∼N(0,\sigma_2I)$` and `$bi∼N(0,D)$`.


> 1. Visualize mean severity over time by treatment group. If we will use M1, determine whether we shall take square root of time based on the visualization plot.

![VZp62R.png](https://s2.ax1x.com/2019/05/27/VZp62R.png)
![VZphVO.png](https://s2.ax1x.com/2019/05/27/VZphVO.png)

#### Conclusion
We shall take square root of time with similar time intervals.
#### R code
```
library(lme4)
dat = readRDS(file = "dat.RDS")
group_mean = tapply(dat$y,list(dat$time,dat$trt1),mean)
print(group_mean)
##         0        1
##0 5.995476 5.947263
##1 5.813797 5.543841
##2 5.777614 5.329954
##4 5.714166 5.022061
# mean_y - time
plot(c(0,1,2,4),group_mean[,2],col='red',type='b',xlab = 'time',ylab = 'mean_y',ylim=c(4,7),pch=16)
lines(c(0,1,2,4),group_mean[,1],col='blue',type='b',pch=16)
legend(3.5,7,c('trt=1','trt=0'),col=c('red','blue'),pch=c(16,16))
# mean_y - sqr(time)
plot(c(0,1,sqrt(2),2),group_mean[,2],col='red',type='b',xlab = 'sqrt(time)',ylab = 'mean_y',ylim=c(4,7),pch=16)
lines(c(0,1,sqrt(2),2),group_mean[,1],col='blue',type='b',pch=16)
legend(1.5,7,c('trt=1','trt=0'),col=c('red','blue'),pch=c(16,16))
```
> 2. Fit M1 and use the scale of tij based on your decision in 1 (either original scale or square root). Report estimated average model and estimated model for subject id=1 and time=3.

####  Estimated average model 
```math
E(y) = 
\left\{
\begin{matrix}
\beta_0+\sqrt3 \beta_1  + \sqrt3 b_{11}, trt=0 \\
\beta_0+\sqrt3 \beta_1+\sqrt3\beta_2 +\sqrt3 b_{11}, trt=1
\end{matrix}
\right.
```
```math
\widehat{\beta}_0 = 5.9749  , \widehat{\beta}_1 = -0.1404, \widehat{\beta}_2 = -0.3206
```

id=1,time=3,trt=0 , y= 5.333423 ;
id=1,time=3,trt=1 , y= 4.778088


#### R code
```
# square root
dat$sqrttime = sqrt(dat$time)
```
```
dat$trtsqrttime = dat$sqrttime*dat$trt1
m1_s = lmer(y~sqrttime+trtsqrttime+(1+sqrttime|person),data=dat)
summary(m1_s)
##Linear mixed model fit by REML ['lmerMod']
##Formula: y ~ sqrttime + trtsqrttime + (1 + sqrttime | person)
##   Data: dat
##REML criterion at convergence: 1021.574
##Random effects:
## Groups   Name        Std.Dev. Corr 
## person   (Intercept) 1.0359        
##          sqrttime    0.7894   -0.12
## Residual             0.3981        
##Number of obs: 424, groups:  person, 106
##Fixed Effects:
##(Intercept)     sqrttime  trtsqrttime  
##    5.9749      -0.1404      -0.3206 
```
```
# estimate 
new_point = data.frame(sqrttime = c(sqrt(3),sqrt(3)),person=c(1,1),trtsqrttime=c(0,1*sqrt(3)))
predict(m1_s,new_point)
##       1        2 
##5.333423 4.778088 
```
------
> 3. For the fitted model M1, report estimated D and σ2.

#### R code
```
VarCorr(m1_s)$person # D
##            (Intercept)   sqrttime
##(Intercept)   1.0730301 -0.1013056
##sqrttime     -0.1013056  0.6230953
sigma(m1_s)^2 # sigma^2
##[1] 0.1584848
```
------
> 4. Consider this hypothetical dataset as a pilot study. 
----
> Suppose we are planning for a larger trial to study the treatment effect. Half of the patients will receive treatment and the rest half will receive placebo. The investigators expect a slight decrease for control group due to placebo effects and a much faster decrease for the treatment group. It is suggested that β1=−0.1 and β2=−0.3. Use β0 and variance estimated from the pilot data under model M1. Provide sample size for achieving 80% power. Please use random seeds to ensure reproducibility. Hint:   
(1) as direction of treatment effect is clear, use one-sided test. You need to modify the example code such that you’re testing for β2<0.   
(2) Here the scale of tij is based on your decision in 1 (either original scale or square root). The time scale should be consistent with how you fit the pilot data.

#### Conclusion
For achieving 80% power, samples size is required at least 230. 

#### R code
```
library(MASS)
library(lme4)
set.seed(1)
# sim function
m1_s.sim = function(n,J){
  time<-rep(seq(0,1,length=J),n)
  sqrttime = sqrt(time)
  person<-rep(1:n, each=J)
  trt<-sample(rep(0:1,n/2)) #randomize half to treatment
  trt1<-trt[person] #extend trt to each subject’s repeated measures (same trt over time) 
  #trttime=time*trt1
  trtsqrttime = sqrttime*trt1
  
  #SET INPUT PARAMETERS HERE!!!!!!(changed)
  beta.0.true<-5.9749#
  beta.1.true<-(-0.1)#
  beta.2.true<-(-0.3)#
  sigma.true<-sqrt(0.1584848)#
  D.true<-cbind(c(1.0730301, -0.1013056),c(-0.1013056, 0.6230953))#
  b.mean.true<-c(0,0)

  #Random effects
  b.true<-mvrnorm(n,b.mean.true,D.true)
  int=b.true[,1]
  slope=b.true[,2]
  
  #Y’s 
  y<-rnorm(n*J,beta.0.true+int[person]+sqrttime*(beta.1.true+beta.2.true*trt1+slope[person]),sigma.true) 
  return(data.frame(y,sqrttime,person,trt1,trtsqrttime))
} 

## power function
m1_s.power<-function(n,J,n.sims=1000) {
  signif<-rep(NA,n.sims) #set up vector to see whether expected effect is detected 
  for (s in 1:n.sims) {
  dataset<-m1_s.sim(n,J) #generate data for simulation s 
  lme.power<-lmer(y~sqrttime+trtsqrttime+(1+sqrttime|person),data=dataset) 
  theta.hat<-fixef(lme.power)["trtsqrttime"] #pull off point estimate of interest 
  se.fixef<-sqrt(diag(vcov(lme.power)))
  theta.se<-se.fixef[3] #pull off corresponding SE 
  signif[s]<-(theta.hat+1.645*theta.se)<0 #0.05 one tail:1.645
  
  }
  power<-mean(signif) #proportion of TRUE 
  return(power)
}

### run
m1_s.power(220,4)
##[1] 0.792
m1_s.power(230,4)
##[1] 0.823
```