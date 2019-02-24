
rm(list = ls())
setwd("E:/Data of R")

#Question 2
library(VGAM)
data2=read.csv("GSS.csv",header = T)

names(data2)

data2

#(a)

model21=vglm(cbind(democrat,republican,independent)~gender+race,data = data2,
             family = multinomial)
model21
summary(model21)

#(b)
deviance(model21)
qchisq(0.05,4*2-3*2,lower.tail = FALSE)

#0.1982117<5.991465 
#We can not reject the null/reduced model in favor of the saturated model,
#indicating the model fit with main effects for all the predictors provides
#a reasonable fit.

#(c)
coefficients(model21)[3]
1/exp(coefficients(model21)[3])
#The estimated coefficient for the Gender Male dummy in the Democrat vs. Independent
#is -0.2202. This means that men are 1.246309 times less likely choose Democrat
#over Independent.

coefficients(model21)[4]
exp(coefficients(model21)[4])
#The estimated coefficient for the Gender Male dummy in the Republican vs. Independent
#is 0.3525732. This means that men are 1.422724  times more likely choose Republican
#over Independent.

#Gender effect is not significant overall. Gender has a statistically significant
#effect when comparing Republican over Independent.

#(d)

#Prob. being Independent for black females
pi3=1/(1+exp(1.3882465)+exp(-1.1771027))
pi3

#Prob. being Democrat for black females
pi1=exp(1.3882465)*pi3
pi1

#(e)
#Intercept1 is >0, which means that pi1/pi3>1, pi1>pi3
#Intercept2 is <0, which means that pi2/pi3<1, pi2<pi3
#So,pi1>pi3>pi2
#PiD_hat > PiI_hat > PiR_hat

#(f)
coefficients(model21)

c1=c(coefficients(model21)[1],coefficients(model21)[3],coefficients(model21)[5])
c1
#log(pi1/pi3)=1.3882465-0.2201865*male-1.1182884*white

c2=c(coefficients(model21)[2],coefficients(model21)[4],coefficients(model21)[6])
c2
#log(pi2/pi3)=-1.1771027+0.3525732*male+1.159845*white

#log(pi1/pi3)-log(pi2/pi3)=log(pi1/pi2)=c1-c2
c1-c2
#log=(PiD/PiR)=log(pi1/pi2)=2.5653492-0.5727597*male-2.2781343*white

#(g)
model22=vglm(cbind(democrat,independent,republican)~gender+race,data = data2,
             family = multinomial)
summary(model22)

coefficients(model22)
#log=(PiD/PiR)=log(pi1/pi2)=2.5653492-0.5727597*male-2.2781343*white
#It is the same as the result in part (f)

#(h)
# Democrat vs. Independence
d1=cbind(rep(1,259),rep(1,259),c(rep(1,132),rep(0,127)))
d2=cbind(rep(1,54),rep(0,54),c(rep(1,42),rep(0,12)))
d3=cbind(rep(0,302),rep(1,302),c(rep(1,172),rep(0,130)))
d4=cbind(rep(0,71),rep(0,71),c(rep(1,56),rep(0,15)))
data21=rbind(d1,d2,d3,d4)
colnames(data21)=c("gender","race","democrat")
data21=data.frame(data21)
logit1=glm(democrat~gender+race,data=data21,family=binomial(link = logit))

# Republican vs. Independence
d1=cbind(rep(1,303),rep(1,303),c(rep(1,176),rep(0,127)))
d2=cbind(rep(1,18),rep(0,18),c(rep(1,6),rep(0,12)))
d3=cbind(rep(0,259),rep(1,259),c(rep(1,129),rep(0,130)))
d4=cbind(rep(0,19),rep(0,19),c(rep(1,4),rep(0,15)))
data22=rbind(d1,d2,d3,d4)
colnames(data22)=c("gender","race","republican")
data22=data.frame(data22)
logit2=glm(republican~gender+race,data=data22,family=binomial(link = logit))

summary(logit1)
summary(logit2)
summary(model21)

#(i)
#The coefficients and their statistical significance in two separate logistic
#models are the same as the corresponding parts of the baseline category logit
#model. That is because the submodel of the baseline category logit is exactly
#the logistic model. Taking first logistic model as a example, the sumation
#of y=0 is exactly the number of people who choose independent

#Question 3
library(gee)
data3=read.csv("attitudes.csv", header = T)
names(data3)
dim(data3)

#(a)
model31=gee(response~gender+as.factor(question),id=case,family=binomial,
            corstr="unstructured", scale.fix=T,data = data3)
summary(model31)

#(b)
model31$working.correlation
#The working correlation indicates that in the same case,the correlation between 1 and 2 is
#0.8248498, the correlation between 1 and 3 is 0.7958825 and the correlation
#between 2 and 3 is 0.8312594

#These estimation is large (closing to 1), which indicates that accounting
#for clustering is necessary

#(c)
model32=gee(response~gender+as.factor(question),id=case,family=binomial,
            corstr="exchangeable", scale.fix=T,data = data3)

model31$working.correlation
model32$working.correlation
#The working correlation matrix indicates that the correlation between observations 
#within a subject is estiamted to be 0.8173308
#Because we spwcified an exchangeable correlation structure, this correlation 
#is the same for all pairs in a group 

#0.8173308 is between 0.8248498 and 0.7958825 and it is also closed to either
#of them, which indicates that is reasonable to use exchangeable correlation

#(d)
summary(model32)
exp(coefficients(model32)[2])
#The estimated coefficient for the gender is 0.003437873
#This means that females are 1.003444 times more likley to support legalized abortion

qnorm(0.975)

#Naive z 0.03912248 < 1.959964
#Can not reject H0

#(e)
lb=0.003437873-qnorm(0.975)*0.08787462
ub=0.003437873+qnorm(0.975)*0.08787462

#the 95% CI for     is
c(lb,ub)

#the 95% CI for     is
c(exp(lb),exp(ub))

#(f)
#response=0.024021377+0.003437873*female-0.097329120*Q2-0.149347107*Q3

#estimated odds of support for legalized abortion in scenario 2 for a male
odds=exp(0.024021377-0.097329120)
odds

#(g)
model33=gee(response~gender+question,id=case,family=binomial,
             corstr="exchangeable", scale.fix=T,data = data3)
summary(model33)

#Comparing to part(c)
summary(model32)

#(h)
model34=glm(response~gender+as.factor(question),family=binomial,data=data3)
summary(model34)

#Comparing to part(c)
summary(model32)

#The coefficents estimates of these two models are the same
#The standard error of exchangeable model is smaller than the standard error
#of independence model

#(i)
#It is because that as for GEE, there is a large correlation within subject, so
#standard error is smaller than GLM. However, using the same data, the total
#standard errors are always the same. Therefore, GEE has larger between-subject
#standard error.


#