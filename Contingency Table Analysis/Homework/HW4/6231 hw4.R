
rm(list = ls())
setwd("E:/Data of R")

#Question 1
#(a)

y=c(0,0,0,0,1,1,1,1)
x1=c(1,2,3,3,5,6,10,11)

data11=data.frame(cbind(y,x1))

model11=glm(y~x1,data=data11,family = binomial(link = logit))
summary(model11)

#Coefficients and standard errors
coef(summary(model11))

exp(model11$coefficients)

p=predict(model11,type = 'response')
qqplot(x1,p)

#As can be seen from the plot, warning message means that 
#When y=1,pi_hat=1. When y=0, pi_hat=0
#Which is also an indication of complete seperation
#Another signal is the unnaturally large standard errors

#(b)
y=c(0,0,0,0,1,1,1,1)
x2=c(1,2,3,3,3,6,10,11)

data12=data.frame(cbind(y,x2))

model12=glm(y~x2,data=data12,family = binomial(link = logit))
summary(model12)

#Coefficients and standard errors
coef(summary(model12))

exp(model12$coefficients)

p=predict(model11,type = 'response')
qqplot(x2,p)

#As can be seen from the plot, warning message means that 
#Some observations have pi_hat=1 or 0, there is not  perfect discrimination
#Which is  an indication of quasi-complete separation
#Another signal is also the unnaturally large standard errors

#Question 3

data3=read.table("donner.txt",header = T)

#(a)
model3a=glm(survival~age,data=data3, family = binomial(link = logit))
summary(model3a)

#(b)
model3a$coefficients[2]
#Interpretation for 
#For every one year increase in age, the log odds of survial decreased by 0.03689

1/exp(model3a$coefficients[2])
#Interpretation for 
#For every one year increase in age, a person is 1.0375770 times less likely to survive

#(c)
#H0:
#Ha:

#Z=          =0.03689/(0.01493)=2.471>1.96

#P-value is 0.01346
#Reject H0

#(d)
coef(summary(model3a))

#Confidence Interval for
lb=coef(summary(model3a))[2,1]-qnorm(0.975)*coef(summary(model3a))[2,2]
ub=coef(summary(model3a))[2,1]+qnorm(0.975)*coef(summary(model3a))[2,2]
c(lb,ub)

#Confidence Interval for
exp(c(lb,ub))

#Effect of age on survial is statistically significant
#because the CI for    does not contain 0 and the CI for    does not contain 1

#(e)
age2=data3$age^2
model3b=glm(survival~age+age2+sex+status, data = data3, family = binomial(link = logit))
summary(model3b)

#(f)
#H0:reduced model
#Ha:full model

#

#Reject if 

diff.dev=deviance(model3a)-deviance(model3b)
diff.dev
qchisq(0.95,4)

#21.65525 > 7.814728
#Reject H0

#(g)
model3b$coefficients[4]
#Interpretation for 
#A male decreases the log odds of survival by 0.663728050

1/exp(model3b$coefficients[4])
#Interpretation for 
#A male is 1.942019 times less likely to survive

#Question 4
#(a)
H=rep(0,88)
for (i in 1:88) {
  if(data3$status[i]=='Hired'){
    H[i]=1
  }
}
S=rep(0,88)
for (i in 1:88) {
  if(data3$status[i]=='Single'){
    S[i]=1
  }
}
X=cbind(1,data3$age,age2,data3$sex,H,S)
fv=X%*%model3b$coefficients
summary(fv)

#(c)
pi_hat=exp(fv)/(1+exp(fv))
summary(pi_hat)

#(d)
#(1)
y_hat1=rep(0,88)
for(i in 1:88){
  if(pi_hat[i]>0.5){
    y_hat1[i]=1
  } else{
    y_hat1[i]=0
  }
}

T1=table(data3$survival,y_hat1)
T1

#(2)
y_hat2=rep(0,88)
for(i in 1:88){
  if(pi_hat[i]>(sum(data3$survival)/88)){
    y_hat2[i]=1
  } else{
    y_hat2[i]=0
  }
}

T2=table(data3$survival,y_hat2)
T2

#(3)
port1=T1[1,1]/88
port1
port2=(T2[1,2]+T2[2,1])/88
port2
#0.2613636 > 0.25
#Second cutoff is better

#(4)
FPR1=T1[1,2]/(sum(T1[1,]))
FNR1=T1[2,1]/(sum(T1[2,]))
FPR2=T2[1,2]/(sum(T2[1,]))
FNR2=T2[2,1]/(sum(T2[2,]))

#(5)
c(FPR1,FPR2)
#0.4102564 > 0.3076923
c(FNR1,FNR2)
#0.1428571 < 0.2040816
#It is hard to decide 

#(6)
library(pROC)
data4=data.frame(cbind(data3$survival,pi_hat,y_hat1,y_hat2))
roc(data4$V1,data4$V2,plot = TRUE)
points(1-FPR1,1-FNR1,col='red', cex=2, pch=21)
points(1-FPR2,1-FNR2,col='blue', cex=2, pch=24)

