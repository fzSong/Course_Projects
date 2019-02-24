
rm(list = ls())

#Question 2
#(b)

#Let S(p,y,n)=Score function=(y/p)-(n-y)/(1-p)
S=function(p,y,n){
  res=(y/p)-(n-y)/(1-p)
  return(res)
}

#Hessian matirxb 
H=function(p,y,n){
  res=-(y/p^2)-(n-y)/(1-p)^2
  return(res)
}

#Newton-Raphson Function 
#t=iterition times
#p0=starting value
NR=function(p0,y,n,t){
  p=p0
  for(i in 1:t){
    p=p-H(p,y,n)^(-1)*S(p,y,n)
    cat("p=",p,"t=",t,"\n")
  }
  return(p)
}

#(c)
#p_hat=0.3 n=10 implies y=3
#p0=(0.1,0.2,...,0.9)
#t=6

NR(0.1,3,10,6)
NR(0.2,3,10,6)
NR(0.3,3,10,6)
NR(0.4,3,10,6)
NR(0.5,3,10,6)
NR(0.6,3,10,6)
NR(0.7,3,10,6)
NR(0.8,3,10,6)
NR(0.9,3,10,6)

#(d)
#The more starting value get closed to true value, the faster speed of 
#convergence is

#(e)
#when pi_hat=0, implies that y/n=0, y=0
#Score function is -10/(1-p)
#Hessian matrix is -10/(1-p)^2
#p(t)=2*p(t-1)-1

#when pi_hat=1, implies that y/n=1, y=10
#Score function is 10/p
#Hessian matrix is -10/p^2
#p(t)=2*p(t-1)

#Thus, we can not have correct result because there is no convergence

#Question 5
#(a)

A=cbind(c(8,7,6,6,3,4,7,2,3,4),rep(0,10))
B=cbind(c(9,9,8,14,8,13,11,5,7,6),rep(1,10))
data=data.frame(rbind(A,B))
colnames(data)=c("Y","X")
model1=glm(Y~X,family = poisson(),data = data)
summary(model1)

#When x=0
#ua=e^a

#When x=1
#ub=e^(a+b)

#So ub/ua=e^b
exp(model1$coefficients[2])
ua=exp(model1$coefficients[1])
ub=exp(model1$coefficients[1]+model1$coefficients[2])
exp(model1$coefficients[2])==ub/ua

#Interpretion
#The coefficient for the intercept is 1.6094379. Thus the estimated 
#expectation for the number of seizures in Treatment A is e^1.6094379=5
#The estimated expectation for the number of seizures in Treatment B
#is e^(1.6094379+0.5877867)=9

#(c)
library(AER)
dispersiontest(model1,trafo = 1)

#Result shows that we can not reject c=0 for Var(y)=u+c*f(u)
#Which means Var(y)=u=E(y). There is no dispersion

#(e)
library(MASS)
model2=glm.nb(Y~X,data = data,link = log)
summary(model2)



#(f)

#Poisson
model1$coefficients[2]

#standard error is 0.1746

#Negative Binomial
model2$coefficients[2]

#standard error is 0.1746

#(g)
model3=glm(Y~1,family = poisson(),data = data)
summary(model3)

model4=glm.nb(Y~1,data = data,link = log)
summary(model4)

#(h)
dispersiontest(model3,trafo = 1)

#Result shows that we can not reject c=0 for Var(y)=u+c*f(u)
#Which means Var(y)=u=E(y). There is no dispersion