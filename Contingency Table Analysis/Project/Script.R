
rm(list=ls())

data=matrix(c(108,19,107,40),2,2,byrow= TRUE)
row.names(data)=c("experimental treatment","control treatment")
colnames(data)=c("success","failure")
data

###Bayesian Theory
#Prior:Pi1~Beta(a,b)
#Prior:Pi2~Beta(c,d)
a=1
b=1
c=1
d=1
y1=data[1,1]
n1=sum(data[1,])
y2=data[2,1]
n2=sum(data[2,])
#Posterior: Pi1~Beta(a1,b1)
#Posterior: Pi2~Beta(c1,d1)
a1=y1+a
b1=n1-y1+b
c1=y2+c
d1=n2-y2+d

#Simulation of finding P(Pi1>=Pi2)
n=10000
S1=rbeta(n,a1,b1)
S2=rbeta(n,c1,d1)
sum=0
for(i in 1:n){
  if(S1[i]>=S2[i]){
    sum=sum+1
  }
}
P=sum/n
P

#Credible Interval of Difference in Proportions in Bayesian Theory
 
# approximate tail method using simulation
diff.app<- function(a1,b1,c1,d1,conflev,nsim=100000)
  { z1 <- rbeta(nsim, a1,b1)
    z2 <- rbeta(nsim, c1,d1)
    z <- z1 - z2
    z <- sort(z)
    lq <- nsim * (1-conflev)/2
    uq <- nsim * (1 - (1-conflev)/2)
    ci <- array(0,2)
    ci[1] <- z[lq]
    ci[2] <- z[uq]
  return(ci) }

# Bayes equal tail interval with beta priors
fct.F1<- function(x,t,a1,b1,c1,d1){dbeta(x,c1,d1)*pbeta(x+t,a1,b1)}
fct.F2<- function(x,t,a1,b1,c1,d1){dbeta(x,c1,d1)*(1-pbeta(x+t,a1,b1))}

diff.F <- function(t,a1,b1,c1,d1){
  if(t < 0)
    Fvalue <- integrate(fct.F1,-t,1,t=t,a1=a1,b1=b1,c1=c1,d1=d1)$value
  else
    Fvalue <- 1-integrate(fct.F2,0,1-t,t=t,a1=a1,b1=b1,c1=c1,d1=d1)$value
  return(Fvalue) }

diff.fct <- function(ab,a1,b1,c1,d1,conflev){
  abs(diff.F(ab[2],a1,b1,c1,d1) - (1 - (1-conflev)/2))
  +abs(diff.F(ab[1],a1,b1,c1,d1) - (1-conflev)/2) }

diffCI <- function(x1,n1,x2,n2,a,b,c,d,conflev=.95){
  a1 <- a + x1
  b1 <- b + n1 - x1
  c1 <- c + x2
  d1 <- d + n2 - x2
  start <- diff.app(a1,b1,c1,d1,conflev)
  tailci <- optim(start,diff.fct,a1=a1,b1=b1,c1=c1,d1=d1,
                conflev=conflev,control=list(maxit=20000))$par
  if(tailci[1] < -1) tailci[1]  <- -1
  if(tailci[2] >  1) tailci[2]  <- 1
  return(tailci)
}

diff.app(a1,b1,c1,d1,0.95)

CI_diff_B=diffCI(y1,n1,y2,n2,a,b,c,d)
CI_diff_B

#HPD Intervals
# Bayes HPD interval with beta priors
fct.F1<- function(x,t,a1,b1,c1,d1){
  dbeta(x,c1,d1)*pbeta(x+t,a1,b1)}

fct.F2<- function(x,t,a1,b1,c1,d1){
  dbeta(x,c1,d1)*(1-pbeta(x+t,a1,b1))}

diff.F <- function(t,a1,b1,c1,d1)
{ if(t < 0)
  Fvalue <- integrate(fct.F1,-t,1,t=t,a1=a1,b1=b1,c1=c1,d1=d1)$value
else
  Fvalue <- 1-integrate(fct.F2,0,1-t,t=t,a1=a1,b1=b1,c1=c1,d1=d1)$value
return(Fvalue)
}

fct.f<- function(x,t,a1,b1,c1,d1){
  dbeta(x,c1,d1)*dbeta(x+t,a1,b1)
}

diff.f <- function(t,a1,b1,c1,d1)
{
  if(t < -1) fvalue <- 100
  else if(t > 1)  fvalue <- 100
  else if((t >= -1) && (t <= 0))
    fvalue <- integrate(fct.f,-t,1,t=t,a1=a1,b1=b1,c1=c1,d1=d1)$value
  else
    fvalue <- integrate(fct.f,0,1-t,t=t,a1=a1,b1=b1,c1=c1,d1=d1)$value
  return(fvalue)
}


diff <- function(ab,a1,b1,c1,d1,conflev)
{
  1000*abs(diff.F(ab[2],a1,b1,c1,d1) -
             diff.F(ab[1],a1,b1,c1,d1) - conflev)+
    abs(diff.f(ab[1],a1,b1,c1,d1) -
          diff.f(ab[2],a1,b1,c1,d1))
}

diffCIhpd <- function(x1,n1,x2,n2,a,b,c,d,conflev=.95)
{
  y <- x1
  if( y > n1/2 ) {
    x2 <- n2-x2
    x1 <- n1-x1
  }
  a1 <- a + x1
  b1 <- b + n1 - x1
  c1 <- c + x2
  d1 <- d + n2 - x2
  start <- diff.app(a1,b1,c1,d1,conflev)
  hdrci <- optim(start,diff,a1=a1,b1=b1,c1=c1,d1=d1,conflev=conflev)$par
  if(hdrci[1] < -1) hdrci[1]  <- -1
  if(hdrci[2] >  1) hdrci[1]  <- 1
  if( y > n1/2 ) {
    ci <- hdrci
    hdrci[1] <- -ci[2]
    hdrci[2] <- -ci[1]
  }
  return(hdrci)
}

HPD_diff_B=diffCIhpd(y1,n1,y2,n2,a,b,c,d)
HPD_diff_B
  
#Credible Interval of Relative Risk in Bayesian Theory

# approximate tail method using simulation
risk.app<- function(a1,b1,c1,d1,conflev,nsim=100000)
{
  z1 <- rbeta(nsim, a1,b1)
  z2 <- rbeta(nsim, c1,d1)
  z <- z1/z2
  z <- sort(z)
  lq <- nsim * (1-conflev)/2
  uq <- nsim * (1 - (1-conflev)/2)
  ci <- array(0,2)
  ci[1] <- z[lq]
  ci[2] <- z[uq]
  return(ci)
}

# Bayes tail interval with beta priors

fct.F1<- function(x,t,a1,b1,a2,b2){
  dbeta(x,a2,b2)*pbeta(x*t,a1,b1)}

fct.F2<- function(x,t,a1,b1,a2,b2){
  dbeta(x,a2,b2)*(1-pbeta(x*t,a1,b1))}

risk.F <- function(t,a1,b1,a2,b2)
{
  if((0<t) && (t<=1)){
    return(integrate(fct.F1,0,1,t=t,a1=a1,b1=b1,a2=a2,b2=b2)$value)
  }
  else{
    return(1-integrate(fct.F2,0,1/t,t=t,a1=a1,b1=b1,a2=a2,b2=b2)$value)
  }
}

risk.fct <- function(ab,a1,b1,c1,d1,conflev)
{
  abs(risk.F(ab[2],a1,b1,c1,d1) - (1 - (1-conflev)/2)) +
    abs(risk.F(ab[1],a1,b1,c1,d1) -(1-conflev)/2)
}

riskCI <- function(x1,n1,x2,n2,a,b,c,d,conflev=.95)
{
  a1 <- a + x1
  b1 <- b + n1 - x1
  c1 <- c + x2
  d1 <- d + n2 - x2
  start <- risk.app(a1,b1,c1,d1,conflev)
  tailci <- optim(start,risk.fct,a1=a1,b1=b1,c1=c1,d1=d1,
                  conflev=conflev,control=list(maxit=20000))$par
  if(tailci[1] < 0) tailci[1]  <- 0
  return(tailci)
}

risk.app(a1,b1,c1,d1,0.95)
CI_RR_B=riskCI(y1,n1,y2,n2,a,b,c,d)
CI_RR_B

#Credible Interval of Odds Ratio in Bayesian Theory

# approximate tail method using simulation

or.app<- function(a1,b1,c1,d1,conflev,nsim=1000000)
{
  z1 <- rf(nsim, 2*a1,2*b1)
  z2 <- rf(nsim, 2*c1,2*d1)
  a <- (d1/c1)/(b1/a1)
  z <- a*z1/z2
  z <- sort(z)
  lq <- nsim * (1-conflev)/2
  uq <- nsim * (1 - (1-conflev)/2)
  ci <- array(0,2)
  ci[1] <- z[lq]
  ci[2] <- z[uq]
  return(ci)
}


# Bayes tail interval with beta priors

fct.F<- function(x,t,a1,b1,a2,b2){
  c <- (b2/a2)/(b1/a1)
  df(x,2*a2,2*b2)*pf(x*t/c,2*a1,2*b1)
}

or.F <- function(t,a1,b1,a2,b2)
{
  return(integrate(fct.F,0,Inf,t=t,a1=a1,b1=b1,a2=a2,b2=b2)$value)
}

or.fct <- function(ab,a1,b1,c1,d1,conflev)
{
  abs(or.F(ab[2],a1,b1,c1,d1) - (1 - (1-conflev)/2))+
    abs(or.F(ab[1],a1,b1,c1,d1) - (1-conflev)/2)
}

orCI <- function(x1,n1,x2,n2,a,b,c,d,conflev=.95)
{
  if(x2!=n2){
    a1 <- a + x1
    b1 <- b + n1 - x1
    c1 <- c + x2
    d1 <- d + n2 - x2
    start <- or.app(a1,b1,c1,d1,conflev)
    tailci <- optim(start,or.fct,a1=a1,b1=b1,c1=c1,d1=d1,
                    conflev=conflev,control=list(maxit=20000))$par
    if(tailci[1] < 0) tailci[1]  <- 0 }
  else{
    a1 <- a + n1 - x1
    b1 <- b +  x1
    c1 <- c + n2 - x2
    d1 <- d + x2
    start <- or.app(a1,b1,c1,d1,conflev)
    tailci1 <- optim(start,or.fct,a1=a1,b1=b1,c1=c1,d1=d1,
                     conflev=conflev,control=list(maxit=20000))$par
    if(tailci[1] < 0) tailci[1]  <- 0
    tailci <- array(0,2)
    tailci[1] <- 1/ tailci1[2]
    tailci[2] <- 1/ tailci1[1]
  }
  return(tailci)
}

or.app(a1,b1,c1,d1,0.95)
CI_OR_B=orCI(y1,n1,y2,n2,a,b,c,d)
CI_OR_B

###Frequency Theory

p1_F=data[1,1]/sum(data[1,])
p1_F
p2_F=data[2,1]/sum(data[2,])
p2_F

#Difference in Proportions in Frequency Theory
diff_F=p1_F-p2_F
diff_F

SE_diff_F=sqrt(p1_F*(1-p1_F)/sum(data[1,])+p2_F*(1-p2_F)/sum(data[2,]))

l=diff_F-qnorm(1-0.05/2)*SE_diff_F
u=diff_F+qnorm(1-0.05/2)*SE_diff_F
CI_diff_F=c(l,u)
CI_diff_F

#Relative Risk in Frequency Theory
RR_F=p1_F/p2_F
RR_F
logRR_F=log(RR_F)
SE_logRR_F=sqrt((1-p1_F)/data[1,1]+(1-p2_F)/data[2,1])
l=logRR_F-qnorm(1-0.05/2)*SE_logRR_F
u=logRR_F+qnorm(1-0.05/2)*SE_logRR_F
CI_logRR_F=c(l,u)
CI_logRR_F
CI_RR_F=exp(CI_logRR_F)
CI_RR_F


#Odds Ratio in Frequency Theory
OR_F=(data[1,1]*data[2,2])/(data[1,2]*data[2,1])
OR_F
logOR_F=log(OR_F)
logOR_F
SE_logOR_F=sqrt(sum(1/data))
l=logOR_F-qnorm(1-0.05/2)*SE_logOR_F
u=logOR_F+qnorm(1-0.05/2)*SE_logOR_F
CI_logOR_F=c(l,u)
CI_logOR_F
CI_OR_F=exp(CI_logOR_F)
CI_OR_F

#Comparison
T=rbind(c(CI_diff_B,CI_diff_F),c(CI_RR_B,CI_RR_F),
        c(CI_OR_B,CI_OR_F))
row.names(T)=c("Difference of Proportion","Relative Risk",
               "Odds Ratio")
colnames(T)=c("Bayesian lb","Bayesian ub",
              "Frequency lb","Frequency ub")
T
