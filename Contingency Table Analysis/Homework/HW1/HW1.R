
rm(list=ls())

#6(c)
scoreCI1 <-function(x) ((0.05-x)/(x*(1-x)/20)^0.5)-1.96
scoreCI2 <-function(x) ((0.05-x)/(x*(1-x)/20)^0.5)+1.96

# interwal point < 0.05
uniroot(scoreCI1,lower = 0, upper = 0.05)

# interwal point > 0.05
uniroot(scoreCI2,lower = 0.05, upper = 1)

#6(d)

lrCI <- function(x) 2*(1*log(0.05/x)+19*log((1-0.05)/(1-x)))-3.84

# interwal point < 0.05
uniroot(lrCI,lower = 0, upper = 0.05)

# interwal point > 0.05
uniroot(lrCI,lower = 0.05, upper = 1)

#6(e)
#probability of observing this sample (just one is better) and more extreme 
#events (no one is better) is p.
p=pbinom(1,20,0.5)
p

#6(f)

#7
