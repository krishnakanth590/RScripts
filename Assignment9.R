asd


pt(-abs(1.13),df=7)

# 3
regular<-c(16, 20, 21,22,23,22,27,25,27,28)

premium<-c(19,22,24,24,25,25,26,26,28,32)

summary(regular)

summary(premium)

difference = premium-regular

mean = mean(difference)

sd = sd(difference)

t = (2)/(1.414 / sqrt(10))

pvalue = pt(-abs(t),df=9)

pvalue

# Problem 4

xbar = 98.2846

sd = 0.6824

n = 52

tcritical = 2.4017

me = (tcritical)*(sd / sqrt(n))

me

upperlimit = xbar + me

lowerlimit = xbar - me

upperlimit

lowerlimit
# b
t = (xbar - 98.2) / (sd / sqrt(n))

t

pvalue = pt(-abs(t),df=n-1)

pvalue

96.79852
x


x= qnorm(0.98,98.2,0.6824)

x

y = 1-pnorm(x,98.2,0.6824)

y




# Problem 2 
children<- c(40.3, 55.0, 45.7, 43.3, 50.3, 45.9, 53.5, 43.0, 44.2, 44.0, 33.6, 55.1, 48.8, 50.4, 37.8, 60.3, 46.6, 47.4, 44.0)
adult<- c(20.0, 30.2, 2.2, 7.5, 4.4, 22.2, 16.6, 14.5, 21.4, 3.3, 10.0, 1.0, 4.4, 1.3, 8.1,6.6, 7.8, 10.6, 10.6, 16.2, 14.5, 4.1, 15.8, 4.1, 2.4, 3.5, 8.5, 4.7, 18.4)

summary (children)
summary(adult) 

boxplot(children,adult)


x1bar <- mean(children)

x2bar <- mean(adult)

dbar<- x1bar - x2bar

sd1 <- sd(children)

sd2 <- sd(adult)

n1 <- 19

n2 <- 29

t <- 2.013

me<- t*sqrt(((sd1^2)/n1)+((sd2^2)/n2))
upper_limit<- dbar + me

upper_limit

lower_limit<- dbar - me
lower_limit


