# Problem 8 a
sample.8a = matrix(0,nrow = 50, ncol = 4)
for(i in 1:50) {
  x = rnorm(4, mean = 10, sd = 2);
  sample.8a[i,] = x;
}
sample.8a
mean(sample.8a)
sd(sample.8a)

# Problem 8 b
sample.8b = matrix(0,nrow = 50, ncol = 25)
for(i in 1:50) {
  x = rnorm(25, mean = 10, sd = 2);
  sample.8b[i,] = x;
}
mean(sample.8b)
sd(sample.8b)

# Problem 8 c
sample.8c = matrix(0,nrow = 50, ncol = 100)
for(i in 1:50) {
  x = rnorm(100, mean = 10, sd = 2);
  sample.8c[i,] = x;
}
mean(sample.8c)
sd(sample.8c)


hist(sample.8a,
     breaks = c(3,5,7,9,11,13,15,17),
     main = "50 random samples with sample size 4",
     col = "lightblue")

hist(sample.8b,
     breaks = c(3,5,7,9,11,13,15,17),
     main = "50 random samples with sample size 25",
     col = "lightgreen")

hist(sample.8c,
     breaks = c(1,3,5,7,9,11,13,15,17,19),
     main = "50 random samples with sample size 100",
     col = "beige")

myvar =1
#Remove all objects
rm(list=ls()) 

x[2]

?matrix
?rnorm

?sample

#################################################################################
# Problem 9 a

x = rbinom(100,1,0.3)

sample.proportion = x

proportion.mean = mean(sample.proportion)

proportion.var = var(sample.proportion)

proportion.mean

proportion.var 

# Problem 9 b
x = rbinom(1000,100,0.3)

sample.p = x/100
sample.p
# Problem 9 c

mean(sample.p)

var(sample.p)

# Problem 9 d
E.p = 0.30

V.p = 0.0021

plot(density(sample.p), main = "Density plot of sample proportions", xlab= "Sample proportions",xlim = c(0.1,0.5), col = "red")

curve(dnorm(x, mean = E.p, sd=sqrt(V.p)), from = 0.1, to = 0.5, add = TRUE, col = "green")
