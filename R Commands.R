# Importing a file 
# From CSV
mydata <- read.table("c:/mydata.csv", header=TRUE,
                     sep=",", row.names="id")

mydate <- read.csv(file.choose(),header = T)


# Remove all objects 
rm(list=ls()) 

# Calculating Chi-square (survey is in build dataset)
library(MASS)       # load the MASS package 
# If frequency count is not known
tbl = table(survey$Smoke, survey$Exer)  #Creates counts table for categorical variables
tbl # the contingency table 
# If the frequency table is already given:
# You can use either row binding or column binding to create the table
# Row binding
row1 = c(30,29,16)
row2 = c(12,33,5)
table = rbind(row1,row2)
# Column binding
col1 = c(30,12)
col2 = c(29,33)
col3 = c(16,5)
table = cbind(col1,col2,col3)
rnames = c("Boys","Girls") #Creating vector to create rownames
cnames = c("A","B","C") #Creating vector to create column names
rownames(table) = rnames # Assigning row names
colnames(table) = cnames # Assigning column names
chisq.test(table)

# Finding critical chi-square value, for alpha = 0.05
qchisq(0.95,df = 2,lower.tail = TRUE)
#or
qchisq(0.05,df = 2,lower.tail = FALSE)

# Finding probability of a chi-square value
pchisq(16.2037, df = 2,lower.tail = FALSE)


# Normal distribution
pnorm(8.98,mean = 0, sd = 1, lower.tail = FALSE)

pnorm(2.129,mean = 0, sd = 1, lower.tail = TRUE)

qnorm(0.025,mean = 0, sd = 1)

#Finding confidence interval for unknown mean and SD
#Installing Rmisc package
install.packages("Rmisc")

#Loading Rmisc package
require("Rmisc")
value = c(12,65,14,52,34,64,95,2,4,95)
CI(value,ci=0.95)

#Hypothesis testing
#For proportions
prop.test(40,90,p=0.5)
?prop.test

# T Test
# Two sampled T test
older = c(45,38,52,48,25,39,51,46,55,46)
younger = c(34,22,15,27,37,41,24,19,26,36)
t.test(older,younger,alternative = "two.sided",conf.level = 0.90)
#Paired T Test
affected = c(488, 478, 480, 426, 440, 410, 458, 460)
notaffected = c(484, 478, 492, 444, 436, 398, 464, 476)
t.test(affected, notaffected, alternative="less", paired=TRUE, conf.level = 0.95)

#Power of t test
power.t.test(n = 52, delta = (98.2-98.6), sd = 0.6824, sig.level =0.02, power = NULL,
             type = "one.sample", alternative = "two.sided",strict = TRUE)

# t distribution
#Calculat the probability of t value
pt(1.963,df = 1, lower.tail = FALSE)
#Calculate the t value from probability
qt(0.10, df = 1, lower.tail = FALSE)

# f distribution
#Calculat the probability of f value
pf(161.45, df1 = 1, df2 = 1, lower.tail = FALSE)
#Calculate the f value from probability
qf(0.05, df1 = 1, df2 = 1, lower.tail = FALSE)

# Anova test
#Import dataset with this data into dataset named "cars"
#  compact midsize fullsize
#1     643     469      484
#2     655     427      456
#3     702     525      402
carsdata = stack(cars) # Stacking the table data 
cnames = c("pressure","cartype") #Creating a vector of column names
colnames(carsdata) = cnames # Assigning column names
carsdata
test = aov(pressure~cartype, data = carsdata) #Performing anova test
summary(test) # Displaying the summary
# To test which means are different we perform TukeyHSD test
tk = TukeyHSD(test)
tk
plot(tk)
# Stacking arrays:
memory = c(70,77,83,90,97)
placebok = c(37,43,50,57,63)
notreatement = c(3,10,17,23,30)
dataset = stack(list(memory=memory,placebok=placebok,notreatement=notreatement))

# Creating a table using data.frame
scores<-c(70,77,83,90,97,37,43,50,57,63,3,10,17,23,30)
treatment<-c(rep("D",5), rep("P",5), rep("N",5)) 
effect<-data.frame(scores,treatment)

#Get details of variables
lapply(wheel,class)

# Override default data type 
wheel$Group <- as.numeric(wheel$Group) # Replace factor with numerica type
