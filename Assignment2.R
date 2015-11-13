#Problem 1
# Storing the data in the dataset "problem1"
problem1
# Using "cor" function to calculate the correlation between first test score and 
# final exam score
cor(problem1$first_test_score, problem1$final_exam_score)
#Correlation between second test score and final exam score
cor(problem1$second_test_score, problem1$final_exam_score)
#Scatter plot: first test score, final exam score
plot(problem1$first_test_score, problem1$final_exam_score
    ,ylab = "Final exam score"
    ,xlab = "First test score")
abline(lm(problem1$final_exam_score~problem1$first_test_score), col = "red")
#D
# Correlation between second test score and final exam score
cor(problem1$second_test_score, problem1$final_exam_score)

#Problem2
#a
# Scatter diagram with Temperature as explanatory variable
plot(problem2$Temperature, problem2$Price
    ,xlab = "Temperature in Fahrenheit"
    ,ylab = "Price in $"
    ,main = "Scatter Diagram"
    ,pch = 20)
abline(lm(problem2$Price~problem2$Temperature), col = "red")
#c
# Regression
reg <- lm(problem2$Price~problem2$Temperature, data = problem2)
reg
#Problem 3 
#a 
problem3
reg <- lm(problem3$Rating~problem3$Price, data = problem3)
reg
summary(reg)$r.squared
#b
summary(reg)$residuals
plot(problem3$Price,summary(reg)$residuals, pch = 20
    ,xlab = "Price", ylab = "Residuals")
abline(lm(summary(reg)$residuals~problem3$Price), col = "red")
#c
cor(problem3$Price,problem3$Rating)
plot(problem3$Price,problem3$Rating
     ,xlab = "Price"
     ,ylab ="Rating"
     ,pch = 20)
abline(lm(problem3$Rating~problem3$Price), col = "red")


































































