startTime = Sys.time()
# Import CSV
training <- read.csv("G:/UIC/Kaggle Competitions/San Francisco/Data/train.csv", nrow = 1000)

# Eliminating records where Y = 90 
training_2 <- subset(training, Y != 90)

# Bin lattitude, generating 5 bins when n = 2 
training_2$LatBin = cut(training_2$X, 5 , labels = c("A","B","C","D","E"))
# Bin longitude, generating 5 bins when n = 2 
training_2$LongBin = cut(training_2$Y, 5, labels = c("A","B","C","D","E"))

# Append cols
training_3 = transform(training_2, Loc = paste0(LatBin,LongBin))

# Convert string to DateTime
training_3$Dates = as.POSIXct(strptime(training_3$Dates,"%Y-%m-%d %H:%M:%S"))

# Extract Month and Hour from Date Time
training_3$Month = format.Date(training_3$Dates, "%b")
training_3$Hour = format.Date(training_3$Dates, "%H")

# Delete cols
training_3[c(1,3,7:11)] = list(NULL)

# Create training data
trainPerc = 0.80 #*********************************
training_size = floor(trainPerc * nrow(training_3))

set.seed(1990)

training_index = sample(seq_len(nrow(training_3)), size = training_size)


# Find out data types of dataset
str(training_3)

# Looping through cols and converting them to Factor type
for (colName in names(training_3)){
  if ( !is.factor(training_3[[colName]])){
    training_3[[colName]] = as.factor(training_3[[colName]])
  }
}
# Verifying data types
str(training_3)

# Creating training and testing datasets
train = training_3[training_index, ]
test = training_3[-training_index, ]

#Naive Bayes Model
nb = naiveBayes(Category~., train)

test$Prediction = predict(nb , test, c("class","raw"))

predProbs = data.frame(predict(nb , test, c("raw")))

finalTest = cbind(test, predProbs)

# Calculate accuracy
correctPred = 0
for ( i in 1:nrow(finalTest)) {
  if(finalTest[i,"Category"] == finalTest[i,"Prediction"]){
    correctPred = correctPred+1
  }
}
accuracy = (correctPred / nrow(finalTest))*100
print (paste0("Accuray : ",accuracy))
endTime = Sys.time()
print(paste0("Time taken: ",(endTime-startTime)))
