rm(list = ls())
library(caret)

# Loading downloaded data
training_data <- read.csv("./pml-training.csv")
testing_data <- read.csv("./pml-testing.csv")

# Cleaning data
# Removing columns with majority empty or NA values
NAcounts_training = rep(0, times = dim(training_data)[2])
NAcounts_testing = rep(0, times = dim(training_data)[2])

for (icol in 1:dim(training_data)[2]){
      NAcounts_training[icol] <- max(sum(is.na(training_data[[icol]])), sum(training_data[[icol]]==""), na.rm = TRUE)
      NAcounts_testing[icol] <- max(sum(is.na(testing_data[[icol]])), sum(testing_data[[icol]]==""), na.rm = TRUE)
}

NAcounts = NAcounts_testing + NAcounts_training
      
GoodCols <- NAcounts > 0
  
training_clean <- training_data[,which(GoodCols == FALSE)]
testing_clean <- testing_data[,which(GoodCols == FALSE)]

training_clean <- training_clean[,8:dim(training_clean)[2]]
testing_clean <- testing_clean[,8:dim(testing_clean)[2]]

# Split training data into training and testing subsets
inTrain <- createDataPartition(y = training_clean$classe ,p = 0.7, list = FALSE)
training <- training_clean[inTrain,]
testing <- training_clean[-inTrain,]

# Check for correlated covariates
CorrMatrix <- abs(cor(training[,-53]))
diag(CorrMatrix) <- 0
which(CorrMatrix>0.8, arr.ind = TRUE)

#######################################################################################################
# Exploratory Data Analysis
library(ggplot2)

# # Model 1
# M1 <- train(classe ~., data = training, method = "gbm")
# 
# library(randomForest)
# 
# M2 <- train(classe ~., data = training, method = "rf")
# 
# library(rpart)
# M3 <- train(classe ~., data = training, method = "rpart")
# 
# library(MASS)
# M4 <- train(classe ~., data = training, method = "lda")


# save(M1, M2, M3, M4, file = "ModelResults.RData")

train
# P14 <- data.frame(P1, P4, classe = testing$classe)
# M14 <- train(classe ~., method = "rf", data = P14)
# P <- predict(M14, P14)
# confusionMatrix(P, testing$classe)$overall[1]
# 
# P12 <- data.frame(P1, P2, classe = testing$classe)
# M12 <- train(classe ~., method = "rf", data = P12)
# P <- predict(M12, P12)
# confusionMatrix(P, testing$classe)$overall[1]


P1testing <- predict(M1, newdata = testing_data)
P2testing <- predict(M2, newdata = testing_data)
P3testing <- predict(M3, newdata = testing_data)
P4testing <- predict(M4, newdata = testing_data)



# P14testing <- predict(M14, newdata = testing_data)
# 
# 
# PC <- prcomp((training[,-53]))
# plot(PC$x[,1],PC$x[,2], col = training$classe)
# 
# preProc <- preProcess(training[,-53], method = "pca", pcaComp = 2)
# trainingPC <- predict(preProc, training[,-53])
# plot(trainingPC[,1], trainingPC[,2], col = training$classe)
# 
# Mtest <- train(training$classe ~., method = "gbm", data = trainingPC)
