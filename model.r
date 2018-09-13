library("caret")
library("xgboost")

memory.limit(10000)

# below 2 lines are used to read the training and testing data - 
training <- read.csv("train.csv")

testing <- read.csv("test.csv")

x <- nrow(training)
x <- as.integer(x)

z <- nrow(testing)
z <- as.integer(z)
# below line is used to draw the plot to see the distribuion of labels in the training data -
qplot(x = Label,data = training)

summary(training$FeaA)
# for feaA column since mean, median, 1st quarter and 3rd quarter are nearly same, so we can replace missing values with mean

a <- mean(training$FeaA,na.rm = TRUE)

for(i in 1:x)
{
  if(is.na(training[i,2]) == TRUE)
  {
    training[i,2] <- a
  }
}

# below line is used to draw the plot to see the relation between FeaA and Label - 
qplot(x = Label,y = FeaA,data = training)

 
summary(training$FeaB)

b <- mean(training$FeaB,na.rm = TRUE)

for(i in 1:x)
{
  if(is.na(training[i,3]) == TRUE)
  {
    training[i,3] <- b
  }
}

qplot(x = Label,y = FeaB,data = training)


summary(training$FeaC)

C <- mean(training$FeaC,na.rm = TRUE)

for(i in 1:x)
{
  if(is.na(training[i,4]) == TRUE)
  {
    training[i,4] <- C
  }
}

qplot(x = Label,y = FeaC,data = training)


summary(training$FeaD)

d <- mean(training$FeaD,na.rm = TRUE)

for(i in 1:x)
{
  if(is.na(training[i,5]) == TRUE)
  {
    training[i,5] <- d
  }
}

qplot(x = Label,y = FeaD,data = training)


summary(training$FeaE)

e <- mean(training$FeaE,na.rm = TRUE)

for(i in 1:x)
{
  if(is.na(training[i,6]) == TRUE)
  {
    training[i,6] <- e
  }
}

qplot(x = Label,y = FeaE,data = training)

# similarly removing missing values in testing data -

summary(testing$FeaB)

b1 <- mean(testing$FeaB,na.rm = TRUE)

for(i in 1:z)
{
  if(is.na(testing[i,3]) == TRUE)
  {
    testing[i,3] <- b1
  }
}

summary(testing$FeaC)

c1 <- mean(testing$FeaC,na.rm = TRUE)

for(i in 1:z)
{
  if(is.na(testing[i,4]) == TRUE)
  {
    testing[i,4] <- c1
  }
}

summary(testing$FeaD)

d1 <- mean(testing$FeaD,na.rm = TRUE)

for(i in 1:z)
{
  if(is.na(testing[i,5]) == TRUE)
  {
    testing[i,5] <- d1
  }
}

# below line is used to copy the elements of training to training2
training2 <- training

# below line is used to see the data type of training2
str(training2)

# below line is used to convert the data type of Timestamp to character type -
training2$Timestamp <- as.character(training2$Timestamp)

# below line is used to convert the data type of Label to factor -
training2$Label <- as.factor(training2$Label)

# Now let us build the XGBoost model - 

num_classes <- 5

y <- as.numeric(training2$Label) - 1

fitnew <- xgboost(data = as.matrix(training2[,-c(1,7)]),label = y,nrounds = 10,objective = "multi:softprob",num_class = num_classes)
# xgboost stands for eXtreme Gradient Boosting
# in above function we have converted the data frame to matrix as xgboost works on matrix
# nrounds tell the maximum number of iterations, objective = "multi:softprob" suggest 
# that we have used multipleclasses for prediction and num_class suggest how many classes
# are there in the model

y_pred3 <- predict(fitnew,newdata = as.matrix(testing[,-1]))

prednew <- matrix(y_pred3, ncol=num_classes, byrow=TRUE)
# prednew contain the predicted values of each class.

pred_labels <- max.col(prednew)
# we are predicting to the class which has maximum probability value

pred_labels <- pred_labels - 1

testing$Label <- pred_labels

qplot(x = Label,data = testing)

write.csv(testing,file = "output_file.CSV",row.names = FALSE)