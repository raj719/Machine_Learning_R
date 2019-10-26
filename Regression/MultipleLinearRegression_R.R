#Multiple Linear Regression

#Read data
ds = read.csv("50_Startups.csv")

#Importing libraries
install.packages('caTools')
library(caTools)

#Encoding categorical variables
ds$State = factor(ds$State,
                  levels = c("California", "New York", "Florida"),
                  labels = c(1,2,3))

#Spliting data set into training and testing
set.seed(123)
split = sample.split(ds$Profit, SplitRatio = .80)
training_set = subset(ds, split == TRUE)
test_set = subset(ds, split == FALSE)

#Fitting multiple linear regression to the training set
?lm
regressor = lm(formula = Profit ~ .,
               data = training_set)

#Predicting the test set results
y_pred = predict(regressor, newdata = test_set)

#Building the optimal model using backward elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = ds)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = ds)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = ds)
summary(regressor)

# Removing any variable further leads to decrease in adjusted r square value