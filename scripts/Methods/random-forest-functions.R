# RANDOM FOREST FUNCTIONS

# RANDOM FOREST---------------------------------------------------------
# Function for fitting the random forest
RandomForestFitting <- function(n, training, formula){
  # n: sample size
  # training: training data to be used
  # formula: formula for fitting the regression
  
  # samples from training data to be used in the random forest
  ind <- sample(1:nrow(training), size = n, replace = FALSE)
  # subset the training data
  subdata <- training[ind, ]
  # fit the random forest using the samples above
  fit <- randomForest(as.formula(formula),
               data = training)
  # return the fitted random forest
  return(fit)
} 

# Function to calculate the error rate of the random forest
RandomForestError <- function(fit, test, y){
  # fit: the fitted linear model
  # test: test data to be used
  # y : dependent variable used in fitting the regression
  
  # retrieve predictions based on fitted random forest and test data
  predictions <- predict(fit,
                         newdata = test)
  # compute Root Mean Square error
  rmse <- sqrt(colMeans((fire_test[, y] - predictions)^2))
  # return root mean square error
  return(rmse)
}

# BAGGING----------------------------------------------------------------
# Function for fitting the bagging
BaggingFitting <- function(n, training, formula, p = 2){
  # n: sample size
  # training: training data to be used
  # formula: formula for fitting the regression
  
  # samples from training data to be used in the random forest
  ind <- sample(1:nrow(training), size = n, replace = FALSE)
  # subset the training data
  subdata <- training[ind, ]
  # fit the bagging using the samples above
  fit <- randomForest(as.formula(formula),
                      mtry = p,
                      data = training)
  # return the fitted bagging model
  return(fit)
} 

# Function to calculate the error rate of bagging
BaggingError <- function(fit, test, y){
  # fit: the fitted linear model
  # test: test data to be used
  # y : dependent variable used in fitting the regression
  
  # retrieve predictions based on fitted bagging and test data
  predictions <- predict(fit,
                         newdata = test)
  # compute Root Mean Square error
  rmse <- sqrt(colMeans((fire_test[, y] - predictions)^2))
  # return root mean square error
  return(rmse)
}

# BOOSTING---------------------------------------------------------------