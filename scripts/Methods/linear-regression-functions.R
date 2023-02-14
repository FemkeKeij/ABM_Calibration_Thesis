# LINEAR REGRESSION FUNCTIONS

# SIMPLE LINEAR MULTIPLE REGRESSION -------------------------------------
# Function for fitting the linear regression
LinearRegFitting <- function(n, training, formula){
  # n: sample size
  # training: training data to be used
  # formula: formula for fitting the regression
  
  # samples from training data to be used in the linear regression
  ind <- sample(1:nrow(training), size = n, replace = FALSE)
  # subset the training data
  subdata <- training[ind, ]
  # fit linear model using the samples above
  fit <- lm(formula, data = subdata)
  # return fitted linear model
  return(fit)
}

# Function to calculate the error rate of the linear regression
LinearRegError <- function(fit, test, y){
  # fit: the fitted linear model
  # test: test data to be used
  # y : dependent variable used in fitting the regression
  
  # retrieve predictions based on fitted linear regression and test data
  predictions <- predict.lm(fit, newdata = test)
  # compure root mean square error
  rmse <- sqrt(colMeans((test[, y] - predictions)^2))
  # return root mean square error
  return(rmse)
}

# Function that returns plot of linear regression fitted
  # to the Fire model
LinearRegPlottingFire <- function(n, training){
  # get training data sample of size n
  ind <- sample(1:nrow(training),
                size = n,
                replace = FALSE)
  subdata <- training[ind, ]
  # set direction to be a factor variable
  subdata$direction <- as.factor(subdata$direction)
  
  # use training sample to plot simple linear regression
  subdata %>%
    ggplot(aes(x = density, y = burn_percentage,
               shape = direction)) +
    labs(x = 'tree density (%)', y = '% of burned trees at last tick',
         caption = str_c('sample size', as.character(n), sep = " ")) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE,
                fullrange = TRUE, 
                aes(color = direction)) ->
    plot
  
  return(plot)
}

# PIECEWISE LINEAR REGRESSION ------------------------------------------
# Function for fitting the piecewise linear regression
PiecewiseRegFitting <- function(n, training, formula){
  # n: sample size
  # training: training data to be used
  # formula: formula for fitting the piecewise linear regression
  
  # samples from the training data to be used in the piecewise regression
  ind <- sample(1:nrow(training), size = n / 2, replace = FALSE)
  # subset of the training data
  subdata <- training[ind, ]
  
  # fit linear regression
  fit <- lm(formula, data = subdata)
  # retrieve independent variable out of formula
  x <- str_split(formula, pattern = "~ ")[[1]][2]
  # fit piecewise linear regression
  segmented_fit <- segmented(fit,
                             seg.Z = ~ x)
  # return piecewise linear regression
  return(segmented_fit)
}

# Function for calculating the RMSE for piecewise linear regression
PiecewiseRegError <- function(fit, test, y){
  # fit: fitted piecewise linear regression
  # test: test data to be used for error calculation
  # y: dependent variable used in fitting the regression
  
  # Find predictions for test data based on fit
  prediction <- predict.segmented(fit,
                                  newdat = as.data.frame(test))
  
  # calculate Root Mean Square Error
  RMSE <- rmse(fit, test)
  RMSE <- sqrt(colMeans((test[, y] - prediction)^2))
  
  return(RMSE)
}

# Function to plot the results of piecewise linear regression
PiecewiseRegPlotting <- function(training, fit){
  # training: training data used to fit the model
  # fit: fitted piecewise linear regression
  
  
}