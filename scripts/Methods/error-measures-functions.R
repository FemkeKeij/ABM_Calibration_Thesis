# ERROR MEASURES FUNCTIONS

# TO DO:
  # references to formula's in function comments

# Baseline comparison

# Performance
CalcPerformance <- function(fit, training, test){
  # fit: fitted model
  # training: training data used to fit the model
  # test: test data to evaluate performance of fitted model
  # theta: the estimated parameter
  # note that this function assumes that the training and test data
      # are formatted with the same column order
  
  # retrieve predicted parameters for test data given fitted model
  prediction <- predict(fit,
                        data = test,
                        type = "terms")
  
  # create vector to hold performance metric for each parameter
  performance <- numeric(ncol(prediction))
  names(performance) <- colnames(prediction)
  
  # for each parameter ...
  for(i in 1:length(performance)){
    theta_pred <- prediction[, i] # retrieve predictions
    
    # find corresponding column in training and test data
    ind <- which(colnames(prediction)[i] == colnames(training))
    # calculate mean parameter in training data
    theta_mean <- colMeans(training[, ind])
    # true parameter values in test data
    theta_true <- test[, ind]
    # calculate performance ACCORDING TO FORMULA ...
    performance[i] <- 1 - (sum(sqrt((theta_pred - theta_true)^2))) /
      (sum(sqrt((theta_true - theta_mean)^2)))
  }
  
  return(performance)
}

# Coverage