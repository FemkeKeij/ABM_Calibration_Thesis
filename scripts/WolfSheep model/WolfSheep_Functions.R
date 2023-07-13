# Functions shared across the wolf-sheep model



################### ERROR COMPUTATION ########################
#Function to compute errors for continuous parameters:
ComputeErrorsContinuous <- function(predicted, true,
                                    mean_training, variable,
                                    fold){
  # predicted: predicted values
  # true: true values
  # mean_training: mean of values in training data set
  
  N <- length(true)
  
  perc_correct <- sum(predicted == true) / N
  perc_correct_cat <- sum(predicted >= true - 0.1 * true &
                            predicted <= true + 0.1 * true)
  RMSE <- sqrt(sum((predicted - true)^2)) / N
  NRMSE <- (sqrt(sum((predicted - true)^2)) / N) / sd(true)
  point_pred <- 1 - sum(sqrt((predicted - true)^2)) /
    sum(sqrt((true - mean_training)^2))
  
  return(tibble(fold = fold,
                variable = variable,
                perc_correct = perc_correct,
                perc_correct_cat = perc_correct_cat,
                RMSE = RMSE,
                NRMSE = NRMSE,
                point_pred = point_pred))
}

################### VISUALISE ERRORS ########################
VisualiseErrorMetrics <- function(out, noise){
  # out: data with errors to visualise
  # noise: true or false, plot for data with or without noise
  out_long <- out %>%
    pivot_longer(cols = c(perc_correct_cat,
                          perc_correct,
                          RMSE, NRMSE, point_pred),
                 names_to = 'metric',
                 values_to = 'vals')
  
  if(noise = FALSE){
    plot <- out_long %>%
      filter(noise == 'no') %>%
      ggplot(mapping = aes(x = timesteps,
                           y = vals,
                           fill = sampling_method)) +
      geom_bar(position = 'dodge', stat = 'identity') +
      facet_wrap(~ metric + variable,
                 scales = 'free_y')
  } else{
    plot <- out_long %>%
      filter(noise == 'yes') %>%
      ggplot(mapping = aes(x = timesteps,
                           y = vals,
                           fill = sampling_method)) +
      geom_bar(position = 'dodge', stat = 'identity') +
      facet_grid(~ metric + variable,
                 scales = 'free_y')
  }
  
  return(plot)
}




