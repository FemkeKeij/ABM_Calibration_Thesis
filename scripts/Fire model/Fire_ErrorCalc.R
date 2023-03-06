# function to calculate errors for the fire model
ComputeErrors <- function(results, ticks = FALSE,
                             density_means, pred_int_included = FALSE){
  # results: dataframe with results from fitting
  # density means: mean density in training data for each sample size
  # pred_int_included: set to TRUE if fitting produced prediction interval for density
  
  # copy mean density to appropriate sample sizes
  merge(results, as_tibble(cbind(n, density_means)),
        by = c('n')) %>%
    # per sample size, summarise
    group_by(n) %>%
              # % correctly predicted direction + density in test set
    summarise(perc_correct = sum(direction_true == direction_pred &
                                   density_true == round(density_pred))
                / nrow(fire_test),
              # % correctly predicted density in test set
              perc_density_correct = sum(density_true ==
                                           round(density_pred)) /
                nrow(fire_test),
              # % correctly predicted direction in test set
              perc_direction_correct = sum(direction_true ==
                                             direction_pred) /
                nrow(fire_test),
              # % predicted density within 10% of true density in test set
              perc_correct_cat = sum(density_pred >=
                                       density_true - 5 &
                                       density_pred <=
                                       density_true + 5) /
                nrow(fire_test),
              # RMSE of predicted vs. true density
              RMSE_density = sqrt(sum((density_pred - density_true)^2))
                / nrow(fire_test),
              # NRMSE of predicted vs. true density
              NRMSE_density = (sqrt(sum((density_pred - density_true)^2))
                               / nrow(fire_test)) / sd(density_true),
              # RMSE of predicted vs. true burn percentage
              RMSE_burn = sqrt(sum((burn_pred - burn_true)^2))
                / nrow(fire_test),
              # NRMSE of predicted vs. true burn percentage
              NRMSE_burn = (sqrt(sum((burn_pred - burn_true)^2))
                            / nrow(fire_test)) / sd(density_true),
              # point prediction performance for density
              point_pred_performance = 1 - 
                sum(sqrt((density_pred - density_true)^2)) /
                sum(sqrt((density_true - density_means)^2))) ->
    errors
  
  # if method produced prediction interval:
  # compute coverage and add to output
  if(pred_int_included == TRUE){
    merge(results, as_tibble(cbind(n, density_means)),
          by = c('n')) %>%
      group_by(n) %>%
      summarise(coverage = sum(density_true <= density_pred_upr &
                                 density_true >= density_pred_lwr) /
                  nrow(fire_test)) -> errors2
    coverage <- errors2$coverage
    
    errors <- cbind(errors, coverage)
  }
  
  # add indicator for ticks included yes or no
  errors$ticks_included <- if(ticks == TRUE){'yes'} else{'no'}
  
  # return data frame
  return(errors)
}