# for confusion matrices
library(caret)
# for matthew's correlation coefficient
library(mltools)

##--------------- COMPUTE ERRORS -------------------##
# function to calculate errors for the fire model
ComputeErrors <- function(results, ticks = FALSE,
                             density_means, pred_int_included = FALSE){
  # results: dataframe with results from fitting
  # density means: mean density in training data for each sample size
  # pred_int_included: set to TRUE if fitting produced prediction interval for density
  
  # copy mean density to appropriate sample sizes
  errors <- merge(results, as_tibble(cbind(n, density_means)),
                  by = c('n')) %>%
    # per sample size, summarise
    group_by(n) %>%
              # % correctly predicted direction + density in test set
    summarise(perc_correct_params = sum(direction_true == direction_pred &
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
              perc_correct_cat_density = sum(density_pred >=
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
              point_pred_performance_density = 1 - 
                sum(sqrt((density_pred - density_true)^2)) /
                sum(sqrt((density_true - density_means)^2)))
  
  errors$direction_kappa <- numeric(nrow(errors))
  errors$direction_f1 <- numeric(nrow(errors))
  errors$direction_mcc <- numeric(nrow(errors))
  
  # add the error measures for the directions
  for(i in 1:length(unique(results$n))){
    results_sub <- results %>%
      filter(n == results$n[i])
    
    cm <- confusionMatrix(as.factor(results_sub$direction_pred),
                          as.factor(results_sub$direction_true),
                          mode = 'everything')
    
    errors$direction_kappa[i] = cm$overall[2]
    errors$direction_f1[i] = cm$byClass[7]
    errors$direction_mcc[i] = mcc(results_sub$direction_pred,
                                  results_sub$direction_true)
  }
  
  # if method produced prediction interval:
  # compute coverage and add to output
  if(pred_int_included == TRUE){
    errors2 <- merge(results, as_tibble(cbind(n, density_means)),
          by = c('n')) %>%
      group_by(n) %>%
      summarise(coverage_density = sum(density_true <= density_pred_upr &
                                 density_true >= density_pred_lwr) /
                  nrow(fire_test))
    coverage_density <- errors2$coverage_density
    
    errors <- cbind(errors, coverage_density)
  }
  
  # add indicator for ticks included yes or no
  errors$ticks_included <- if(ticks == TRUE){'yes'} else{'no'}
  
  # return data frame
  return(errors)
}

##--------------- PLOT ERRORS -------------------##
# plot predicted vs. true density
PlotPredTrueDensity <- function(results, n_plot){
  # results: data frame with fitting results
  # n: sample size for which to plot
  supp_labs <- c('without ticks', 'with ticks')
  names(supp_labs) <- c('no', 'yes')
  
  plot <- results %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = density_pred,
                         y = density_true,
                         colour = direction_correct)) +
    facet_grid(. ~ ticks_included,
               labeller = labeller(ticks_included = supp_labs)) +
    geom_point() +
    theme_minimal() +
    labs(x = 'predicted density',
         y = 'true density',
         colour = 'Number of directions \n predicted correctly') +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(plot)
}

# plot predicted vs. true burn percentage
PlotPredTrueBurn <- function(results, n_plot){
  # results: data frame with fitting results
  # n: sample size for which to plot
  supp_labs <- c('without ticks', 'with ticks')
  names(supp_labs) <- c('no', 'yes')
  
  plot <- results %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = burn_pred,
                         y = burn_true,
                         colour = direction_correct)) +
    facet_grid(. ~ ticks_included,
               labeller = labeller(ticks_included = supp_labs)) +
    geom_point() +
    theme_minimal() +
    labs(x = 'predicted burn percentage',
         y = 'true burn percentage',
         colour = 'Number of directions \n predicted correctly') +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(plot)
}

# % of correct predictions of the parameters
PlotPercCorrectParams <- function(errors, n_plot){
  # errors: data frame with calculated error measures
  # n_plot: sample sizes to plot (can be multiple)
  p1 <- errors %>%
    filter(n %in% n_plot)%>%
    ggplot(mapping = aes(x = n, y = perc_correct_params,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(y = 'density & direction',
         x = 'training sample size',
         fill = 'Ticks included') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  p2 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = perc_density_correct,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(y = 'density',
         x = 'training sample size',
         fill = 'Ticks included') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  p3 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = perc_direction_correct,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(y = 'direction',
         x = 'training sample size',
         fill = 'Ticks included') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  p4 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = perc_correct_cat_density,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(y = 'density within \n 10% of true density',
         x = 'training sample size',
         fill = 'Ticks included') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  plot <- p1 + p2 + p3 + p4 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A',
                    title = '% correctly predicted') & 
    theme(plot.tag = element_text(size = 8))
  
  return(plot)
}

# RMSE etc. of density
PlotRMSEParams <- function(errors, n_plot){
  p1 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = RMSE_density,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(fill = 'Ticks included',
         x = NULL,
         y = 'RMSE of density') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  p2 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = NRMSE_density,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(fill = 'Ticks included',
         x = 'training sample size',
         y = 'NRMSE of density') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  p3 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = point_pred_performance_density,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(fill = 'Ticks included',
         x = NULL,
         y = 'point prediction performance') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  plot <- p1 + p2 + p3 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 8))
  
  return(plot)
}

# RMSE etc. of burn percentage
PlotRMSEOut <- function(errors, n_plot){
  p1 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = RMSE_burn,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(fill = 'Ticks included',
         y = 'RMSE of burn percentage at last tick') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  p2 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = NRMSE_burn,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(fill = 'Ticks included',
         y = 'NRMSE of burn percentage at last tick') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  patch <- p1 + p2 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 8)) &
    xlab(NULL)
  
  plot <- wrap_elements(panel = patch) +
    labs(tag = 'training sample size') +
    theme(plot.tag = element_text(size = rel(1)),
          plot.tag.position = 'bottom')
  
  return(plot)
}

PlotCoverageDensity <- function(errors, n_plot){
  plot <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = coverage_density,
                         colour = ticks_included)) +
    lims(y = c(0.94, 1.0)) +
    geom_hline(yintercept = 0.95,
               colour = 'light green') +
    geom_beeswarm() +
    labs(x = 'training sample size',
         colour = 'Ticks included',
         y = 'coverage (95% \n prediction interval)') +
    theme_minimal()
  
  return(plot)
}

PlotMetricsDirections <- function(errors, n_plot){
  p1 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = factor(n), y = direction_kappa,
                         colour = ticks_included)) +
    geom_point(size = 3) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(y = 'kappa score for directions',
         colour = 'ticks included') +
    lims(y = c(0, NA))
  
  p2 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = factor(n), y = direction_f1,
                         colour = ticks_included)) +
    geom_point(size = 3) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(y = 'F1 score for directions',
         colour = 'ticks included') +
    lims(y = c(0, NA))
  
  p3 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = factor(n), y = direction_mcc,
                         colour = ticks_included)) +
    geom_point(size = 3) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(y = 'MCC for directions',
         colour = 'ticks included') +
    lims(y = c(0, NA))
  
  patch <- p1 + p2 + p3 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 8)) &
    xlab(NULL)
  
  plot <- wrap_elements(panel = patch) +
    labs(tag = 'training sample size') +
    theme(plot.tag = element_text(size = rel(1)),
          plot.tag.position = 'bottom')
  
  return(plot)
}
