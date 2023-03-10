##--------------- COMPUTE ERRORS -------------------##
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

##--------------- PLOT ERRORS -------------------##
# plot predicted vs. true density
PlotPredTrueDensity <- function(results, n_plot){
  # results: data frame with fitting results
  # n: sample size for which to plot
  supp_labs <- c('without ticks', 'with ticks')
  names(supp_labs) <- c('no', 'yes')
  
  results %>%
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
          panel.grid.minor.x = element_blank()) -> plot
  
  return(plot)
}

# plot predicted vs. true burn percentage
PlotPredTrueBurn <- function(results, n_plot){
  # results: data frame with fitting results
  # n: sample size for which to plot
  supp_labs <- c('without ticks', 'with ticks')
  names(supp_labs) <- c('no', 'yes')
  
  results %>%
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
          panel.grid.minor.x = element_blank()) -> plot
  
  return(plot)
}

# % of correct predictions of the parameters
PlotPercCorrectParams <- function(errors, n_plot){
  # errors: data frame with calculated error measures
  # n_plot: sample sizes to plot (can be multiple)
  errors %>%
    filter(n %in% n_plot)%>%
    ggplot(mapping = aes(x = n, y = perc_correct,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(y = 'density & direction',
         x = 'training sample size',
         fill = 'Ticks included') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) -> p1
  
  errors %>%
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
          panel.grid.minor.x = element_blank()) -> p2
  
  errors %>%
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
          panel.grid.minor.x = element_blank()) -> p3
  
  errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = perc_correct_cat,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(y = 'density within \n 10% of true density',
         x = 'training sample size',
         fill = 'Ticks included') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) -> p4
  
  plot <- p1 + p2 + p3 + p4 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A',
                    title = '% correctly predicted') & 
    theme(plot.tag = element_text(size = 8))
  
  return(plot)
}

# RMSE etc. of density
PlotRMSEParams <- function(errors, n_plot){
  errors %>%
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
          panel.grid.minor.x = element_blank()) -> p1
  
  errors %>%
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
          panel.grid.minor.x = element_blank()) -> p2
  
  errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = point_pred_performance,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(fill = 'Ticks included',
         x = NULL,
         y = 'point prediction performance') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) -> p3
  
  p1 + p2 + p3 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 8))
  
  
}

# RMSE etc. of burn percentage
PlotRMSEOut <- function(errors, n_plot){
  errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = RMSE_burn,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(fill = 'Ticks included',
         y = 'RMSE of burn percentage at last tick') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) -> p1
  
  errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = n, y = NRMSE_burn,
                         fill = ticks_included)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(fill = 'Ticks included',
         y = 'NRMSE of burn percentage at last tick') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) -> p2
  
  patch <- p1 + p2 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 8)) &
    xlab(NULL)
  
  wrap_elements(panel = patch) +
    labs(tag = 'training sample size') +
    theme(plot.tag = element_text(size = rel(1)),
          plot.tag.position = 'bottom')
  
}

PlotCoverageDensity <- function(errors, n_plot){
  errors %>%
    ggplot(mapping = aes(x = n, y = coverage,
                         colour = ticks_included)) +
    lims(y = c(0.94, 1.0)) +
    geom_hline(yintercept = 0.95,
               colour = 'light green') +
    geom_beeswarm() +
    labs(x = 'training sample size',
         colour = 'Ticks included',
         y = 'coverage (95% \n prediction interval)') +
    theme_minimal()
}