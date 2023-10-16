# for confusion matrices
library(caret)
# for matthew's correlation coefficient
library(mltools)

##--------------- COMPUTE ERRORS -------------------##
# function to calculate errors for the fire model
FireComputeErrors <- function(predictions,
                              sample_size, sample_method,
                              summarise_runs, datapoints){
  # predictions: dataframe with results from fitting
  # requires the columns density, density_pred,
  # directions, directions_pred
  
  # sample_size, sample_method, summarise_runs, datapoints: experiment info to be added to output data row
  
  noise_levels <- unique(predictions$noise)
  
  # to store results
  errors <- tibble(perc_correct_params = numeric(),
                   perc_density_correct = numeric(),
                   perc_directions_correct = numeric(),
                   perc_correct_cat_density = numeric(),
                   RMSE_density = numeric(),
                   NRMSE_density = numeric(),
                   point_pred_performance_density = numeric(),
                   directions_kappa = numeric(),
                   directions_f1 = numeric(),
                   directions_mcc = numeric(),
                   fold = numeric(),
                   noise = character())
  
  # for each noise level in the data (clean & output)
  for(j in noise_levels){
    # retrieve data with appropriate noise level
    dat_noise <- predictions %>%
      filter(noise == j)
    # for each of the 5 folds
    for(i in 1:5){
      # retrieve correct rows
      dat <- dat_noise %>%
        filter(fold == i)
      
      # compute mean density in training data
      mean_density <- predictions %>%
        filter(fold != i) %>%
        summarise(mean(density))
      
      # compute errors
      errors_new <- dat %>%
        # % correctly predicted direction + density in test set
        summarise(perc_correct_params = sum(
          directions == directions_pred &
            density == round(density_pred)) / nrow(dat) * 100,
          # % correctly predicted density in test set
          perc_density_correct = sum(density == round(density_pred)) /
            nrow(dat) * 100,
          # % correctly predicted direction in test set
          perc_directions_correct = sum(directions == directions_pred) /
            nrow(dat) * 100,
          # % predicted density within 10% of true density in test set
          perc_correct_cat_density = sum(density_pred >= density - 5 &
                                           density_pred <= density + 5) /
            nrow(dat) * 100,
          # RMSE of predicted vs. true density
          RMSE_density = sqrt(sum((density_pred - density)^2))
          / nrow(dat),
          # NRMSE of predicted vs. true density
          NRMSE_density = (sqrt(sum((density_pred - density)^2))
                           / nrow(dat)) / sd(density),
          # point prediction performance for density
          point_pred_performance_density = 1 - 
            sum(sqrt((density_pred - density)^2)) /
            sum(sqrt((density - mean_density)^2)))
      
      # add columns for the metrics for directions
      errors_new$directions_kappa <- NA
      errors_new$directions_f1 <- NA
      errors_new$directions_mcc <- NA
      
      # add the error measures for the directions
      # compute confusion matrix
      cm <- confusionMatrix(as.factor(dat_noise$directions_pred),
                            as.factor(dat_noise$directions),
                            mode = 'everything')
      # retrieve kappa, F1, and MCC
      errors_new$directions_kappa = cm$overall[2]
      errors_new$directions_f1 = cm$byClass[7]
      errors_new$directions_mcc =
        mcc(as.factor(dat_noise$directions_pred),
            as.factor(dat_noise$directions))
      
      # add fold and noise levels & add to errors data frame
      errors_new$fold <- i
      errors_new$noise <- j
      errors <- errors %>%
        add_row(errors_new)
    }
  }
  
  
  # compute the mean statistics for each fold
  errors <- errors %>%
    group_by(noise) %>%
    summarise_all(mean) %>%
    select(- fold) %>%
    # add the experiment info to the df
    add_column(sample_size = sample_size,
               sample_method = sample_method,
               summarise_runs = summarise_runs,
               datapoints = datapoints)
  
  # return data row
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

# plot predicted vs. true directions (confusion matrix)
PlotPredTrueDirection <- function(results, n_plot){
  # results: data frame with fitting results
  # n: sample size for which to plot
  
  # split data based on ticks or no, and extract correct sample size
  results_ticks <- results %>%
    filter(ticks_included == 'yes',
           n == n_plot)
  results_noticks <- results %>%
    filter(ticks_included == 'no',
           n == n_plot)
  
  # compute confusion matrices
  cm_ticks <- confusionMatrix(factor(results_ticks$direction_pred),
                              factor(results_ticks$direction_true),
                              dnn = c('prediction', 'test'))
  cm_noticks <- confusionMatrix(factor(results_noticks$direction_pred),
                                factor(results_noticks$direction_true),
                                dnn = c('prediction', 'test'))
  dat_ticks <- as.data.frame(cm_ticks$table)
  dat_noticks <- as.data.frame(cm_noticks$table)
  
  p1 <- dat_noticks %>%
    ggplot(mapping = aes(x = prediction, y = test, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "#009194") +
    labs(y = 'predicted') +
    theme_minimal() +
    theme(legend.position = 'none',
          panel.border = element_rect(colour = 'lightgrey', fill = NA)) +
    scale_x_discrete(labels = c('4 directions', '8 directions')) +
    scale_y_discrete(labels = c('4 directions', '8 directions')) +
    ggtitle('without ticks')
  
  p2 <- dat_ticks %>%
    ggplot(mapping = aes(x = prediction, y = test, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "#009194") +
    labs(y = 'predicted') +
    theme_minimal() +
    theme(legend.position = 'none',
          panel.border = element_rect(colour = 'lightgrey', fill = NA)) +
    scale_x_discrete(labels = c('4 directions', '8 directions')) +
    scale_y_discrete(labels = c('4 directions', '8 directions')) +
    ggtitle('with ticks')
  
  plot <- p1 + p2
  
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
PlotPercCorrectParams <- function(errors){
  # errors: data frame with calculated error measures
  p1 <- errors %>%
    ggplot(mapping = aes(x = noise, y = perc_correct_params,
                         fill = summarise_runs)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(y = 'density and direction',
         x = 'noise', fill = 'runs summarised') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    facet_grid(~ datapoints)
  
  p2 <- errors %>%
    ggplot(mapping = aes(x = noise, y = perc_density_correct,
                         fill = summarise_runs)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(x = 'noise',
         y = 'tree density',
         fill = 'runs summarised') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    facet_grid(~ datapoints)
  
  p3 <- errors %>%
    ggplot(mapping = aes(x = noise, y = perc_directions_correct,
                         fill = summarise_runs)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(x = 'noise',
         y = 'directions',
         fill = 'runs summarised') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    facet_grid(~ datapoints)
  
  p4 <- errors %>%
    ggplot(mapping = aes(x = noise, y = perc_correct_cat_density,
                         fill = summarise_runs)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    labs(x = 'noise',
         y = 'density within 10%',
         fill = 'runs summarised') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    facet_grid(~ datapoints)
  
  plot <- p1 + p2 + p3 + p4 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A',
                    title = '% correctly predicted') &
    theme(plot.tag = element_text(size = 8))
  
  return(plot)
}

# RMSE etc. of density
PlotErrorsDensity <- function(errors){
  p1 <- errors %>%
    ggplot(mapping = aes(x = noise, y = RMSE_density,
                         fill = summarise_runs)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    facet_grid(~ datapoints) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'RMSE density',
         fill = 'runs summarised y/n')
  
  p2 <- errors %>%
    ggplot(mapping = aes(x = noise, y = point_pred_performance_density,
                         fill = summarise_runs)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    labs(fill = 'runs summarised y/n',
         y = 'point prediction performance') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    facet_grid(~ datapoints)
  
  plot <- p1 + p2 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 8)) &
    xlab(NULL)
  
  return(plot)
}

# plot kappa score, F1 score, and MCC for directions
PlotErrorsDirections <- function(errors, n_plot){
  p1 <- errors %>%
    ggplot(mapping = aes(x = noise, y = directions_kappa,
                         fill = summarise_runs)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'kappa', fill = 'runs summarised y/n') +
    facet_grid(~ datapoints)
  
  p2 <- errors %>%
    ggplot(mapping = aes(x = noise, y = directions_f1,
                         fill = summarise_runs)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'F1 score', fill = 'runs summarised') +
    lims(y = c(0, NA)) +
    facet_grid(~ datapoints)
  
  p3 <- errors %>%
    ggplot(mapping = aes(x = noise, y = directions_mcc,
                         fill = summarise_runs)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'MCC', fill = 'runs summarised') +
    lims(y = c(0, NA)) +
    facet_grid(~ datapoints)
  
  plot <- p1 + p2 + p3 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') &
    theme(plot.tag = element_text(size = 8)) &
    xlab(NULL)
  
  return(plot)
}

# RMSE etc. of burn percentage
PlotRMSEOut <- function(errors, n_plot){
  p1 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = factor(n), y = RMSE_burn,
                         colour = ticks_included)) +
    geom_point(position = position_dodge(width = 0.5),
               size = 3) +
    geom_linerange(aes(x = n, ymin = 0, ymax = RMSE_burn,
                       colour = ticks_included),
                   position = position_dodge(width = 0.5)) +
    labs(colour = 'Ticks included',
         y = 'RMSE') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  p2 <- errors %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = factor(n), y = NRMSE_burn,
                         colour = ticks_included)) +
    geom_point(position = position_dodge(width = 0.5),
               size = 3) +
    geom_linerange(aes(x = n, ymin = 0, ymax = NRMSE_burn,
                       colour = ticks_included),
                   position = position_dodge(width = 0.5)) +
    labs(colour = 'Ticks included',
         y = 'NRMSE') +
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