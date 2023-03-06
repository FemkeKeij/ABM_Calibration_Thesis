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