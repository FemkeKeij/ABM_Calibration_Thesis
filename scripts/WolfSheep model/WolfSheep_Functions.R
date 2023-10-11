################### ERROR COMPUTATION ########################
# Function to compute errors for continuous parameters:
WSComputeErrors <- function(predictions,
                            sample_size, sample_method,
                            summarise_runs, datapoints,
                            double_noise = TRUE){
  # predictions: dataframe with results from fitting
  # sample_size, sample_method, summarise_runs, datapoints: info to
  #     pass on to output data frame
  # double_noise: indicates whether the dataframe includes temporal
  #     noise as well as output noise
  
  # to store the results:
  errors <- tibble(perc_correct_params = numeric(),
                   perc_initial_sheep_correct = numeric(),
                   perc_initial_wolves_correct = numeric(),
                   perc_food_sheep_correct = numeric(),
                   perc_food_wolves_correct = numeric(),
                   perc_sheep_rep_correct = numeric(),
                   perc_wolves_rep_correct = numeric(),
                   perc_grass_correct = numeric(),
                   RMSE_initial_sheep = numeric(),
                   RMSE_initial_wolves = numeric(),
                   RMSE_food_sheep = numeric(),
                   RMSE_food_wolves = numeric(),
                   RMSE_sheep_rep = numeric(),
                   RMSE_wolves_rep = numeric(),
                   RMSE_grass = numeric(),
                   NRMSE_initial_sheep = numeric(),
                   NRMSE_initial_wolves = numeric(),
                   NRMSE_food_sheep = numeric(),
                   NRMSE_food_wolves = numeric(),
                   NRMSE_sheep_rep = numeric(),
                   NRMSE_wolves_rep = numeric(),
                   NRMSE_grass = numeric(),
                   ppp_initial_sheep = numeric(),
                   ppp_initial_wolves = numeric(),
                   ppp_food_sheep = numeric(),
                   ppp_food_wolves = numeric(),
                   ppp_sheep_rep = numeric(),
                   ppp_wolves_rep = numeric(),
                   ppp_grass = numeric(),
                   fold = numeric(),
                   noise = character())
  
  # for each noise level
  if(double_noise){
    noise_levels <- c('clean', 'output noise', 'double noise')
  } else {
    noise_levels <- c('clean', 'output noise')
  }
  
  for(i in noise_levels){
    data_noise <- predictions %>%
      filter(noise == i)
    for(j in 1:5){
      # compute mean values in training data
      mean_vals <- data_noise %>%
        filter(fold != j) %>%
        summarise(mean_initial_number_sheep =
                    mean(initial_number_sheep),
                  mean_initial_number_wolves = 
                    mean(initial_number_wolves),
                  mean_sheep_gain_from_food = 
                    mean(sheep_gain_from_food),
                  mean_wolves_gain_from_food = 
                    mean(wolves_gain_from_food),
                  mean_sheep_reproduce = mean(sheep_reproduce),
                  mean_wolves_reproduce = mean(wolves_reproduce),
                  mean_grass_regrowth = mean(grass_regrowth))
      
      data <- data_noise %>%
        filter(fold == j)
      
      new_errors <- data %>%
        summarise(perc_correct_params =
                    sum(initial_number_sheep_pred ==
                          initial_number_sheep &
                          initial_number_wolves_pred ==
                          initial_number_wolves & 
                          sheep_gain_from_food_pred ==
                          sheep_gain_from_food &
                          wolves_gain_from_food_pred ==
                          wolves_gain_from_food &
                          sheep_reproduce_pred ==
                          sheep_reproduce &
                          wolves_reproduce_pred == 
                          wolves_reproduce & 
                          grass_regrowth_pred ==
                          grass_regrowth) / nrow(data) * 100,
                  perc_initial_sheep_correct =
                    sum(initial_number_sheep ==
                          initial_number_sheep_pred) /
                    nrow(data) * 100,
                  perc_initial_wolves_correct =
                    sum(initial_number_wolves ==
                          initial_number_wolves_pred) /
                    nrow(data) * 100,
                  perc_food_sheep_correct =
                    sum(sheep_gain_from_food ==
                          sheep_gain_from_food_pred) /
                    nrow(data) * 100,
                  perc_food_wolves_correct =
                    sum(wolves_gain_from_food ==
                          wolves_gain_from_food_pred) /
                    nrow(data) * 100,
                  perc_sheep_rep_correct =
                    sum(sheep_reproduce == sheep_reproduce_pred) /
                    nrow(data) * 100,
                  perc_wolves_rep_correct =
                    sum(wolves_reproduce == wolves_reproduce_pred) /
                    nrow(data) * 100,
                  perc_grass_correct =
                    sum(grass_regrowth == grass_regrowth_pred) /
                    nrow(data) * 100,
                  RMSE_initial_sheep =
                    sqrt(sum((initial_number_sheep_pred -
                                initial_number_sheep)^2)) /
                    nrow(data),
                  RMSE_initial_wolves =
                    sqrt(sum((initial_number_wolves_pred -
                                initial_number_wolves)^2)) /
                    nrow(data),
                  RMSE_food_sheep =
                    sqrt(sum((sheep_gain_from_food_pred -
                                sheep_gain_from_food)^2)) /
                    nrow(data),
                  RMSE_food_wolves =
                    sqrt(sum((wolves_gain_from_food_pred -
                                wolves_gain_from_food)^2)) /
                    nrow(data),
                  RMSE_sheep_rep =
                    sqrt(sum((sheep_reproduce_pred -
                                sheep_reproduce)^2)) / nrow(data),
                  RMSE_wolves_rep =
                    sqrt(sum((wolves_reproduce_pred -
                                wolves_reproduce)^2)) / nrow(data),
                  RMSE_grass =
                    sqrt(sum((grass_regrowth_pred -
                                grass_regrowth)^2)) / nrow(data),
                  NRMSE_initial_sheep =
                    (sqrt(sum((initial_number_sheep_pred -
                                 initial_number_sheep)^2)) /
                       nrow(data)) / sd(initial_number_sheep),
                  NRMSE_initial_wolves =
                    (sqrt(sum((initial_number_wolves_pred -
                                 initial_number_wolves)^2)) /
                       nrow(data)) / sd(initial_number_wolves),
                  NRMSE_food_sheep =
                    (sqrt(sum((sheep_gain_from_food_pred -
                                 sheep_gain_from_food)^2)) /
                       nrow(data)) / sd(sheep_gain_from_food),
                  NRMSE_food_wolves =
                    (sqrt(sum((wolves_gain_from_food_pred -
                                 wolves_gain_from_food)^2)) /
                       nrow(data)) / sd(wolves_gain_from_food),
                  NRMSE_sheep_rep =
                    (sqrt(sum((sheep_reproduce_pred -
                                 sheep_reproduce)^2)) / nrow(data)) /
                    sd(sheep_reproduce),
                  NRMSE_wolves_rep =
                    (sqrt(sum((wolves_reproduce_pred -
                                 wolves_reproduce)^2)) /
                       nrow(data)) / sd(wolves_reproduce),
                  NRMSE_grass =
                    (sqrt(sum((grass_regrowth_pred -
                                 grass_regrowth)^2)) /
                       nrow(data)) / sd(grass_regrowth),
                  ppp_initial_sheep =
                    1 - sum(sqrt((initial_number_sheep_pred -
                                    initial_number_sheep)^2)) /
                    sum(sqrt((initial_number_sheep  - 
                                mean_vals$mean_initial_number_sheep)^2)),
                  ppp_initial_wolves =
                    1 - sum(sqrt((initial_number_wolves_pred -
                                    initial_number_wolves)^2)) /
                    sum(sqrt((initial_number_wolves -
                                mean_vals$mean_initial_number_wolves)^2)),
                  ppp_food_sheep =
                    1 - sum(sqrt((sheep_gain_from_food_pred -
                                    sheep_gain_from_food)^2)) /
                    sum(sqrt((sheep_gain_from_food -
                                mean_vals$mean_sheep_gain_from_food)^2)),
                  ppp_food_wolves =
                    1 - sum(sqrt((wolves_gain_from_food_pred -
                                    wolves_gain_from_food)^2)) /
                    sum(sqrt((wolves_gain_from_food -
                                mean_vals$mean_wolves_gain_from_food)^2)),
                  ppp_sheep_rep =
                    1 - sum(sqrt((sheep_reproduce_pred -
                                    sheep_reproduce)^2)) /
                    sum(sqrt((sheep_reproduce -
                                mean_vals$mean_sheep_reproduce)^2)),
                  ppp_wolves_rep =
                    1 - sum(sqrt((wolves_reproduce_pred -
                                    wolves_reproduce)^2)) /
                    sum(sqrt((wolves_reproduce -
                                mean_vals$mean_wolves_reproduce)^2)),
                  ppp_grass =
                    1 - sum(sqrt((grass_regrowth_pred -
                                    grass_regrowth)^2)) /
                    sum(sqrt((grass_regrowth -
                                mean_vals$mean_grass_regrowth)^2)),
                  fold = j,
                  noise = i)
      
      errors <- errors %>%
        add_row(new_errors)
    }
  }
  # compute mean over folds (per noise type)
  errors <- errors %>%
    group_by(noise) %>%
    summarise_all(mean) %>%
    select(- fold) %>%
    # add the experiment info to the df
    add_column(sample_size = sample_size,
               sample_method = sample_method,
               summarise_runs = summarise_runs,
               datapoints = datapoints)
  
  return(errors)  
}


################### VISUALISE ERRORS ########################
PrepErrorsForPlotting <- function(errors_df){
  errors_long <- errors_df %>%
    pivot_longer(cols = - c(noise, sample_size, sample_method,
                            summarise_runs, datapoints),
                 names_to = 'metric',
                 values_to = 'value') %>%
    rowwise %>%
    mutate(metric_general = str_split(metric, '_', 2)[[1]][1],
           entity = str_split(metric, '_', 2)[[1]][2]) %>%
    select(- metric) %>%
    rename(metric = metric_general) %>%
    mutate(entity = ifelse(entity == 'initial_sheep_correct',
                           'initial_sheep', entity),
           entity = ifelse(entity == 'initial_wolves_correct',
                           'initial_wolves', entity),
           entity = ifelse(entity == 'food_sheep_correct',
                           'food_sheep', entity),
           entity = ifelse(entity == 'food_wolves_correct',
                           'food_wolves', entity),
           entity = ifelse(entity == 'sheep_rep_correct',
                           'sheep_rep', entity),
           entity = ifelse(entity == 'wolves_rep_correct',
                           'wolves_rep', entity),
           entity = ifelse(entity == 'grass_correct',
                           'grass', entity),
           entity = ifelse(entity == 'correct_params',
                           'all parameters', entity))
  
  return(errors_long)
}

PlotRMSE <- function(errors_long){
  plot_errors_full_RMSE <- errors_long %>%
    filter(summarise_runs == 'full',
           metric == 'RMSE' | metric == 'NRMSE') %>%
    ggplot(mapping = aes(x = datapoints, y = value,
                         fill = noise)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    facet_wrap(metric ~ entity,
               scales = 'free_y') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
  
  plot_errors_sum_RMSE <- errors_long %>%
    filter(summarise_runs == 'summarised',
           metric == 'RMSE' | metric == 'NRMSE') %>%
    ggplot(mapping = aes(x = datapoints, y = value,
                         fill = noise)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    facet_wrap(metric ~ entity,
               scales = 'free_y') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
  
  return(list(plot_errors_full_RMSE,
              plot_errors_sum_RMSE))
}

PlotPPP <- function(errors_long){
  plot_errors_full_ppp <- errors_long %>%
    filter(summarise_runs == 'full',
           metric == 'ppp' | metric == 'perc') %>%
    ggplot(mapping = aes(x = datapoints, y = value,
                         fill = noise)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    facet_wrap(metric ~ entity,
               scales = 'free_y') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
  
  plot_errors_sum_ppp <- errors_long %>%
    filter(summarise_runs == 'summarised',
           metric == 'ppp' | metric == 'perc') %>%
    ggplot(mapping = aes(x = datapoints, y = value,
                         fill = noise)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    facet_wrap(metric ~ entity,
               scales = 'free_y') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
  
  return(plot_errors_full_ppp,
         plot_errors_sum_ppp)
}