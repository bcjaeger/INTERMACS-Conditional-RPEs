
# max_depth = 2 # maximum depth of a tree
# eta = 0.02
# gamma = 1 # minimum loss reduction required to make a further partition
# min_child_weight = 1 # minimum sum of instance weight needed in a child
# subsample = 1/2 # subsample ratio of the training instance
# colsample_bytree = 1/2 # subsample ratio of columns for each tree
# colsample_bylevel = 1/2 # subsample ratio of colums for each split.
# model_evnt = 'dead'
# model_time = 'M0_25MD'
# num_iterations = 10

library(rslurm)

slurm_xgb <- function(
  max_depth, # maximum depth of a tree
  eta,
  gamma, # minimum loss reduction required to make a further partition
  min_child_weight, # minimum sum of instance weight needed in a child
  subsample, # subsample ratio of the training instance
  colsample_bytree, # subsample ratio of columns for each tree
  colsample_bylevel, # subsample ratio of colums for each split.
  model_evnt,
  model_time,
  num_iterations # num_iterations can go as high as 1000
){
  
  library(tidyverse)
  library(tidymodels)
  library(magrittr)
  library(xgboost)
  library(gbm)
  library(survival)
  library(pec)
  
  # R objects for this analysis
  folds <- read_rds('data/R objects/folds.rds')
  eval_times <- read_rds("data/R objects/evaluation_times.rds")
  
  # R data for this analysis
  training_file <- paste0('train_', model_evnt, '_', model_time, '.csv')
  training_data <- read_csv(file.path('data','training',training_file))
  
  reci <- recipe(time + status ~ ., data = training_data) %>% 
      #step_center(all_numeric(), -all_outcomes()) %>% 
      #step_scale(all_numeric(), -all_outcomes()) %>% 
      step_nzv(all_predictors()) %>% 
      step_unknown(all_nominal()) %>% 
      step_dummy(all_nominal()) %>% 
      prep()
    
  training_mtrx <- juice(reci)
    
  trn_lab <- training_mtrx$time
  trn_lab[training_mtrx$status==0] %<>% multiply_by(-1)
  training_mtrx %<>% select(-time, -status)
  
  params <- list(
    max_depth = max_depth,
    eta = eta,
    gamma = gamma,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    colsample_bylevel = colsample_bylevel,
    colsample_bynode = colsample_bynode,
    objective='survival:cox',
    eval_metric='cox-nloglik'
  )
  
  cv_full <- xgb.cv(
      params = params,
      data = as.matrix(training_mtrx),
      label = trn_lab,
      folds = folds,
      nrounds = 5000,
      print_every_n = 50,
      early_stopping_rounds = 100
    )
    
  cv_full$evaluation_log[cv_full$best_iteration] %>% 
    select(iter, test_cox_nloglik_mean) %>% 
    as_tibble() %>% 
    mutate(params = list(params))
  
}

mem_per_cpu = 40000
model_evnt = 'dead'
model_time = 'M0_25MD'

jobname <- paste(
  'xgb_tuner', 
  model_evnt, 
  model_time, 
  num_iterations,
  'iters',
  sep="_"
)

pars <- expand.grid(
  max_depth = 4:6, # maximum depth of a tree
  eta = 0.02,
  gamma = c(1, 3, 5), # minimum loss reduction required 
  min_child_weight = c(1,15,30), # minsum of instance weight 
  subsample = 1:3 / 4, # subsample ratio of the training instance
  colsample_bytree = 1:3 / 4, # subsample ratio of columns for each tree
  colsample_bylevel = 1:3 / 4, # subsample ratio of columns for each level
  colsample_bynode = 1:3 / 4, # subsample ratio of columns for each node
  model_evnt = model_evnt,
  model_time = model_time
)

sopt <-
  list(
    partition='short',
    time='11:59:00',
    share=TRUE,
    'mem-per-cpu'=mem_per_cpu
  )

slurm_apply(
  slurm_xgb,
  pars,
  jobname = jobname,
  slurm_options = sopt,
  nodes = nrow(pars),
  cpus_per_node = 1,
  submit = TRUE
)


# Code to read these results
# library(tidyverse)
# library(feather)
# library(pec)
# library(survival)
# library(magrittr)
# 
# model_type <- "xgb"
# model_evnt <- "dead"
# model_time <- "M0_25MD"
# 
# fpath <- file.path(
#   'results', 
#   '_rslurm_xgb_tuner_dead_M1_MD_25_iters'
# )
# 
# results <- list.files(
#   path = fpath,
#   pattern = 'results_',
#   full.names = TRUE
# ) %>% 
#   map_dfr(~read_rds(.x)[[1]])
# 
# nrow(results)
# 
# tmp = results %>% 
#   gather(variable, value, -params) %>% 
#   separate(variable, into = c('stat', 'ftr')) %>% 
#   group_by(stat, ftr) %>% 
#   arrange(desc(value)) %>% 
#   dplyr::slice(1) %>% 
#   unnest_wider(col = params)
# 
# tmp