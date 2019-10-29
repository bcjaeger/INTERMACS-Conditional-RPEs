

library(tidyverse)
library(tidymodels)
library(xgboost)
library(xgboost.surv)
library(magrittr)
library(gbm)
library(glue)

set.seed(329)

outcome <- 'dead'
time <- 'M0_25MD'

for(time in c('M0_MD', 'M0_25MD', 'M1_MD')){
  
  #params <- read_rds(glue("results/params/xgb_dead_M0_25MD.rds"))
  params <- read_rds(glue("results/params/xgb_{outcome}_{time}.rds"))
  eval_times <- read_rds("data/R objects/evaluation_times.rds")
  
  train <- .read_csv(glue("data/training/{outcome}_{time}.csv")) %>% 
    mutate_if(is.character, as.factor)
  
  test <- .read_csv(glue("data/testing/{outcome}_{time}.csv")) %>% 
    mutate_if(is.character, as.factor)
  
  # columns in test that have nothing but missing values
  all_na <- map_lgl(test, ~all(is.na(.x)))
  
  # gotta remove em
  train[, names(which(all_na))] = NULL
  test[, names(which(all_na))] = NULL
  
  train_sgb <- train %>% 
    cat_spread() %>% 
    as_sgb_data(status = status, time = time)
  
  test_sgb <- test %>% 
    cat_transfer(from = train) %>% 
    cat_spread() %>% 
    as_sgb_data(status = status, time = time)
  
  cv_full <- xgb.cv(
    nfold = 30,
    data = train_sgb$data,
    label = train_sgb$label,
    nrounds = 5000,
    params = params,
    print_every_n = 50,
    early_stopping_rounds = 100
  )
  
  fit_full <- sgb_fit(
    sgb_df = train_sgb, 
    nrounds = cv_full$best_iteration,
    params = params
  )
  
  top_30 <- xgb.importance(model = fit_full$fit) %>% 
    dplyr::slice(1:30) %>% 
    pull(Feature)
  
  train_sgb_rdcd <- train_sgb
  train_sgb_rdcd$data <- train_sgb_rdcd$data[, top_30]
  
  train_sgb_rdcd <- train_sgb
  train_sgb_rdcd$data <- train_sgb_rdcd$data[, top_30]
  
  fit_rdcd <- sgb_fit(
    sgb_df = train_sgb_rdcd, 
    nrounds = cv_full$best_iteration,
    params = params
  )
  
  models <- list(
    full = fit_full,
    rdcd = fit_rdcd
  )
  
  sprobs <- vector(mode = 'list', length = length(eval_times))
  
  names(sprobs) <- paste('time', eval_times, sep = '_')
  
  for(e in seq_along(eval_times)){
    
    tseq <- seq(
      from = min(test$time), 
      to = eval_times[e], 
      length.out = 100
    )
    
    sprobs[[e]] <- models %>% 
      map(
        .f = ~ {
          ftrs <- .x$fit$feature_names
          predict(
            object = .x, 
            new_data = test_sgb$data[,ftrs], 
            eval_times = tseq
          )
        }
      )
    
  }
  
  write_rds(models, glue('results/models/xgb_{outcome}_{time}.rds'))
  write_rds(sprobs, glue('results/sprobs/xgb_{outcome}_{time}.rds'))
  
}




