

library(tidyverse)
library(tidymodels)
library(xgboost.surv)
library(magrittr)
library(MASS)
library(survival)
library(pec)
library(glue)

set.seed(329)
source("code/functions/read_csv_high_guessmax.R")
outcome <- 'dead'

for(time in c('M0_MD', 'M0_25MD', 'M1_MD')){
  
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
  
  imputes <- map(
    .x = train, 
    .f = function(x){
      if(is.factor(x)){
        tb <- table(x)
        names(tb)[which.max(tb)]
      } else {
        mean(x, na.rm=TRUE)
      }
    }
  )
  
  for(i in names(train)){
    na_values <- is.na(train[[i]])
    if(any(na_values)){
      na_indx <- which(na_values)
      train[[i]][na_indx] <- imputes[[i]]
    }
  }
  
  for(i in names(test)){
    na_values <- is.na(test[[i]])
    if(any(na_values)){
      na_indx <- which(na_values)
      test[[i]][na_indx] <- imputes[[i]]
    }
  }
  
  .train <- train %>% 
    cat_spread() %>% 
    as_tibble(.name_repair = 'universal')
  
  .test <- test %>% 
    cat_transfer(from = train) %>% 
    cat_spread() %>% 
    as_tibble(.name_repair = 'universal')

  xnames <- setdiff(names(.train), c('time', 'status')) %>% 
    paste(collapse = ' + ')
  
  full <- glue::glue("Surv(time, status) ~ {xnames}")
  
  cph_full <- stepAIC(
    object = coxph(Surv(time, status) ~ 1, data = .train, x = TRUE),
    scope = as.formula(full),
    trace = FALSE,
    steps = 2
  )
  
  xnames_rdcd <- cph_full$assign %>% 
    enframe() %>% 
    unnest(col=value) %>% 
    filter(value <= 30) %>% 
    pull(name) %>% 
    paste(collapse = ' + ')
  
  rdcd <- glue::glue("Surv(time, status) ~ {xnames_rdcd}")
  cph_rdcd <- coxph(as.formula(rdcd), data=.train, x=TRUE)
  
  models <- list(
    full = cph_full,
    rdcd = cph_rdcd
  )
  
  sprobs <- vector(mode = 'list', length = length(eval_times))
  
  names(sprobs) <- paste('time', eval_times, sep = '_')
  
  for(e in seq_along(eval_times)){
    
    tseq <- seq(
      from = min(.test$time), 
      to = eval_times[e], 
      length.out = 100
    )
    
    sprobs[[e]] <- models %>% 
      map(
        .f = ~ {
          predictSurvProb(
            object = .x, 
            newdata = .test, 
            times = tseq
          )
        }
      )
  }
  
}

  