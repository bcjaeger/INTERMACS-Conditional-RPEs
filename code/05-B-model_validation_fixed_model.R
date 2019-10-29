library(pec)
library(glue)
library(survival)
library(tidyverse)
library(survival)
library(magrittr)

outcome <- "dead"
model <- "xgb"

source("code/functions/read_csv_high_guessmax.R")

w00 <- read_rds(glue("results/sprobs/{model}_{outcome}_M0_MD.rds"))
w01 <- read_rds(glue("results/sprobs/{model}_{outcome}_M0_25MD.rds"))
w04 <- read_rds(glue("results/sprobs/{model}_{outcome}_M1_MD.rds"))

tst <- list(
  w00 = .read_csv(glue("data/testing/{outcome}_M0_MD.csv")),
  w01 = .read_csv(glue("data/testing/{outcome}_M0_25MD.csv")),
  w04 = .read_csv(glue("data/testing/{outcome}_M1_MD.csv"))
)

eval_times <- read_rds("data/R objects/evaluation_times.rds")

if(is.null(names(w00$time_1))){
  
  sprobs <- list(
    w00 = w00,
    w01 = w01,
    w04 = w04
  ) %>% 
    enframe() %>% 
    unnest_longer(col = value) %>% 
    mutate(tst = rep(tst, each = length(eval_times))) %>% 
    rename(time = value_id) %>% 
    mutate(time = str_replace(time, 'time_', ''), mdl = 'full') %>% 
    dplyr::select(name, mdl, time, prediction=value, tst)
  
} else {
  
  sprobs <- list(
    w00 = w00,
    w01 = w01,
    w04 = w04
  ) %>% 
    enframe() %>% 
    unnest_longer(col = value) %>% 
    mutate(tst = rep(tst, each = length(eval_times))) %>% 
    rename(time = value_id) %>% 
    mutate(time = str_replace(time, 'time_', '')) %>% 
    unnest_longer(col = value) %>% 
    rename(mdl = value_id) %>% 
    dplyr::select(name, mdl, time, prediction=value, tst)
  
}

cstats <- sprobs %>% 
  mutate(
    cstat = pmap_dbl(
      .l = list(time, prediction, tst),
      .f = function(.time, .pred, .tst){
        cindex(
          object = .pred[,ncol(.pred),drop=FALSE],
          formula = Surv(time, status) ~ m0_age_deident,
          eval.times = as.numeric(.time),
          cens.model = 'cox',
          data = .tst
        ) %>% 
          use_series("AppCindex") %>%
          .[[1]]
      }
    )
  ) %>% 
  mutate(cstat = if_else(name=='w04' & time == '1', NA_real_, cstat))

write_rds(
  select(cstats, name, mdl, time, cstat), 
  glue('results/cstats/{outcome}_{model}.rds')
)

cstats %>% 
  dplyr::select(name, mdl, time, cstat) %>% 
  pivot_wider(names_from = c(mdl, time), values_from = cstat)

ibstats <- sprobs %>% 
  mutate(
    bstat = pmap_dbl(
      .l = list(time, prediction, tst),
      .f = function(.time, .pred, .tst){
        
        time_seq <- seq(
          from = min(.tst$time), 
          to = .time, 
          length.out = 100
        )
        .ibs <- pec(
          object = .pred,
          exact = FALSE,
          data = .tst,
          cens.model = 'cox',
          times = time_seq,
          start = time_seq[1],
          formula = Surv(time, status) ~ m0_age_deident,
          maxtime = time_seq[length(time_seq)]
        ) %>% 
          ibs()
        
        #.ibs[-1,]
        1 - .ibs[-1,] / .ibs[1,]
        
      }
    )
  )

write_rds(
  select(ibstats, name, mdl, time, bstat), 
  glue('results/ibstats/{outcome}_{model}.rds')
)

ibstats %>% 
  dplyr::select(name, mdl, time, bstat) %>% 
  pivot_wider(names_from = c(mdl, time), values_from = bstat) %>% 
  dplyr::select(name, starts_with('full'), starts_with('rdcd'))



