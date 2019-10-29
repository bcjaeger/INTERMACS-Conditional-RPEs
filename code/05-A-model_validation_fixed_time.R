
library(pec)
library(tidyverse)
library(glue)
library(survival)

outcome <- "dead"
time <- "M0_25MD"

xgb <- read_rds(glue('results/sprobs/xgb_{outcome}_{time}.rds'))
#rsf <- read_rds(glue('results/sprobs/rsf_{outcome}_{time}.rds'))
ref <- read_rds(glue('results/sprobs/cph_{outcome}_{time}.rds'))
#osf <- read_rds(glue('results/sprobs/orsf_{outcome}_{time}.rds'))

sprobs <- list(
  xgb_full = xgb$full,
  xgb_rdcd = xgb$rdcd, # 30 selected features
  rsf_full = rsf$full,
  rsf_rdcd = rsf$rdcd,
  osf_full = osf,
  ens_full = (rsf$full + xgb$full) / 2,
  ens_rdcd = (rsf$rdcd + rsf$full) / 2,
  ref = ref
)

# adjust?
# last event occurs at t=19 months
# 3rd quartile is 6.14
# should eval times be 1, 3, 6, and 12?
eval_times <- read_rds("data/R objects/evaluation_times.rds")

eval_times <- eval_times[-length(eval_times)]
sprobs %<>% map(~.x[,-ncol(.x)])

eval_data <- read_csv("data/testing/dead_M0_25MD.csv", guess_max = 10000) %>% 
  select(time, status)

cstats = cindex(
  object = sprobs,
  formula = Surv(time, status) ~ 1,
  eval.times = eval_times,
  cens.model = 'cox',
  data = eval_data
)

cstats

bstats = pec(
  exact = FALSE,
  object = sprobs,
  data = eval_data,
  cens.model = 'cox',
  times = eval_times,
  start = eval_times[1],
  formula = Surv(time, status) ~ 1,
  maxtime = eval_times[length(eval_times)]
)

plot(bstats)

ibstats <- ibs(bstats)

ref_ibstat <- ibstats['Reference',1]

1 - ibstats[-1,1] / ref_ibstat

