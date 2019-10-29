
library(obliqueRSF)
library(tidyverse)
library(magrittr)
library(ipa)
library(pec)
library(glue)

outcome <- 'dead'
time <- 'M0_25MD'

time_adjust <- switch(
  time,
  "M0_MD" = 0,
  "M0_25MD" = 1/4,
  "M1_MD" = 1,
  stop("time value is unrecognized")
)

eval_times <- read_rds('data/R objects/evaluation_times.rds') %>% 
  subtract(time_adjust)

params <- read_rds(glue("results/params/xgb_{outcome}_{time}.rds"))

train <- read_csv(
  file = glue("data/training/imputes/{outcome}_{time}/missRanger.csv"),
  guess_max = 10000
) %>% 
  mutate(time = time - time_adjust) %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(time > 0)

# Do not adjust time value in testing data
# (adjust time value in eval_times instead)
test <- read_csv(
  file = glue("data/testing/imputes/{outcome}_{time}/missRanger.csv"),
  guess_max = 10000
)  %>% 
  mutate_if(is.character, as.factor) %>% 
  transfer_factor_levels(from = train)

train <- spread_cats(train)
test <- spread_cats(test)

orsf = ORSF(data = train, ntree = 1000)

sprobs <- predictSurvProb(orsf, newdata = test, times = eval_times)

write_rds(orsf, glue('results/models/orsf_{outcome}_{time}.rds'))
write_rds(sprobs, glue('results/sprobs/orsf_{outcome}_{time}.rds'))


