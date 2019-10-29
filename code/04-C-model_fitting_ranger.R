
library(tidyverse)
library(tidymodels)
library(ranger)
library(pec)
library(glue)
library(survival)
library(magrittr)
library(gbm)

predictSurvProb.ranger <- function (object, newdata, times, ...) {
  
  ptemp <- ranger:::predict.ranger(
    object, 
    data = newdata, 
    importance = "none"
  )$survival
  
  pos <- prodlim::sindex(
    jump.times = object$unique.death.times,
    eval.times = times
  )
  
  p <- cbind(1, ptemp)[, pos + 1, drop = FALSE]
  
  if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
    stop(
      paste(
        "Prediction matrix has wrong dimensions:",
        "\nRequested newdata x times: ",
        NROW(newdata), " x ", length(times),
        "\nProvided prediction matrix: ",
        NROW(p), " x ", NCOL(p),
        "\n",
        sep = ""
      )
    )
  p
}

outcome <- 'dead'

for(time in c("M0_MD", "M0_25MD", "M1_MD")){
  
}

time_adjust <- switch(
  time,
  "M0_MD" = 0,
  "M0_25MD" = 1/4,
  "M1_MD" = 1,
  stop("time value is unrecognized")
)

eval_times <- read_rds('data/R objects/evaluation_times.rds') %>% 
  subtract(time_adjust)

train <- read_csv(
  file = glue("data/training/{outcome}_{time}.csv"),
  guess_max = 10000
) %>% 
  mutate(time = time - time_adjust) %>% 
  filter(time > 0) # talk to ryan about this

# Do not adjust time value in testing data
# (adjust time value in eval_times instead)
test <- read_csv(
  file = glue("data/testing/{outcome}_{time}.csv"),
  guess_max = 10000
)

reci <- recipe(time + status ~ ., data = train) %>% 
  step_nzv(all_predictors()) %>% 
  prep()

trn <- juice(reci)
tst <- bake(reci, new_data = test)

library(missRanger)

trn_imputed <- missRanger(data = trn, verbose = TRUE)
tst_imputed <- missRanger(data = tst, verbose = TRUE)

# tst_imputed$m0_25_inotrope_therapy <- "No"

mdl_full <- ranger(
  formula = Surv(time, status) ~ ., 
  data = trn_imputed, 
  min.node.size = 30,
  splitrule = 'extratrees',
  num.random.splits = 5,
  verbose = TRUE,
  num.trees = 1000,
  importance = 'impurity_corrected'
)

top_30 <- importance(mdl_full) %>% 
  enframe() %>% 
  arrange(desc(value)) %>% 
  dplyr::slice(1:30) %>% 
  pull(name) %>% 
  c('time','status', .)

mdl_full <- ranger(
  formula = Surv(time, status) ~ ., 
  data = trn_imputed, 
  min.node.size = 30,
  splitrule = 'extratrees',
  num.random.splits = 5,
  verbose = TRUE,
  num.trees = 1000
)

mdl_rdcd <- ranger(
  formula = Surv(time, status) ~ ., 
  data = trn_imputed[, top_30], 
  min.node.size = 30,
  splitrule = 'extratrees',
  num.random.splits = 5,
  verbose = TRUE,
  num.trees = 1000
)

models <- list(
  full = mdl_full,
  rdcd = mdl_rdcd
)

sprobs <- models %>% 
  map(predictSurvProb, newdata = tst_imputed, times = eval_times)

write_rds(models, glue('results/models/rsf_{outcome}_{time}.rds'))
write_rds(sprobs, glue('results/sprobs/rsf_{outcome}_{time}.rds'))


write_csv(
  trn_imputed, 
  glue('data/training/imputes/{outcome}_{time}/missRanger.csv')
)

write_csv(
  tst_imputed, 
  glue('data/testing/imputes/{outcome}_{time}/missRanger.csv')
)

