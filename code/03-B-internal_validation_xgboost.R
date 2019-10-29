
library(tidyverse)

outcome <- 'dead'
time <- 'M1_MD'

files <- list.files(
  glue("results/_rslurm_xgb_tuner_{outcome}_{time}_iters/"),
  pattern = 'results',
  full.names = TRUE
)

data <- vector(mode='list', length = length(files))

for(f in 1:length(files)){
  if (f%%100 == 0) print(f)
  data[[f]] <- read_rds(files[f])
}

output <- data %>% 
  reduce(bind_rows) %>% 
  arrange(test_cox_nloglik_mean) 

params <- output %>% 
  dplyr::slice(1) %>% 
  pull(params) %>%
  .[[1]]

write_rds(params, glue("results/params/xgb_{outcome}_{time}.rds"))
