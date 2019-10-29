
library(dataPreparation)
library(tidyverse)
library(magrittr)

loop_over <- expand.grid(
  data = paste0(c("M0_MD","M0_25MD", "M1_MD"), "_Conditional.csv"),
  label = c("txpl", "cess", "dead"),
  stringsAsFactors = FALSE
) %>% 
  mutate(
    out_file = paste(label, data, sep = "_"),
    out_file = gsub(
      "_Conditional",
      "",
      out_file,
      fixed = TRUE
    )
  )


for(i in 1:nrow(loop_over)){
  
  recoder <- c(
    time = "pt_outcome_months",
    status = paste0('pt_outcome_',loop_over$label[i]) 
  )
  
  data <- read_csv(
    file.path("data", "source", loop_over$data[i]),
    na = c('', ' ', "NA", "Missing", "Unknown"),
    trim_ws = TRUE,
    guess_max = 19433
  ) %>%
    set_names(tolower(names(.))) 
  
  # this column is missing after 2016?
  if("m0_25_inotrope_therapy" %in% names(data)){
    data %<>% select(-m0_25_inotrope_therapy)
  }
  
  vars_to_keep <- data %>% 
    summarize_all(~mean(is.na(.x))) %>% 
    gather(variable, miss_perc) %>% 
    filter(miss_perc <= 0.90) %>% 
    pull(variable)
  
  data %<>% select_at(vars_to_keep)
  
  cnst <- whichAreConstant(data, verbose = TRUE)
  
  data[, cnst] = NULL
  
  dbls <- whichAreInDouble(data, verbose = TRUE)
  
  data[, dbls] = NULL
  
  data %>% 
    as_tibble() %>% 
    rename(!!!recoder) %>% 
    filter(time > 0) %>% 
    select(
      -ends_with("_id"),
      -ends_with("_i"),
      -starts_with("pt_outcome")
    ) %>% 
    write_csv(paste0("data/full/",loop_over$out_file[i]))
  
}


eval_times = c(1, 3, 6, 12)
write_rds(eval_times, "data/R objects/evaluation_times.rds")



