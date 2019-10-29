
library(tidyverse)
library(rsample)
library(readr)
library(glue)
library(ipa)

data_files <- list.files(path = "data/full/")

for(f in data_files){
  
  all_data <- read_csv(
    file.path('data','full',f),
    na = c("NA", "", "(Missing)"),
    guess_max = 19433
  ) %>% 
    mutate_if(is.character, as.factor)
  
  trn_data <- all_data %>% 
    filter(m0_impl_yr %in% c(2012, 2013, 2014, 2015)) %>% 
    select(-m0_impl_yr) %>% 
    droplevels()
  
  # folds are stratified by status - hence, different folds
  # are created for each outcome variable.
  folds <- xgb_folds(trn_data, nfolds = 100, strata = status)
  
  tst_data <- all_data %>% 
    filter(m0_impl_yr %in% c(2016, 2017)) %>% 
    select(-m0_impl_yr) %>% 
    transfer_factor_levels(from = trn_data)
  
  if( all(colnames(trn_data) == colnames(tst_data)) ) {
    
    write_csv(trn_data, file.path('data','training',f))
    write_csv(tst_data, file.path('data','testing',f))
    
    f_rds <- gsub('.csv','.rds',f, fixed=TRUE)
    
    write_rds(folds, file.path('data', 'training', 'folds', f_rds))
    
  } else {
    
    stop("Check columns")
    
  }
  
  
}









