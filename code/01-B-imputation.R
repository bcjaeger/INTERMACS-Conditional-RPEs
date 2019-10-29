

library(tidyverse)
library(tidymodels)
library(magrittr)
library(ipa)
library(glue)

data_files <- file.path('data','training') %>% 
  list.files(pattern = '.csv')

f <- data_files[1]

# only run once to create directories.
# new_dirs <- gsub('.csv', '', data_files)
# for(i in new_dirs){
#   dir.create(file.path('data','training','imputes', i))
#   dir.create(file.path('data','testing','imputes', i))
# }


# Import testing data -----------------------------------------------------

test <- read_csv(
  file.path('data','testing', f),
  guess_max = 10000
)

# Identify columns with all missing values
all_na <- map_lgl(test, ~all(is.na(.x))) %>% 
  enframe() %>% 
  filter(value) %>% 
  pull(name)

# Remove these
test[, all_na] = NULL

# Import training data ----------------------------------------------------

train <- read_csv(
  file.path('data','training', f),
  guess_max = 20000
) %>% 
  select_at(names(test)) %>% # keep just cols in test
  mutate_if(is.character, as.factor) # turn chars to factors

test %<>% # do the same thing in testing data
  mutate_if(is.character, as.factor) %>% 
  transfer_factor_levels(from = train)

# KNN Imputation ----------------------------------------------------------

knn_brew <- malt_nbrs(train, outcome = c(time, status)) %>% 
  mash_nbrs(n_impute = 10, step_size = 1, min_neighbors = 5) %>% 
  boil(verbose = TRUE) %>% 
  ferment(new_data = test, verbose = FALSE) %>% 
  bottle(flavor = 'tibble')

knn_brew

# rgr_brew <- malt_rngr(train, outcome = c(time, status)) %>% 
#   mash_rngr(n_impute = 10, step_size = 5, min_nodesize = 5) %>% 
#   boil(num.trees = 100, verbose = TRUE) %>% 
#   ferment(new_data = test) %>% 
#   bottle(flavor = 'tibble')




