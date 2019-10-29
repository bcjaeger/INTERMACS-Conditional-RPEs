

# factor: m0_primary_dgn?


# mdl_full <- xgboost(
#   params = params,
#   data = as.matrix(trn_mat),
#   label = trn_lab,
#   nrounds = cv_full$best_iteration,
#   print_every_n = 50
# )
# 
# ftr_rdcd <- xgb.importance(model = mdl_full) %>% 
#   pull(Feature) %>% 
#   .[1:50]
# 
# trn_mat_rdcd <- select_at(trn_mat, ftr_rdcd)
# tst_mat_rdcd <- select_at(tst_mat, ftr_rdcd)
# 
# cv_rdcd <- xgb.cv(
#   params = params,
#   data = as.matrix(trn_mat_rdcd),
#   label = trn_lab,
#   nfold = 10,
#   nrounds = 5000,
#   print_every_n = 50,
#   early_stopping_rounds = 100
# )
# 
# mdl_rdcd <- xgboost(
#   params = params,
#   data = as.matrix(trn_mat_rdcd),
#   label = trn_lab,
#   nrounds = cv_rdcd$best_iteration,
#   print_every_n = 50
# )
# 
# library(midytrees)
# 
# impute_init <- list(training = trn_mat_rdcd, testing = tst_mat_rdcd) %>% 
#   map(kNN_mi, nimpute = 10, composition = 'midy')
# 
# imputes <- impute_init %>% 
#   bind_rows(.id = 'role') %>% 
#   mutate(data = as_xmats(data, formula = ~.)) %>% 
#   select(-miss_strat) %>% 
#   deframe()
# 
# imputes$training_label <- expand_label(trn_lab, 'midy', 10)
# 
# imputes$folds <- gen_fold_indices(
#   ntrain = nrow(trn_mat),
#   nfolds = 10
# )
# 
# cv_midy <- xgb_cv(
#   params = params,
#   data = imputes$training,
#   label = imputes$training_label,
#   folds = imputes$folds,
#   nrounds = 5000,
#   print_every_n = 50,
#   early_stopping_rounds = 100
# )
# 
# mdl_midy <- midytrees::xgb_train(
#   params = params,
#   data = xgb.DMatrix(
#     data = imputes$training,
#     label = imputes$training_label
#   ),
#   nrounds = cv_midy$best_iteration,
#   print_every_n = 50
# )
# 
# midy_trn_prd <- predict(
#   object = mdl_midy, 
#   newdata = imputes$training,
#   outputmargin = TRUE
# ) %>% 
#   pool_preds(
#     nobs = nrow(trn_mat),
#     nimpute = 10,
#     miss_strat = 'midy'
#   )
# 
# midy_tst_prd <- predict(
#   object = mdl_midy, 
#   newdata = imputes$testing,
#   outputmargin = TRUE
# ) %>% 
#   pool_preds(
#     nobs = nrow(tst_mat),
#     nimpute = 10, 
#     miss_strat = 'midy'
#   )
# 
# bh <- basehaz.gbm(
#   t = training$time,
#   delta = training$status,
#   f.x = midy_trn_prd,
#   t.eval = eval_times,
#   smooth = TRUE,
#   cumulative = TRUE
# )
# 
# midy_tst_prb <- matrix(
#   data = 0, 
#   nrow=nrow(tst_mat), 
#   ncol=length(eval_times)
# )
# 
# for(i in 1:length(bh)){
#   midy_tst_prb[,i] <- exp(-exp(midy_tst_prd) * (bh[i]))
# }
# 
# midy_rslt <- tibble(
#   type = 'midy', 
#   mdl = list(mdl_midy), 
#   mat = list(trn_mat_impt),
#   ftr = list(ftr_rdcd),
#   prb = list(midy_tst_prb)
# )
# 
# rslt <- tibble(
#   type = c("full", "rdcd"),
#   mdl = list(mdl_full, mdl_rdcd),
#   mat = list(trn_mat, trn_mat_rdcd)
# ) %>% 
#   mutate(
#     ftr = map(mat, names),
#     prb = pmap(
#       .l = list(mdl, mat, ftr),
#       .f = function(.mdl, .mat, .ftr){
#         
#         bh <- basehaz.gbm(
#           t = training$time,
#           delta = training$status,
#           f.x = predict(
#             .mdl, 
#             newdata = as.matrix(.mat), 
#             outputmargin = TRUE
#           ),
#           t.eval = eval_times,
#           smooth = TRUE,
#           cumulative = TRUE
#         )
#         
#         tst_prd <- predict(
#           .mdl, 
#           newdata = as.matrix(tst_mat[,.ftr]), 
#           outputmargin = TRUE
#         )
#         
#         tst_prb <- matrix(
#           data = 0, 
#           nrow=nrow(tst_mat), 
#           ncol=length(eval_times)
#         )
#         
#         for(i in 1:length(bh)){
#           tst_prb[,i] <- exp(-exp(tst_prd) * (bh[i]))
#         }
#         
#         tst_prb
#         
#       }
#     )
#   ) %>% 
#   bind_rows(midy_rslt)
# 
# concordance <- pec::cindex(
#   object = rslt$prb,
#   formula = Surv(time, status) ~ 1,
#   cens.model = 'cox',
#   data = testing,
#   eval.times = eval_times
# ) %>% 
#   use_series("AppCindex") %>%
#   set_names(rslt$type) %>%
#   bind_cols() %>% 
#   mutate(time = eval_times) %>% 
#   filter(time == max(time)) %>% 
#   select(-time) %>% 
#   mutate_all(as.numeric) %>% 
#   rename_all(~paste0("cstat_",.x))
# 
# int_brier <- pec::pec(
#   object = set_names(rslt$prb, rslt$type),
#   formula = Surv(time, status) ~ 1,
#   cens.model = 'cox',
#   data = testing,
#   exact = FALSE,
#   times = eval_times,
#   start = eval_times[1],
#   maxtime = eval_times[length(eval_times)]
# ) %>% 
#   ibs() %>% 
#   tibble(
#     name = rownames(.),
#     value = as.numeric(.)
#   ) %>% 
#   dplyr::select(name, value) %>% 
#   mutate(
#     value = 1 - value / value[name=='Reference'],
#     value = format(round(100 * value, 1), nsmall=1)
#   ) %>% 
#   filter(name!='Reference') %>% 
#   spread(name, value) %>% 
#   mutate_all(as.numeric) %>% 
#   rename_all(~paste0("bstat_",.x))
# 
# results[[i]] <- bind_cols(
#   int_brier, concordance
# )
# 
# }
# 
# output <- bind_rows(results) %>% 
#   mutate_all(as.numeric) %>% 
#   summarize_all(mean) %>% 
#   mutate(params = list(params))  
# 
# output

# split_names <- 
#   gsub(
#     ".csv", 
#     "", 
#     data_files, 
#     fixed = TRUE
#   )
# 
# train_proportion <- 0.75
# 
# set.seed(329)
# 
# for(f in data_files){
#   
#   train_test_splits <- vector(mode='list', length = 1000)
#   analysis <- read_csv(paste0("data/analysis/",f))
#   
#   for(i in seq_along(train_test_splits)){
#     
#     trn_indx <- sample(
#       x = nrow(analysis),
#       size = round(nrow(analysis)*train_proportion),
#       replace = FALSE
#     )
#     
#     train_test_splits[[i]] <- list(
#       training = trn_indx,
#       testing = setdiff(1:nrow(analysis), trn_indx)
#     )
#     
#   }
#   
#   outfile <- gsub('.csv', '.rds', f)
#   
#   write_rds(
#     train_test_splits, 
#     file.path("data","R objects",paste0("train_test_splits_",outfile))
#   )
#   
# }


# 
# library(tidyverse)
# library(xgboost)
# library(glue)
# library(midy)
# library(rBayesianOptimization)
# 
# source("code/functions/xgb_bayes_opt.R")
# 
# target <- 'dead'
# time <- 'M0_25MD'
# 
# training <- read_csv(
#   glue("data/training/train_{target}_{time}.csv")
# ) %>% 
#   mutate(label = label_for_survival(time, status))
# 
# 
# xgb_label <- training$label
# 
# xgb_data <- training %>%
#   select(-time, -status, -label) %>% 
#   spread_cats() %>% 
#   as.matrix() %>% 
#   xgb.DMatrix(label = xgb_label)
# 
# opt_xgb <- xgb_bayes_opt(
#   trn_dmat = xgb_data,
#   trn_y = xgb_label,
#   objective = 'survival:cox',
#   eval_metric = 'cox-nloglik',
#   eval_maximize = FALSE,
#   nfolds = 15,
#   init_points = 100,
#   n_iter = 100
# )
# 
# params <- opt_xgb %>%
#   use_series('Best_Par') %>%
#   enframe() %>% 
#   spread(name, value) %>% 
#   mutate(
#     eta = eta / 5,
#     objective = 'binary:logistic',
#     eval_metric = 'auc'
#   ) %>% 
#   as.list()
# 
# xgb_cv <- xgboost::xgb.cv(
#   data=xgb_data, 
#   nrounds = 5000, 
#   early_stopping_rounds = 100,
#   print_every_n = 100,
#   nfold = 15,
#   params = params
# )
# 
# xgb_mdl <- xgboost(
#   data = xgb_data, 
#   nrounds = xgb_cv$best_iteration,
#   params = params,
#   verbose = FALSE
# )