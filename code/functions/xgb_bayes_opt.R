

library(rBayesianOptimization)

xgb_bayes_opt <- function(
  trn_dmat, 
  trn_y,
  nfolds, 
  eta_range = c(0.05, 0.50),
  max_depth_range = c(1L, 4L),
  min_child_weight_range = c(10L, 40L),
  subsample_range = c(0.2, 0.8),
  colsample_bynode_range = c(0.2, 0.8),
  num_parallel_tree_range = c(1L, 5L),
  objective = 'reg:logistic', 
  eval_metric = 'auc',
  eval_maximize = TRUE,
  early_stopping_rounds = 50,
  init_points = 10,
  n_iter = 20,
  acq = "ucb", 
  kappa = 2.576, 
  eps = 0.0,
  verbose = TRUE
){
  
  test_col <- paste('test', eval_metric, 'mean', sep = '_')
  test_col <- gsub(":", "_", test_col)
  
  cv_folds <- sample(1:nfolds, nrow(trn_dmat), replace = TRUE)
  cv_folds <- map(1:nfolds, ~ which(cv_folds==.x))
  
  xgb_cv_bayes <- function(
    eta,
    max_depth,
    min_child_weight,
    subsample,
    colsample_bynode,
    num_parallel_tree
  ) {
    
    params <- list(
      eta = eta,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bynode = colsample_bynode,
      num_parallel_tree = num_parallel_tree,
      objective = objective,
      eval_metric = eval_metric
    )
    
    cv <- xgb.cv(
      params = params,
      data = trn_dmat, 
      nrounds = 1000,
      early_stopping_rounds = early_stopping_rounds,
      folds = cv_folds, 
      prediction = TRUE, 
      maximize = eval_maximize, 
      verbose = 0
    )
    
    cv$evaluation_log <- as.data.frame(cv$evaluation_log)
    sc <- cv$evaluation_log[[test_col]][cv$best_iteration]
    
    if(!eval_maximize) sc <- sc * (-1)
    
    list(Score = sc, Pred = cv$pred)
    
  }
  
  BayesianOptimization(
    xgb_cv_bayes,
    bounds = list(
      eta = eta_range,
      max_depth = max_depth_range,
      min_child_weight = min_child_weight_range,
      subsample = subsample_range,
      colsample_bynode = colsample_bynode_range,
      num_parallel_tree = num_parallel_tree_range
    ),
    init_grid_dt = NULL,
    init_points = init_points,
    n_iter = n_iter,
    acq = acq, 
    kappa = kappa, 
    eps = eps,
    verbose = verbose
  )
  
  
}



