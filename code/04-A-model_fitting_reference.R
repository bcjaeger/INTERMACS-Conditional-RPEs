

library(tidyverse)
library(survival)
library(magrittr)
library(glue)
library(pec)

outcome <- "dead"


for(time in c("M0_MD", "M0_25MD", "M1_MD")){
  
  model_formula <- switch(
    time, 
    "M0_MD" = Surv(time, status) ~ 
      age2 + 
      bmi + 
      ccs + 
      periph_vasc +
      current_smoker + 
      non_comp +
      m0_ra_pres + 
      m0_cv_pres + 
      m0_bili_total_mg_dl + 
      m0_lvedd + 
      m0_intervention_48_hrs_dialysis + 
      m0_bun_mg_dl + 
      m0_albumin_g_dl +
      m0_prev_cardiac_oper_none,
    "M0_25MD" = Surv(time, status) ~ 
      age2 + 
      bmi + 
      ccs + 
      periph_vasc +
      current_smoker + 
      non_comp +
      m0_25_ra_pres + 
      m0_25_cv_pres + 
      m0_25_bili_total_mg_dl + 
      m0_25_lvedd + 
      m0_intervention_48_hrs_dialysis + 
      m0_25_bun_mg_dl + 
      m0_25_albumin_g_dl +
      m0_prev_cardiac_oper_none,
    "M1_MD" = Surv(time, status) ~ 
      age2 + 
      bmi + 
      ccs + 
      periph_vasc +
      current_smoker + 
      non_comp +
      m0_25_ra_pres + 
      m0_25_cv_pres + 
      m1_bili_total_mg_dl + 
      m1_lvedd + 
      m0_intervention_48_hrs_dialysis + 
      m1_bun_mg_dl + 
      m1_albumin_g_dl +
      m0_prev_cardiac_oper_none
  ) 
  
  data <- list(
    train = read_csv(
      glue("data/training/{outcome}_{time}.csv"), 
      guess_max = 10000
    ),
    test = read_csv(
      glue("data/testing/{outcome}_{time}.csv"), 
      guess_max = 10000
    )   
  ) %>% 
    map(
      .f = function(df){
        mutate(
          df, 
          ccs = m0_px_profile == '1 Critical Cardiogenic Shock',
          ccs = as.numeric(ccs),
          current_smoker = if_else(
            m0_cc_curr_smoking_m == "Yes" | 
              m0_cc2_curr_smoking_m == "Yes",
            true = 1, 
            false = 0
          ),
          periph_vasc = if_else(
            m0_cc_periph_vasc_disease_m == "Yes" | 
              m0_cc2_periph_vasc_disease_m == "Yes",
            true = 1, 
            false = 0
          ),
          non_comp = if_else(
            m0_cc2_rptd_non_compliance_m == 'Yes' | 
              m0_cc_rptd_non_compliance_m == 'Yes',
            true = 1, 
            false = 0
          ),
          age2 = m0_age_deident^2,
          bmi = m0_wgt_kg / (m0_hgt_cm/100)^2
        ) %>%
          dplyr::select(all.vars(model_formula)) %>% 
          mutate_if(is.character, as.factor)
      }
    )
  
  imputes <- map(
    .x = data$train, 
    .f = function(x){
      if(is.factor(x)){
        tb <- table(x)
        names(tb)[which.max(tb)]
      } else {
        mean(x, na.rm=TRUE)
      }
    }
  )
  
  data <- map(
    .x = data, 
    .f = function(dat){
      for(i in names(dat)){
        na_values <- is.na(dat[[i]])
        if(any(na_values)){
          na_indx <- which(na_values)
          dat[[i]][na_indx] <- imputes[[i]]
        }
      }
      dat
    }
  )
  
  # sfit <- survfit(Surv(time, status) ~ ccs, data = data$train)
  # survminer::ggsurvplot(sfit, data = data$train)
  
  model <- coxph(
    model_formula,
    x = TRUE,
    data = data$train
  ) 
  
  eval_times <- read_rds("data/R objects/evaluation_times.rds")
  
  sprobs <- vector(mode = 'list', length = length(eval_times))
  
  names(sprobs) <- paste('time', eval_times, sep = '_')
  
  for(e in seq_along(eval_times)){
    
    tseq <- seq(
      from = min(test$time), 
      to = eval_times[e], 
      length.out = 100
    )
    
    sprobs[[e]] <- predictSurvProb(
      object = model, 
      newdata = data$test, 
      times = tseq
    )
    
  }
  
  write_rds(model, path = glue('results/models/cph_{outcome}_M0_MD.rds'))
  write_rds(sprobs, path = glue('results/sprobs/cph_{outcome}_{time}.rds'))
  
  
}


