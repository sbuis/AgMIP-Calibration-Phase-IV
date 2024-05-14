
model_test_wrapper <- function(model_options,
                             param_values = NULL,
                             situation = NULL,
                             var = NULL,
                             dates = NULL,
                             sit_var_dates_mask = NULL,
                             sit_names = lifecycle::deprecated(),
                             var_names = lifecycle::deprecated()) {
  
  var <- c(var,"iplts")
  
  res <- stics_wrapper(model_options,
                       param_values,
                       situation,
                       var,
                       dates,
                       sit_var_dates_mask)

  if (model_options$test_model=="date_format") {
    
    for (sit in names(res$sim_list)) {
      res$sim_list[[sit]]$Date <- as.Date(res$sim_list[[sit]]$Date)
    }
  
  } else if (model_options$test_model=="date_ref") {
    
    for (sit in names(res$sim_list)) {
      res$sim_list[[sit]]$iamfs <- res$sim_list[[sit]]$iamfs - res$sim_list[[sit]]$iplts
      res$sim_list[[sit]]$ilaxs <- res$sim_list[[sit]]$ilaxs - res$sim_list[[sit]]$iplts
      res$sim_list[[sit]]$imats <- res$sim_list[[sit]]$imats - res$sim_list[[sit]]$iplts
    }
    
  } else if (model_options$test_model=="date_bbch90_na") {
    
    for (sit in names(res$sim_list)) {
      # res$sim_list[[sit]]$masec_n <- NA
      res$sim_list[[sit]]$imats <- NA
    }
    
  } else if (model_options$test_model=="early_end_date") {
    
    for (sit in names(res$sim_list)) {
      res$sim_list[[sit]] <- res$sim_list[[sit]][nrow(res$sim_list[[sit]])-10,]
    }
  
  } else if (model_options$test_model=="missing_var") {
    
    for (sit in names(res$sim_list)) {
      res$sim_list[[sit]]$imats <- NULL
    }
    
  } else if (model_options$test_model=="date_bbch90_999") {
    
    for (sit in names(res$sim_list)) {
      res$sim_list[[sit]]$imats <- 999
    }
    
  }  

  return(res)

}

model_test_wrapper_without_var <- function(model_options,
                               param_values = NULL,
                               situation = NULL, ...) {
  
  res <- stics_wrapper(model_options,
                       param_values,
                       situation)
  
  return(res)
  
}


model_test_wrapper_special_char <- function(model_options,
                               param_values = NULL,
                               situation = NULL,
                               var = NULL,
                               dates = NULL,
                               sit_var_dates_mask = NULL,
                               sit_names = lifecycle::deprecated(),
                               var_names = lifecycle::deprecated()) {
  
  print(param_values)
  param_values <- param_values[-grep("[%#$]",param_values)]
  res <- stics_wrapper(model_options,
                       param_values,
                       situation,
                       var,
                       dates,
                       sit_var_dates_mask)
  
  return(res)
  
}
