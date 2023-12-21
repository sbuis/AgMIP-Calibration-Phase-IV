# Run the model using the estimated values of the parameters and apply transformations 
# if transform_sim and/or transform_var is provided.
run_wrapper <- function(model_wrapper, model_options, param_values, situation, var, obs_list, 
                        transform_sim=NULL, transform_var=NULL) {

  if (is.null(var)) var <- setdiff(unique(unlist(lapply(obs_list, names))),"Date")
  
  # Compute parameter values in case there are some equality constraints defined in parma_values
  param_values <- CroptimizR:::compute_eq_const(param_values, NULL)
  
  if ( (("situation" %in% names(formals(model_wrapper))) & ("var" %in% names(formals(model_wrapper)))) ) {
    
    sim <- model_wrapper(model_options = model_options,
                         param_values = param_values,
                         situation = situation,
                         var = var)
    
  } else if ( ("sit_names" %in% names(formals(model_wrapper))) & ("var_names" %in% names(formals(model_wrapper)))) {
    
    sim <- model_wrapper(model_options = model_options,
                         param_values = param_values,
                         sit_names = situation,
                         var_names = var)
    
  }
  
  if (!is.null(transform_sim)) {
    
    sim <- transform_sim(model_results=sim, obs_list=obs_list,
                         param_values=param_values,
                         model_options=model_options)
    
  }
  
  sim$sim_list <- apply_transform_var(sim$sim_list, transform_var)
      
  return(sim)
  
}

apply_transform_var <- function(sim_list, transform_var) {
  
  if (!is.null(transform_var)) {
    
    sim_list <- lapply(sim_list, function(x) {
      for (var in intersect(names(x),names(transform_var))) {
        x[var] <- transform_var[[var]](x[var])
      }
      return(x)
    })
    attr(sim_list, "class") <- "cropr_simulation"
    
  }
  
  return(sim_list) 
  
}

check_run_wrapper <- function(sim, obs_list, protocol_path) {
  
  is_present <- names(obs_list) %in% names(sim$sim_list)
  if (any(!is_present)) {
    stop(paste("Situation(s)",paste(names(obs_list)[!is_present],collapse = ","),
               "not simulated by your model wrapper although it is mandatory.",
               "\nPlease check your model wrapper and protocol description file."))
  }
  
  for (sit in names(obs_list)) {
    var_obs <- names(obs_list[sit])
    var_sim <- names(sim$sim_list[sit])
    is_present <- var_obs %in% var_sim
    if (any(!is_present)) {
      stop(paste("Variable(s)",paste(var_obs[!is_present],collapse = ","),
                 "not simulated by your model wrapper for situation",sit,"although defined in the protocol description file",protocol_path,
                 "\nPlease check your model wrapper and protocol description file."))
    }
    
    is_present <- obs_list[[sit]]$Date %in% sim$sim_list[[sit]]$Date
    if (any(!is_present)) {
      stop(paste("Date(s)",paste(obs_list[[sit]]$Date[!is_present],collapse = ","),
                 "not simulated by your model wrapper for situation",sit,"although there are observations for this date(s).",
                 "\nPlease allow your model to return simulated results at least up to this date."))
    }
  }
  
}
