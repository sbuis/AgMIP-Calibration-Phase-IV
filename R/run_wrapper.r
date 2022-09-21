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
