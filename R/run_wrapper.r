# Run the model using the estimated values of the parameters and apply transformations 
# if transform_sim and/or transform_var is provided.
run_wrapper <- function(model_options, param_values, situation, var, obs_list, 
                        transform_sim=NULL, transform_var=NULL) {

  if (is.null(var)) var <- setdiff(unique(unlist(lapply(obs_list, names))),"Date")
  
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
  
  if (!is.null(transform_var)) {
    
    sim$sim_list <- lapply(sim$sim_list, function(x) {
      for (var in intersect(names(x),names(transform_var))) {
        x[var] <- transform_var[[var]](x[var])
      }
      return(x)
    })
    attr(sim$sim_list, "class") <- "cropr_simulation"
  
  }
      
  return(sim)
  
}
