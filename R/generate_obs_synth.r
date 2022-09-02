# Generate synthetic observations using the model_wrapper

generate_obs_synth <- function(true_param_values, model_options, sitNames_corresp, 
                               reqVar_Wrapper, converted_obs_list, transform_sim,
                               simVar_units, varNames_corresp, obsVar_units,  
                               obs_list) {
  
  # run the model_wrapper from default parameter values
  sim_true <- run_wrapper(model_options=model_options,
                             param_values=true_param_values,
                             situation=sitNames_corresp, var=reqVar_Wrapper, 
                             obs_list=converted_obs_list,
                             transform_sim=transform_sim)
  sim_true$sim_list_converted <- convert_and_rename(sim_true$sim_list, sitNames_corresp, simVar_units, 
                                          varNames_corresp, obsVar_units)
  
  
  # Extract simulated values corresponding to observation sites/dates
  obs_sim_list <- CroptimizR:::make_obsSim_consistent(sim_true$sim_list_converted,  
                                                      obs_list)
  res <- CroptimizR:::intersect_sim_obs(sim_list = obs_sim_list$sim_list,
                                        obs_list = obs_sim_list$obs_list)
  obs_list_synth <- res$sim_list
  obs_list_synth <- set_units(obs_list_synth, obsVar_units)
  # Remove cropr attribute ?
  
  # Check that the number of synthetic observations is the same as the one of the 
  # real observations
  obs_synth_df <- bind_rows(obs_list_synth)
  obs_df <- as.data.frame(bind_rows(obs_list))
  obs_synth_nb <- dplyr::summarise(obs_synth_df, across(.fns=function(x) sum(!is.na(x))))
  obs_real_nb <- dplyr::summarise(obs_df, across(.fns=function(x) sum(!is.na(x))))
  if (!identical(obs_synth_nb[,names(obs_real_nb)],obs_real_nb)) {
    print(obs_synth_nb[,names(obs_real_nb)])
    print(obs_real_nb)
    stop("Error generating synthetic observations: number of observations are different between synthetic and real observations.")
  }
            
  
  # Convert obs_list_synth to simulated names and units
  converted_obs_list <- obs_list_synth
  if (!is.null(sitNames_corresp))
    converted_obs_list <- rename_sit(obs_list_synth, sitNames_corresp, invert=TRUE)
  converted_obs_list <- rename_var(converted_obs_list, varNames_corresp, invert=TRUE)
  converted_obs_list <- convert_units(converted_obs_list, simVar_units)
  
  # Remove units from obs lists
  converted_obs_list <- lapply(converted_obs_list,drop_units)
  obs_list_synth  <- lapply(obs_list_synth ,drop_units)
  
  # Check that the difference between synthetic observations and simulations is NULL
  # Extract simulated values corresponding to observation sites/dates
  ## In the observation space
  tmp <- CroptimizR:::make_obsSim_consistent(sim_true$sim_list,  
                                             converted_obs_list)
  res <- CroptimizR:::intersect_sim_obs(sim_list = tmp$sim_list,
                                        obs_list = tmp$obs_list)
  diff <- crit_ols(res$sim_list, res$obs_list)
  if (diff > 1e-20) {
    stop(paste("Error generating synthetic observations: difference between observations and simulations is not null in the simulation space: diff=",diff))
  }
  ## In the simulation
  tmp <- CroptimizR:::make_obsSim_consistent(sim_true$sim_list_converted,  
                                             obs_list_synth)
  res <- CroptimizR:::intersect_sim_obs(sim_list = tmp$sim_list,
                                        obs_list = tmp$obs_list)
  diff <- crit_ols(res$sim_list, res$obs_list)
  if (diff > 1e-20) {
    stop(paste("Error generating synthetic observations: difference between observations and simulations is not null in the observation space: diff=",diff))
  }
  
  return(list(obs_list=obs_list_synth, converted_obs_list=converted_obs_list))

}