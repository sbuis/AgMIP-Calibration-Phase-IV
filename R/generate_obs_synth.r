# Generate synthetic observations using the model_wrapper

generate_obs_synth <- function(true_param_values, model_wrapper, model_options, sitNames_corresp, 
                               reqVar_Wrapper, converted_obs_list, transform_sim,
                               simVar_units, varNames_corresp, obsVar_units,  
                               obs_list, obsVar_used, noise_sd=0) {
  
  # run the model_wrapper from default parameter values
  sim_true <- run_wrapper(model_wrapper=model_wrapper, model_options=model_options,
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
  obs_list_synth_true <- res$sim_list
  obs_list_synth_true <- set_units(obs_list_synth_true, obsVar_units)
  # Remove cropr attribute ?
  
  # Check that the number of synthetic observations is the same as the one of the 
  # real observations
  obs_synth_df <- bind_rows(obs_list_synth_true)
  obs_df <- as.data.frame(bind_rows(obs_list))
  obs_synth_nb <- dplyr::summarise(obs_synth_df, across(.fns=function(x) sum(!is.na(x))))
  obs_real_nb <- dplyr::summarise(obs_df, across(.fns=function(x) sum(!is.na(x))))
  if (!identical(obs_synth_nb[,names(obs_real_nb)],obs_real_nb)) {
    print(obs_synth_nb[,names(obs_real_nb)])
    print(obs_real_nb)
    stop("Error generating synthetic observations: number of observations are different between synthetic and real observations.")
  }
            
  # Add gaussian noise
  obs_list_synth <- lapply(obs_list_synth_true, function(x) {
    x %>% mutate(across(
      intersect(names(x),obsVar_used), 
      ~ .x + .x * rnorm(length(.x), sd = noise_sd)
    ))
  }
  )

  # sim_synth <- sim_true
  # tmp <- lapply(names(sim_synth$sim_list_converted), function(x) {
  #   common_var <- intersect(names(sim_synth$sim_list_converted[[x]]), 
  #             names(obs_list_synth[[x]]))
  #   if (!is.null(common_var)) {
  #     common_dates <- intersect(sim_synth$sim_list_converted[[x]]$Date, 
  #                               obs_list_synth[[x]]$Date)
  #     sim_synth$sim_list_converted[[x]][match(common_dates,sim_synth$sim_list_converted[[x]]$Date), common_var] <- 
  #       obs_list_synth[[x]][match(common_dates,obs_list_synth[[x]]$Date), common_var]
  #   }
  #   sim_synth$sim_list_converted[[x]]
  # })
  # names(tmp) <- names(sim_synth$sim_list_converted)
  # sim_synth$sim_list_converted <- tmp
  # attr(sim_synth$sim_list_converted, "class") <- "cropr_simulation"
  # generate_cal_results(sim_synth, obs_list, obsVar_units, obsVar_used, 
  #                      sitNames_corresp, template_path, 
  #                      out_dir, test_case, 
  #                      variety="Janz", varNames_corresp, resVar_names, 
  #                      file_type="synth_values")
  
  # Convert obs_list_synth to simulated names and units
  converted_obs_list_true <- obs_list_synth_true
  converted_obs_list <- obs_list_synth
  if (!is.null(sitNames_corresp)) {
    converted_obs_list_true <- rename_sit(obs_list_synth_true, sitNames_corresp, invert=TRUE)
    converted_obs_list <- rename_sit(obs_list_synth, sitNames_corresp, invert=TRUE)
  }
  converted_obs_list_true <- rename_var(converted_obs_list_true, varNames_corresp, invert=TRUE)
  converted_obs_list <- rename_var(converted_obs_list, varNames_corresp, invert=TRUE)
  converted_obs_list_true <- convert_units(converted_obs_list_true, simVar_units)
  converted_obs_list <- convert_units(converted_obs_list, simVar_units)
  
  # Remove units from obs lists
  converted_obs_list_true <- lapply(converted_obs_list_true,drop_units)
  converted_obs_list <- lapply(converted_obs_list,drop_units)
  obs_list_synth_true  <- lapply(obs_list_synth_true ,drop_units)
  obs_list_synth  <- lapply(obs_list_synth ,drop_units)
  
  # Store True simulated values in cal_4_results_*** and obs format  
  generate_cal_results(sim_true, obs_list, obsVar_units, obsVar_used, 
                       sitNames_corresp, template_path, 
                       out_dir, test_case, 
                       variety, varNames_corresp, resVar_names, 
                       file_type="true_values")
  generate_obs_file(obs_list_synth_true, obsVar_units, obsVar_used, 
                    sitNames_corresp, obs_data_path, out_dir, test_case, 
                    variety, varNames_corresp, resVar_names, file_type="true_values")
  generate_obs_file(obs_list_synth, obsVar_units, obsVar_used, 
                    sitNames_corresp, obs_data_path, out_dir, test_case, 
                    variety, varNames_corresp, resVar_names, file_type=paste0("noisy_values_SDnoise",noise_sd))
  
  # Check that the difference between synthetic observations and simulations is NULL
  # Extract simulated values corresponding to observation sites/dates
  ## In the observation space
  tmp <- CroptimizR:::make_obsSim_consistent(sim_true$sim_list,  
                                             converted_obs_list_true)
  res <- CroptimizR:::intersect_sim_obs(sim_list = tmp$sim_list,
                                        obs_list = tmp$obs_list)
  diff <- crit_ols(res$sim_list, res$obs_list)
  if (diff > 1e-20) {
    stop(paste("Error generating synthetic observations: difference between observations and simulations is not null in the simulation space: diff=",diff))
  }
  ## In the simulation
  tmp <- CroptimizR:::make_obsSim_consistent(sim_true$sim_list_converted,  
                                             obs_list_synth_true)
  res <- CroptimizR:::intersect_sim_obs(sim_list = tmp$sim_list,
                                        obs_list = tmp$obs_list)
  diff <- crit_ols(res$sim_list, res$obs_list)
  if (diff > 1e-20) {
    stop(paste("Error generating synthetic observations: difference between observations and simulations is not null in the observation space: diff=",diff))
  }
  
  return(list(obs_list=obs_list_synth, converted_obs_list=converted_obs_list))

}