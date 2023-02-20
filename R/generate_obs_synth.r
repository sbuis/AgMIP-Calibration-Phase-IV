# Generate synthetic observations using the model_wrapper

generate_obs_synth <- function(true_param_values, model_wrapper, model_options, sitNames_corresp, 
                               reqVar_Wrapper, converted_obs_list, transform_sim,
                               simVar_units, varNames_corresp, obsVar_units,  
                               obs_list, obsVar_used, noise_sd=0, descr_ref_date) {
  
  # run the model_wrapper from default parameter values
  sim_true <- run_wrapper(model_wrapper=model_wrapper, model_options=model_options,
                             param_values=true_param_values,
                             situation=sitNames_corresp, var=reqVar_Wrapper, 
                             obs_list=converted_obs_list,
                             transform_sim=transform_sim)
  sim_true$sim_list_converted <- convert_and_rename(sim_true$sim_list, sitNames_corresp, simVar_units, 
                                          varNames_corresp, obsVar_units)
  
  # Extract simulated values corresponding to observation sites/dates
  # ... except for harvest date for which the simulated date of maturity is used
  ref_date <- get_reference_date(descr_ref_date, template_path)
  mask <- obs_list
  for (sit in names(mask)) {
    jul_BBCH90 <- tail(sim_true$sim_list_converted[[sit]]$Date_BBCH90,n=1)
    Date_BBCH90 <- as.Date(as.numeric(jul_BBCH90),
                           origin=ref_date[[sit]],
                           format="%Y-%m-%d")[[1]]
    mask[[sit]][nrow(mask[[sit]]),"Date"] <- Date_BBCH90
    ## check that the maturity date is posterior to the last observation date ...
    ## in this case warn the user and set harvest date later
    if (nrow(mask[[sit]]) > 1) {
      if (mask[[sit]][nrow(mask[[sit]]),"Date"] <= mask[[sit]][nrow(mask[[sit]])-1,"Date"]) {
        mask[[sit]][nrow(mask[[sit]]),"Date"] <- mask[[sit]][nrow(mask[[sit]])-1,"Date"] + 5
      }
    }
  }
  
  obs_sim_list <- CroptimizR:::make_obsSim_consistent(sim_true$sim_list_converted,  
                                                      mask)
  res <- CroptimizR:::intersect_sim_obs(sim_list = obs_sim_list$sim_list,
                                        obs_list = obs_sim_list$obs_list)
  obs_list_synth_true <- lapply(names(res$sim_list), function(sit) {
    res$sim_list[[sit]] <- res$sim_list[[sit]][,names(res$obs_list[[sit]])]
    res$sim_list[[sit]][is.na(res$obs_list[[sit]])] <- NA
    res$sim_list[[sit]]
  })
  names(obs_list_synth_true) <- names(res$sim_list)
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
            
  # Add (truncated) gaussian noise
  var_dates <- obsVar_used[grepl("Date",obsVar_used)]
  var_others <- setdiff(obsVar_used, var_dates)
  if (noise_sd>0) {
    obs_list_synth <- lapply(obs_list_synth_true, function(x) {
      x %>% 
        mutate(across(
          intersect(names(x),var_dates), 
          ~ .x + as_units(truncnorm::rtruncnorm(length(.x), a=-6 , b=6, sd=2),"d")
        )) %>%
        mutate(across(
          intersect(names(x),var_others), 
          ~ .x + .x * truncnorm::rtruncnorm(length(.x), a=-3*noise_sd , b=3*noise_sd, sd=noise_sd)
        ))
    }
    )
  } else {
    obs_list_synth <- obs_list_synth_true
  }
  
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
                       file_type="true_values", use_obs_synth=TRUE, sim_true=sim_true, 
                       descr_ref_date=descr_ref_date)
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
  
  return(list(obs_list=obs_list_synth, converted_obs_list=converted_obs_list, sim_true=sim_true))

}