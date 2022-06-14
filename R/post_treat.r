post_treat <- function(model_options, final_forced_param_values, res_it3,
                       sitNames_corresp, wrapper_outputs, obs, 
                       transform_sim, template_path, out_dir, test_case, variety) {
  
  converted_obs_list <- obs$converted_obs_list
  obs_list <- obs$obs_list
  obsVar_names <- obs$obsVar_names
  obsVar_units <- obs$obsVar_units
  obsVar_used <- obs$obsVar_used
  var_date <- obsVar_used[grepl("Date",obsVar_used)]
  
  sim_final <- run_wrapper(model_options=model_options,
                                 param_values=c(final_forced_param_values,res_it3$final_values),
                                 situation=sitNames_corresp, var=wrapper_outputs, 
                                 obs_list=converted_obs_list,
                                 transform_sim=transform_sim)
  
  # Convert simulations to observation space (names of situations and variables, units)
  sim_final_converted <- sim_final
  if (!is.null(sitNames_corresp))
    sim_final_converted$sim_list <- rename_sit(sim_final_converted$sim_list, 
                                               sitNames_corresp, invert=FALSE)
  sim_final_converted$sim_list <- set_units(sim_final_converted$sim_list, simVar_units)
  sim_final_converted$sim_list <- rename_var(sim_final_converted$sim_list, 
                                             varNames_corresp, invert=FALSE)
  sim_final_converted$sim_list <- convert_units(sim_final_converted$sim_list, obsVar_units)
  sim_final_converted$sim_list  <- lapply(sim_final_converted$sim_list ,drop_units)
  attr(sim_final_converted$sim_list, "class") <- "cropr_simulation"
  
  # Plot the results
  p <- plot(sim_final_converted$sim_list, obs=obs_list, type="scatter")
  CroPlotR::save_plot_pdf(p, out_dir, file_name = "scatterPlots")
    
  # Compute stats criteria
  stats <- summary(sim_final_converted$sim_list, obs=obs_list, stats=c("MSE", "Bias2","SDSD","LCS"))
  write.table(dplyr::select(stats,-group, -situation),file = file.path(out_dir,"stats.txt"),row.names = FALSE, quote=FALSE)
  
  
  # Generate the required results file
  
  ## Read the template
  template_df <- read.table(template_path,
                            header = TRUE, stringsAsFactors = FALSE)
  if ("Date_sowing" %in% names(template_df)) {
    template_df_ext <- template_df %>% 
      mutate(year_sowing=year(as.Date(Date_sowing, format = "%d/%m/%Y")),
             Origin=as.Date(paste0(year_sowing-1,"-12-31"))) %>% 
      mutate(Date=as.Date(Date, format = "%d/%m/%Y")) 
  } else {
    template_df_ext <- template_df %>% 
      mutate(year_sowing=year(as.Date(SowingDate, format = "%d/%m/%Y")),
             Origin=as.Date(paste0(year_sowing-1,"-12-31"))) %>% 
      mutate(Date=as.Date(Date, format = "%d/%m/%Y")) 
  } 
  
  ## Create a mask for extracting required values from simulations
  mask <- obs_list
  for (sit in setdiff(names(sitNames_corresp),names(obs_list))) {
    if ("Harvest_Date" %in% names(sim_final_converted$sim_list[[sit]])) {
      harvest_jul <- tail(sim_final_converted$sim_list[[sit]]$Harvest_Date,n=1)
    } else {
      harvest_jul <- tail(sim_final_converted$sim_list[[sit]]$HarvestDate,n=1)
    }
    harvest_Date <- as.Date(as.numeric(harvest_jul),
                            origin=paste0(filter(template_df_ext,Number==as.numeric(sit))["year_sowing"]-1,"-12-31"),
                            format="%Y-%m-%d")[[1]]
    mask[[sit]] <- data.frame(Date=harvest_Date, 
                              t(setNames(rep(0,length(obsVar_names)), nm=obsVar_names)))
  }
  mask <- lapply(mask, function(x) {x[,obsVar_names] <- 0; x})
  
  ## Intersect mask and simulated values
  obs_sim_list <- CroptimizR:::make_obsSim_consistent(sim_final_converted$sim_list,  
                                                      mask)
  res <- CroptimizR:::intersect_sim_obs(sim_list = obs_sim_list$sim_list,
                                        obs_list = obs_sim_list$obs_list)
  res_df <- CroPlotR::bind_rows(res$sim_list)
  res_df <- mutate(res_df, Number=as.integer(situation)) %>% select(-situation)
  
  ## Add information from the template file
  template_df_ext <- slice(template_df_ext, which(!duplicated(template_df_ext$Number)))
  res_df <- left_join(res_df, select(template_df_ext, Number,
                                     setdiff(names(template_df_ext), names(res_df))), 
                      by="Number") 
  ## Convert julian days in Dates
  if ("Date_sowing" %in% names(res_df)) {
    res_df <- res_df %>% 
      mutate(year_sowing=year(as.Date(Date_sowing, format = "%d/%m/%Y")),
             Origin=as.Date(paste0(year_sowing-1,"-12-31"))) %>% 
      rowwise() %>% mutate(across(all_of(var_date), ~ case_when(.==0 ~ as.character(NA), 
                                                                .!=0 ~ format(as.Date(.x, 
                                                                                      origin=Origin),"%d/%m/%Y")))) %>%
      select(-Origin, -year_sowing) %>% relocate(names(template_df))
  } else {
    res_df <- res_df %>% 
      mutate(year_sowing=year(as.Date(SowingDate, format = "%d/%m/%Y")),
             Origin=as.Date(paste0(year_sowing-1,"-12-31"))) %>% 
      rowwise() %>% mutate(across(all_of(var_date), ~ case_when(.==0 ~ as.character(NA), 
                                                                .!=0 ~ format(as.Date(.x, 
                                                                                      origin=Origin),"%d/%m/%Y")))) %>%
      select(-Origin, -year_sowing) %>% relocate(names(template_df))
  }
      
  ################################################################################
  # At the beginning ask model name and contact person to set the name fo the result file
  ################################################################################
  
  suffix <- NULL
  if (test_case=="French") suffix <- paste0("_",variety) 
  write.table(res_df,file = file.path(out_dir,paste0("cal_4_results_", test_case, suffix, "numerical_XXX.txt")),
              row.names = FALSE, quote=FALSE)
  
  # Generate a report with the list of parameters, ...
  
  
  
}