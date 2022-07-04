generate_results_files <- function(group, model_options, 
                                   complem_info, res_it2, res_it3,
                                   sitNames_corresp, sim_final, obs, 
                                   template_path, out_dir, test_case, variety,
                                   varNames_corresp) {

  # Table5 
  group_name <- names(group)
  
  dirs <- list.dirs(file.path(out_dir,"Iteration1"))
  files <- list.files(path=dirs, pattern="optim_results.Rdata", full.names = TRUE)          
  files <- lapply(group_name, function(x) {
    if (length(grep(x,files))>1) {
      return(files[grep(x,files)[-1]])
    } else {
      return(files[grep(x,files)])
    }
  })
  names(files) <- group_name
  
  Table5 <- NULL        
  for (gr in group_name) {
    
    Table5 <- bind_rows(Table5, lapply(files[[gr]], function(x) {
      load(x)
      setNames(
        tibble(gr,
               list(names(res$final_values)),
               nrow(res$init_values),
               res$min_crit_value,
               res$BIC,
               nrow(res$params_and_crit)),
        nm=c("group","name of the parameters","Number of starting values",
             "Final SS","Final BIC", "Total number of calls to model")
      )
    }))
    
  }
  save_table(table=Table5, table_name="Table5", path=out_dir)
  
  
  # Table6
  model_error_sd <- complem_info$it2$weight
  for (var in obs$obsVar_used) {  # convert from model units to obs units
    varSim <- varNames_corresp[[var]]
    units(model_error_sd[[varSim]]) <- simVar_units[[varSim]]
    units(model_error_sd[[varSim]]) <- obs$obsVar_units[[var]]
  }
  Table6 <- data.frame(Variables=obs$obsVar_used, `Estimated sd of model error`=as.numeric(model_error_sd))
  save_table(table=Table6, table_name="Table6", path=out_dir)
  
  # Table7
  Table7 <- generate_table7_like(res_it2, group)
  save_table(table=Table7, table_name="Table7", path=out_dir)
  
  # Table8
  model_error_sd <- complem_info$it3$weight
  for (var in obs$obsVar_used) {  # convert from model units to obs units
    varSim <- varNames_corresp[[var]]
    units(model_error_sd[[varSim]]) <- simVar_units[[varSim]]
    units(model_error_sd[[varSim]]) <- obs$obsVar_units[[var]]
  }
  Table8 <- data.frame(Variables=obs$obsVar_used, `Estimated sd of model error`=as.numeric(model_error_sd))
  save_table(table=Table8, table_name="Table8", path=out_dir)
  
  # Table9
  Table9 <- generate_table7_like(res_it3, group)
  save_table(table=Table9, table_name="Table9", path=out_dir)
  
  # Table10
  sim_list_transformed <- apply_transform_var(sim_final$sim_list, transform_var)
  rmse_final <- setNames(
    summary(sim_list_transformed, obs=obs$converted_obs_list, stats = c("RMSE"))$RMSE,
    nm=summary(sim_list_transformed, obs=obs$converted_obs_list, stats = c("RMSE"))$variable)
  rmse_final <- tibble::tibble(!!!rmse_final)
  for (var in obs$obsVar_used) {  # convert from model units to obs units
    varSim <- varNames_corresp[[var]]
    units(rmse_final[[varSim]]) <- simVar_units[[varSim]]
    units(rmse_final[[varSim]]) <- obs$obsVar_units[[var]]
  }
  Table10 <- data.frame(Variables=obs$obsVar_used, `Estimated sd of model error`=as.numeric(rmse_final))
  save_table(table=Table10, table_name="Table10", path=out_dir)
  
  
  # Generate cal_4_results_* files
  generate_cal_results(sim_final, obs, sitNames_corresp, 
                       template_path, out_dir, test_case, variety, varNames_corresp)
  
}


generate_cal_results <- function(sim_final, obs, sitNames_corresp, 
                                 template_path, out_dir, test_case, variety,
                                 varNames_corresp) {
  
  obs_list <- obs$obs_list
  obsVar_names <- obs$obsVar_names
  obsVar_units <- obs$obsVar_units
  obsVar_used <- obs$obsVar_used
  var_date <- names(varNames_corresp)[grepl("Date",names(varNames_corresp))]
  
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
  
  suffix <- NULL
  if (test_case=="French") suffix <- paste0("_",variety) 
  write.table(res_df,file = file.path(out_dir,paste0("cal_4_results_", test_case, suffix, "numerical_XXX.txt")),
              row.names = FALSE, quote=FALSE)
  
}



generate_table7_like <- function(res, group) {
  param_group <- setNames(object=sapply(strsplit(names(unlist(group)), split="[.]"), `[`)[1,], 
                          nm=unlist(group))
  table <- bind_rows(lapply(names(res$final_values), function(param) {
    setNames(
      tibble(param_group[param][[1]],
             param,
             res$init_values[res$ind_min_crit, param],
             res$final_values[param]),
      nm=c("group","name of the parameters","Initial parameter value",
           "Final parameter value")
    )
  }))
  
  return(table)
}


save_table <- function (table, table_name, path) {
  
  tb <- purrr::modify_if(
    table,
    function(x) !is.list(x), as.list
  )
  # format everything in char and 2 digits
  tb <- purrr::modify(
    tb,
    function(x) {
      unlist(
        purrr::modify(x, function(y) {
          paste(format(y,
                       scientific = FALSE,
                       digits = 2, nsmall = 2
          ), collapse = ", ")
        })
      )
    }
  )
  
  utils::write.table(tb,
                     sep = ";", file = file.path(
                       path,
                       paste0(table_name,".csv")
                     ),
                     row.names = FALSE
  )
  
}