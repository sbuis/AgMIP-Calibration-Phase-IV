load_obs <- function(obs_data_path, obs_unit_path, varNames_corresp, 
                     sitNames_corresp, simVar_units, obsVar_group) {
  
  obs_df <- read.table(file=obs_data_path, header = TRUE, stringsAsFactors = FALSE)
  obs_units <- read.table(file=obs_unit_path, header = TRUE, stringsAsFactors = FALSE, 
                          sep=";", strip.white=TRUE)
  
  # obsVar_names <- obs_units$Column.name
  obsVar_names <- setdiff(names(obs_df),c("Number","Site","HarvestYear","SowingDate", "Variety","Date"))
  obsVar_units <- setNames(obs_units$Unit, nm = obs_units$Column.name)
  if ("Date_*" %in% names(obsVar_units)) {
    obsVar_units[obsVar_names[grep("Date_",obsVar_names)]] <- obsVar_units["Date_*"]
    obsVar_units <- obsVar_units[names(obsVar_units)!="Date_*"]
  }
  # Check that all observed variables defined in the obs_data file have a unit defined in the obs_unit file
  if ( !all(obsVar_names %in% names(obsVar_units)) ) {
    stop(paste("Variable(s)",paste(setdiff(obsVar_names,names(obsVar_units)),collapse = ","),"included in the observation data file",obs_data_path,
    "is (are) not defined in the observation unit file",
    obs_unit_path))
  }
  ## Change the def. of their units
  obsVar_units[grep("Date",names(obsVar_units))] <- "d"
  
  
  obsVar_groups <- tolower(obs_units$Group[!is.na(obs_units$Group)])
  obsVar_used <- intersect(intersect(obsVar_names, names(varNames_corresp)), 
                            names(obsVar_group))
  
  # Check coherency between varNames_corresp and obs names
  if (!all(names(varNames_corresp) %in% obsVar_names)) 
    stop(paste0("Incorrect names for the observed variables. ",
                "Please check that the names of observed variables defined in the \"variables\" sheet of the protocol description xls file are identical to these listed in the observation units file:",
                obs_unit_path,"\n",
                paste(setdiff(names(varNames_corresp), obsVar_names), collapse = ","), 
                " are listed in the protocol description xls file but not in the observation unit file.\n",
                "List of variables included in the observation unit file: ",
                paste(obsVar_names, collapse = ",")))
  
  # Transform observed dates of phenological stages in julian days from 31/12/(sowing year-1)
  ###################################################################################
  ## WARNING: that should be adapted depending on the model (ref. could be different)
  ###################################################################################
  var_date <- obsVar_used[grepl("Date",obsVar_used)]
  if ("Date_sowing" %in% names(obs_df)) {
    obs_df <- obs_df %>% 
      mutate(year_sowing=year(as.Date(Date_sowing, format = "%d/%m/%Y")),
             Origin=as.Date(paste0(year_sowing-1,"-12-31"))) %>% 
      rowwise() %>% mutate(across(all_of(var_date), 
                                  ~ julian(as.Date(.x, 
                                                   format = "%d/%m/%Y"),
                                           origin=Origin)))
  } else {
    obs_df <- obs_df %>% 
      mutate(year_sowing=year(as.Date(SowingDate, format = "%d/%m/%Y")),
             Origin=as.Date(paste0(year_sowing-1,"-12-31"))) %>% 
      rowwise() %>% mutate(across(all_of(var_date), 
                                  ~ julian(as.Date(.x, 
                                                   format = "%d/%m/%Y"),
                                           origin=Origin)))
  }
  
  # Add units
  for (var in intersect(names(obs_df), obsVar_used)) {
    units(obs_df[[var]]) <- obsVar_units[var]
  }
  
  # Add a column Situation including the situation names
  obs_df$Situation <- as.character(obs_df$Number)
  
  # Transform Date column from character to Date format
  # BE CAREFUL: HARD-CODED FORMAT (format defined in obs_units should be used)
  # obs_df$Date <- as.Date(obs_df$Date, format = "%d/%m/%Y")
  obs_df$Date <- as.POSIXct(obs_df$Date, format = "%d/%m/%Y", tz = "UTC")
  
  # Check coherency between obs data file and obs units file 
  if (!all(obsVar_names %in% names(obs_df)))
    stop(paste0("Obs data file and obs units file are not coherent. ",
                "Please check that all variables included in obs units file are also included in the obs data file.\n",
                paste(setdiff(obsVar_names, names(obs_df)), collapse = ","), 
                " included in obs units file ",obs_unit_path," but not in obs data file ",
                obs_data_path,".\n"))
  
  # Only keep the observed variables that will be used 
  obs_df <- select(obs_df, c("Situation", "Date", all_of(obsVar_used)))
  # or select all observed variables ?
  # obs_df <- select(obs_df, c("Situation", "Date", all_of(obsVar_names)))
  
  # Transform into CroptimizR format
  obs_list <- split(obs_df, obs_df$Situation)
  obs_list <- lapply(obs_list, function(x) select(x,-Situation))
  
  # Convert obs_list to simulated names and units
  converted_obs_list <- obs_list
  if (!is.null(sitNames_corresp))
    converted_obs_list <- rename_sit(obs_list, sitNames_corresp, invert=TRUE)
  converted_obs_list <- rename_var(converted_obs_list, varNames_corresp, invert=TRUE)
  converted_obs_list <- convert_units(converted_obs_list, simVar_units)
  
  # Remove units from obs lists
  converted_obs_list <- lapply(converted_obs_list,drop_units)
  obs_list  <- lapply(obs_list ,drop_units)
  
  return(list(obs_list=obs_list, converted_obs_list=converted_obs_list, 
              obsVar_names=obsVar_names, obsVar_units=obsVar_units, 
              obsVar_groups=obsVar_groups, obsVar_used=obsVar_used))

}