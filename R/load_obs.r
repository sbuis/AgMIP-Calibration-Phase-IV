load_obs <- function(obs_data_path, obs_unit_path, varNames_corresp, 
                     sitNames_corresp, simVar_units, obsVar_group, 
                     flag_eos, ref_date) {
  
  obs_df <- read.table(file=obs_data_path, header = TRUE, stringsAsFactors = FALSE)
  obs_df <- select_if(obs_df, ~!all(is.na(.))) # remove columns with just NAs
  obs_units <- read.table(file=obs_unit_path, header = TRUE, stringsAsFactors = FALSE, 
                          sep=";", strip.white=TRUE)
  
  harvest_year <- setNames(obs_df$HarvestYear[!duplicated(obs_df$Number)], 
                          obs_df$Number[!duplicated(obs_df$Number)])
  
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
  
  obsVar_used <- intersect(intersect(obsVar_names, names(varNames_corresp)), 
                            names(obsVar_group))
  # TODO: Remove intersection with names(obsVar_group) if HarvestDate is removed to the observations.
  
  # Transform observed dates of phenological stages in julian days from the reference date given by the user
  var_date <- obsVar_used[grepl("Date",obsVar_used)]
  if ("Date_sowing" %in% names(obs_df)) {
    obs_df <- rename(obs_df, Date_sowing="SowingDate")
  }
  
  obs_df <- obs_df %>% 
    mutate(Origin=ref_date[as.character(obs_df$Number)]) %>% 
    rowwise() %>% mutate(across(all_of(c("SowingDate",var_date)), 
                                ~ julian(as.Date(.x, 
                                                 format = "%d/%m/%Y"),
                                         origin=Origin)))
  
  sowing_jul_obs <- setNames(obs_df$SowingDate[!duplicated(obs_df$Number)], 
                             obs_df$Number[!duplicated(obs_df$Number)])
  
  # Add units
  for (var in obsVar_used) {
    units(obs_df[[var]]) <- obsVar_units[var]
  }
  
  # Add a column Situation including the situation names
  obs_df$Situation <- as.character(obs_df$Number)
  
  # Transform Date column from character to Date format
  # BE CAREFUL: HARD-CODED FORMAT (format defined in obs_units should be used)
  # obs_df$Date <- as.Date(obs_df$Date, format = "%d/%m/%Y")
  obs_df$Date <- as.POSIXct(obs_df$Date, format = "%d/%m/%Y", tz = "UTC")
  
  # Only keep the observed variables that will be used 
  obs_df <- select(obs_df, c("Situation", "Date", all_of(obsVar_used)))

  # Transform into CroptimizR format
  obs_list <- split(obs_df, obs_df$Situation)
  obs_list <- lapply(obs_list, function(x) select(x,-Situation))
  
  # In case flag_eos is activated, replace last observed date by 31/12/harvest_year 
  if (flag_eos) { 
    for (sit in names(obs_list)) {
      eos_Date <- as.Date(paste0(harvest_year[[sit]],"-12-31"), format="%Y-%m-%d")[[1]]
      # except for "Lake_2010_***" since there's not enough weather data ...
      if (sit %in% names(sitNames_corresp[grep("Lake-2010",sitNames_corresp)])) {
        eos_Date <- as.Date("2011-01-30", format="%Y-%m-%d")[[1]]
      }
      obs_list[[sit]][nrow(obs_list[[sit]]),"Date"] <- eos_Date
    } 
  }
  
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
              obsVar_used=obsVar_used, sowing_jul_obs=sowing_jul_obs,
              harvest_year=harvest_year))

}