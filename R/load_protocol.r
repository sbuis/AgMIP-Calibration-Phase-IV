load_protocol <- function(protocol_path, transform_outputs, use_obs_synth=FALSE, beta) {
  # Load the protocol description as given in xls file `protocol_path`
  # Returns:
  #   - sitNames_corresp: correspondance between situation Numbers and Names
  #   - varNames_corresp: correspondance between simulated and observed variables names
  #   - simVar_units: units of simulated variables
  #   - param_info: bounds of parameters to estimate
  #   - default_param_values: default values of parameters to estimate and parameters to fix to compute
  #   - param_group: list of major and candidate parameters to estimate
  
  check_protocol_structure(protocol_path)
  
  sheets <- excel_sheets(protocol_path)

  # Correspondance between situation Number and name
  sitNames_corresp_df <- read_excel(protocol_path, sheet = grep(tolower("situation names"),sheets))
  sitNames_corresp <- setNames(object = sitNames_corresp_df$`Situation Name`, 
                               nm = sitNames_corresp_df$Number)
  
  # Correspondance between names of observed or required and simulated variables
  variables_df <- read_excel(protocol_path, sheet = grep(tolower("variables"),sheets)) 
  varNames_corresp_df <- variables_df %>%
    filter(`Name of the simulated variable`!="NA",`Name of the simulated variable`!="na")
  varNames_corresp <- setNames(object = varNames_corresp_df$`Name of the simulated variable`, 
                               nm = varNames_corresp_df$`Name of the observed or required variable`)

  # Group of the observed variables
  tmp <- variables_df %>% filter(`Group for calibration` != "NA")
  obsVar_group <- setNames(object = tmp$`Group for calibration`, 
                        nm = tmp$`Name of the observed or required variable`)
  
  # Unit of simulated variables
  simVar_units <- setNames(object = varNames_corresp_df$`Unit of the simulated variable`, 
                           nm = varNames_corresp_df$`Name of the simulated variable`)
  
  # Building param_info 
  major_params_df <- read_excel(protocol_path, 
                                   sheet = grep(tolower("major parameters"),sheets))
  if (!is.numeric(major_params_df$`default value`)) {
    stop(paste("Default values of major parameters must be numeric. Please check file",protocol_path))
  }
  if (!is.numeric(major_params_df$`lower bound`)) {
    stop(paste("Lower bounds of major parameters must be numeric. Please check file",protocol_path))
  }
  if (!is.numeric(major_params_df$`upper bound`)) {
    stop(paste("Upper bounds of major parameters must be numeric. Please check file",protocol_path))
  }
  
  candidate_params_df <- read_excel(protocol_path, 
                                    sheet = grep(tolower("candidate parameters"),sheets))
  if (!is.numeric(candidate_params_df$`default value`)) {
    stop(paste("Default values of candidate parameters must be numeric. Please check file",protocol_path))
  }
  if (!is.numeric(candidate_params_df$`lower bound`)) {
    stop(paste("Lower bounds of candidate parameters must be numeric. Please check file",protocol_path))
  }
  if (!is.numeric(candidate_params_df$`upper bound`)) {
    stop(paste("Upper bounds of candidate parameters must be numeric. Please check file",protocol_path))
  }
  if (any(grepl(tolower("parameters to fix or calculate"),sheets))) {
    constraints_df <- read_excel(protocol_path, 
                                 sheet = grep(tolower("parameters to fix or calculate"),sheets))
  } else {
    constraints_df <- NULL
  }
  
  # Check default values and bounds
  if (any(major_params_df$`default value` < major_params_df$`lower bound` | 
          major_params_df$`default value` > major_params_df$`upper bound`)) { 
    stop(paste("Default value of parameter(s))",
               paste(major_params_df$`name of the parameter`[major_params_df$`default value` < major_params_df$`lower bound` | major_params_df$`default value` > major_params_df$`upper bound`], 
                     collapse = ","),
               "out of bounds. Please check default values and bounds of major parameters in file",protocol_path))
  }
  if (any(candidate_params_df$`default value` < candidate_params_df$`lower bound` | 
          candidate_params_df$`default value` > candidate_params_df$`upper bound`)) { 
    stop(paste("Default value of parameter(s))",
               paste(candidate_params_df$`name of the parameter`[candidate_params_df$`default value` < candidate_params_df$`lower bound` | candidate_params_df$`default value` > candidate_params_df$`upper bound`], 
                     collapse = ","),
               "out of bounds. Please check default values and bounds of candidate parameters in file",protocol_path))
  }
  if (any(major_params_df$`lower bound` >= major_params_df$`upper bound`)) { 
    stop(paste("Bounds of parameter(s))",
               paste(major_params_df$`name of the parameter`[major_params_df$`lower bound` >= major_params_df$`upper bound`], 
                     collapse = ","),
               "are not well defined (lower bound >= upper bound. Please check bounds of major parameters in file",protocol_path))
  }
  if (any(candidate_params_df$`lower bound` >= candidate_params_df$`upper bound`)) { 
    stop(paste("Bounds of parameter(s))",
               paste(candidate_params_df$`name of the parameter`[candidate_params_df$`lower bound` >= candidate_params_df$`upper bound`], 
                     collapse = ","),
               "are not well defined (lower bound >= upper bound. Please check bounds of candidate parameters in file",protocol_path))
  }
  

  # Generate new default values in case of synthetic experiments
  true_param_values <- NA
  if (use_obs_synth) {

    # Building true_param_values 
    true_param_values <- c(
      as.list(setNames(object = major_params_df$`default value`,
                       nm = major_params_df$`name of the parameter`)),
      as.list(setNames(object = candidate_params_df$`default value`,
                       nm = candidate_params_df$`name of the parameter`))
    )
    true_param_values <- c(
      true_param_values,
      as.list(setNames(object = constraints_df$`value or formula`,
                       nm = constraints_df$`name of the parameter`))
    )
  
    major_params_df$`default value` <- perturb_param(major_params_df, beta)
      
    candidate_params_df$`default value` <- perturb_param(candidate_params_df, beta)
    
    write.csv2(major_params_df, 
               file=file.path(out_dir,paste0("synth_almost_additive_parameters",
                                             "_beta",beta,".csv")), 
               row.names = FALSE)
    
    write.csv2(candidate_params_df, 
               file=file.path(out_dir,paste0("synth_candidate_parameters",
                                            "_beta",beta,".csv")), 
               row.names = FALSE)     
    
  }
  
  
  param_info <- list(lb=setNames(object = c(major_params_df$`lower bound`,
                                            candidate_params_df$`lower bound`),
                                 nm = c(major_params_df$`name of the parameter`,
                                        candidate_params_df$`name of the parameter`)),
                     ub=setNames(object = c(major_params_df$`upper bound`, 
                                            candidate_params_df$`upper bound`),
                                 nm = c(major_params_df$`name of the parameter`,
                                        candidate_params_df$`name of the parameter`)),
                     init_values=setNames(object = c(major_params_df$`default value`, 
                                                     candidate_params_df$`default value`),
                                          nm = c(major_params_df$`name of the parameter`,
                                                 candidate_params_df$`name of the parameter`)))
  
  # Building default_param_values 
  default_param_values <- c(
    as.list(setNames(object = major_params_df$`default value`,
                     nm = major_params_df$`name of the parameter`)),
    as.list(setNames(object = candidate_params_df$`default value`,
                                  nm = candidate_params_df$`name of the parameter`))
  )
  default_param_values <- c(
    default_param_values,
    as.list(setNames(object = constraints_df$`value or formula`,
                     nm = constraints_df$`name of the parameter`))
  )
  
  # Parameters per group of observed variables
  param_group <- lapply(unique(major_params_df$group),function(x) {
    res <- list(obligatory=filter(major_params_df, group==x)$`name of the parameter`,
                candidates=filter(candidate_params_df, group==x)$`name of the parameter`)
    if (length(res$candidates)==0) 
      res$candidates <- NULL
    return(res)})
  names(param_group) <- unique(major_params_df$group)
  param_group <- param_group[intersect(unique(obsVar_group), names(param_group))]   # use ordering of the groups as defined in tab Variables
    
  # Check protocol content
  check_protocol_content(protocol_path, variables_df, varNames_corresp,
                         simVar_units, transform_outputs, param_group,
                         obsVar_group, sitNames_corresp)
  
  return(list(sitNames_corresp=sitNames_corresp, 
              varNames_corresp=varNames_corresp, 
              simVar_units=simVar_units, 
              param_info=param_info, 
              default_param_values=default_param_values, 
              param_group=param_group, obsVar_group=obsVar_group, 
              true_param_values=true_param_values))
}

check_protocol_structure <- function(protocol_path) {
  # Check that the protocol description as given in xls file `protocol_path`
  # include the required sheets and columns
  
  sheets <- excel_sheets(protocol_path)
  expected_sheets <- c("variables", "major parameters", "candidate parameters", 
                       "parameters to fix or calculate", "situation names")
  if (!all(sapply(expected_sheets, function(x) {tolower(x) %in% tolower(expected_sheets)})))
    stop(paste0("Sheet(s) \"",paste(setdiff(tolower(expected_sheets), tolower(sheets)),collapse = "\", \""),
                "\" not found in ",protocol_path,"\nPlease add it (them)."))
  
  expected_cols_ls <- list(
    `variables`=c("Name of the observed or required variable", "Name of the simulated variable"),
    `major parameters`=c("name of the parameter", "group", "default value", "lower bound", "upper bound"),
    `candidate parameters`=c("name of the parameter", "group", "default value", "lower bound", "upper bound"),
    `parameters to fix or calculate`=c("name of the parameter", "value or formula"),
    `situation names`=c("Number", "Situation Name")
  )
  invisible(
    lapply(names(expected_cols_ls), function(x) {
      df <- read_excel(protocol_path, sheet = grep(tolower(x),sheets))
      check_col_names(protocol_path, expected_cols_ls[[x]], names(df), x)
    }
    )
  )
  
}

check_col_names <- function(protocol_path, expected_cols, cols, sheet) {
  # check that columns listed in `expected_cols` are included in columns listed in `cols`
  # for sheet `sheet`
  if (!all(sapply(expected_cols, function(x) {x %in% cols})))
    stop(paste0("Column(s) \"",paste(setdiff(expected_cols, cols),collapse = "\", \""),
                "\" not found in sheet \"",sheet,"\" of file ",
                protocol_path,"\nPlease add it (them)."))
  
}

check_protocol_content <- function(protocol_path, variables_df, varNames_corresp,
                                   simVar_units, transform_outputs, param_group,
                                   obsVar_group, sitNames_corresp) {
  # Check the content of the protocol description as given in xls file `protocol_path`

  # Check situation names were provided
  if (any(is.na(sitNames_corresp))) {
    stop(paste("Missing (one or several) situation name(s). Please check tab sheet \"Situation names\" in file",protocol_path,
               "\nA situation name your model_wrapper is able to handle, i.e. is able to run the corresponding situation from its name, must be given for each situation number."))
  }
  
  # Check that there is a correspondence for Date_BBCH90 (mandatory since the simulated 
  # variables such as Yield must be provided at Date_BBCH90)
  if ( !("Date_BBCH90" %in% variables_df$`Name of the observed or required variable`) )
    stop(paste("Date_BBCH90 must be in the list of observed or required variable in sheet \"variables\" of file",
               protocol_path))
  if (is.na(filter(variables_df, `Name of the observed or required variable`=="Date_BBCH90") %>% 
            select(`Name of the simulated variable`))) 
    stop(paste("The name of a simulated variable must be provided for observed variable Date_BBCH90 in sheet \"variables\" of file",
               protocol_path))
  
  # Check that there is at least one "major param" when there are candidates
  invisible(lapply(names(param_group), function(x) {
    if(is.null(param_group[[x]]$obligatory)) 
      stop(paste("\"major parameters\" must be defined for group",x,"\n Please correct file",protocol_path))
    }
  ))  
  
  # Check that there is observations for the groups defined in the parameters (major)
  invisible(lapply(names(param_group), function(x) {
    if ( !(x %in% unique(obsVar_group[names(varNames_corresp)])) ) 
      stop(paste("\"major parameters\" are defined for group",x,
                 "but there is either no observed or simulated variables defined for this group in the \"variables\" sheet.\n Please correct file",protocol_path))
  }
  ))  

  if (!all(sort(names(simVar_units))==sort(varNames_corresp)))
    stop(paste0("Incorrect definition of simVar_units or varNames_corresp. ",
                "Please check that they include the same variables.\n",
                paste(setdiff(varNames_corresp, names(simVar_units)), collapse = ","), 
                " included in varNames_corresp but not in simVar_units.\n",
                paste(setdiff(names(simVar_units), varNames_corresp), collapse = ","), 
                " included in simVar_units but not in varNames_corresp.\n"))
  
  if (!all(transform_outputs %in% varNames_corresp)) 
    stop(paste0("Incorrect definition of transform_outputs or of the list of simulated variables defined in the \"variables\" sheet of the protocol description xls file ",
                "Please check that all variables included in transform_outputs are also included in this sheet.\n",
                paste(setdiff(transform_outputs, varNames_corresp), collapse = ","), 
                " included in transform_outputs but not in the xls sheet.\n"))
  
}


perturb_param <- function(params_df, beta) {
# Generates new values for `default values` from perturbation of original values
# using x <- x + alpha * beta * (distance between x and bound in direction alpha)
# where alpha is randomly chosen in {-1, 1}
# params_df is a df including columns `default value`, `lower bound` and `upper bound`
# Return the vector of new generated values
  
  params_df <- 
    params_df %>%
    mutate(
      `alpha` = case_when(
        `default value` == `lower bound` ~ 1,
        `default value` == `upper bound` ~ -1, 
        `default value` > `lower bound` & `default value` < `upper bound` ~ 
          sample(x=c(-1,1), size=nrow(params_df), replace=TRUE)
      )
    ) %>%
    mutate(
      `default value` = `default value` + `alpha` * beta * 
        case_when(`alpha` == -1 ~ `default value` - `lower bound`, 
                  `alpha` == 1 ~ `upper bound` - `default value`)
    )
  
  # rows_to_change <- match(unique(candidate_params_df$group),
  #                         candidate_params_df$group)

  return(params_df$`default value`)
  
}
