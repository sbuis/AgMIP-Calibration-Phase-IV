load_protocol <- function(protocol_path) {
  # Load the protocol description as given in xls file `protocol_path`
  # Returns:
  #   - sitNames_corresp: correspondance between situation Numbers and Names
  #   - varNames_corresp: correspondance between simulated and observed variables names
  #   - simVar_units: units of simulated variables
  #   - param_info: bounds of parameters to estimate
  #   - forced_param_values: default values of parameters to estimate and equality constraints to compute
  #   - group: list of almost additive and candidate parameters to estimate
  
  check_protocol(protocol_path)
  
  sheets <- excel_sheets(xls_path)

  sitNames_corresp_df <- read_excel(xls_path, sheet = grep(tolower("situation names"),sheets))
  sitNames_corresp <- setNames(object = sitNames_corresp_df$`Situation Name`, 
                               nm = sitNames_corresp_df$Number)
  
  varNames_corresp_df <- read_excel(xls_path, sheet = grep(tolower("variables"),sheets)) %>%
    filter(`Name of the simulated variable`!="NA",`Name of the simulated variable`!="na")
  varNames_corresp <- setNames(object = varNames_corresp_df$`Name of the simulated variable`, 
                               nm = varNames_corresp_df$`Name of the observed variable`)
  
  simVar_units <- setNames(object = varNames_corresp_df$`Unit of the simulated variable`, 
                           nm = varNames_corresp_df$`Name of the simulated variable`)
  
  additive_params_df <- read_excel(xls_path, 
                                   sheet = grep(tolower("almost additive parameters"),sheets))
  candidate_params_df <- read_excel(xls_path, 
                                    sheet = grep(tolower("candidate parameters"),sheets))
  
  param_info <- list(lb=setNames(object = c(additive_params_df$`lower bound`,
                                            candidate_params_df$`lower bound`),
                                 nm = c(additive_params_df$`name of the parameter`,
                                        candidate_params_df$`name of the parameter`)),
                     ub=setNames(object = c(additive_params_df$`upper bound`, 
                                            candidate_params_df$`upper bound`),
                                 nm = c(additive_params_df$`name of the parameter`,
                                        candidate_params_df$`name of the parameter`)),
                     init_values=setNames(object = c(additive_params_df$`default value`, 
                                                     candidate_params_df$`default value`),
                                          nm = c(additive_params_df$`name of the parameter`,
                                                 candidate_params_df$`name of the parameter`)))
  
  forced_param_values <- c(
    as.list(setNames(object = additive_params_df$`default value`,
                     nm = additive_params_df$`name of the parameter`)),
    as.list(setNames(object = candidate_params_df$`default value`,
                                  nm = candidate_params_df$`name of the parameter`))
  )
  if (any(grepl(tolower("equality constraints"),sheets))) {
    constraints_df <- read_excel(xls_path, 
                                 sheet = grep(tolower("equality constraints"),sheets))
    forced_param_values <- c(
      forced_param_values,
      as.list(setNames(object = constraints_df$`formula`,
                       nm = constraints_df$`name of the parameter`))
    )
  }
  
  # names(tmp) <- unique(constraints_df$group)
  # forced_param_values <- c(forced_param_values, tmp)
      
  split(data.table(select(additive_params_df,"name of the parameter","group")),
        by="group", keep.by=FALSE)
  
  group <- lapply(unique(additive_params_df$group),function(x) {
    res <- list(obligatory=filter(additive_params_df, group==x)$`name of the parameter`,
                candidates=filter(candidate_params_df, group==x)$`name of the parameter`)
    if (length(res$candidates)==0) 
      res$candidates <- NULL
    return(res)})
  names(group) <- unique(tolower(additive_params_df$group))
  
  return(list(sitNames_corresp=sitNames_corresp, 
              varNames_corresp=varNames_corresp, 
              simVar_units=simVar_units, 
              param_info=param_info, 
              forced_param_values=forced_param_values, 
              group=group))
}

check_protocol <- function(protocol_path) {
  # Check that the protocol description as given in xls file `protocol_path`
  # include the required sheets and columns
  
  sheets <- excel_sheets(xls_path)
  expected_sheets <- c("variables", "almost additive parameters", "candidate parameters", 
                       "equality constraints", "situation names")
  if (!all(sapply(sheets, function(x) {tolower(x) %in% tolower(expected_sheets)})))
    stop(paste0("Sheet(s) \"",paste(setdiff(tolower(expected_sheets), tolower(sheets)),collapse = "\", \""),
                "\" not found in ",xls_path,"\nPlease add it (them)."))
  
  expected_cols_ls <- list(
    `variables`=c("Name of the observed variable", "Name of the simulated variable"),
    `almost additive parameters`=c("name of the parameter", "group", "default value", "lower bound", "upper bound"),
    `candidate parameters`=c("name of the parameter", "group", "default value", "lower bound", "upper bound"),
    `equality constraints`=c("name of the parameter", "formula"),
    `situation names`=c("Number", "Situation Name")
  )
  invisible(
    lapply(names(expected_cols_ls), function(x) {
      df <- read_excel(xls_path, sheet = grep(tolower(x),sheets))
      check_col_names(expected_cols_ls[[x]], names(df), x)
    }
    )
  )
  
}

check_col_names <- function(expected_cols, cols, sheet) {
  # check that columns listed in `expected_cols` are included in columns listed in `cols`
  # for sheet `sheet`
  if (!all(sapply(expected_cols, function(x) {x %in% cols})))
    stop(paste0("Column(s) \"",paste(setdiff(expected_cols, cols),collapse = "\", \""),
                "\" not found in sheet \"",sheet,"\" of file ",
                xls_path,"\nPlease add it (them)."))
  
}
