check_user_inputs <- function(simVar_units, varNames_corresp, transform_outputs) {

  if (!all(sort(names(simVar_units))==sort(varNames_corresp)))
    stop(paste0("Incorrect definition of simVar_units or varNames_corresp. ",
                "Please check that they include the same variables.\n",
                paste(setdiff(varNames_corresp, names(simVar_units)), collapse = ","), 
                " included in varNames_corresp but not in simVar_units.\n",
                paste(setdiff(names(simVar_units), varNames_corresp), collapse = ","), 
                " included in simVar_units but not in varNames_corresp.\n"))
  
  if (!all(transform_outputs %in% varNames_corresp)) 
    stop(paste0("Incorrect definition of transform_outputs or varNames_corresp. ",
                "Please check that all variables included in transform_outputs are also included in varNames_corresp.\n",
                paste(setdiff(transform_outputs, varNames_corresp), collapse = ","), 
                " included in transform_outputs but not in varNames_corresp.\n"))
  
}

