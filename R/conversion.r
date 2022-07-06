rename_sit <- function(simobs, corresp, invert=FALSE) {
  # Change simulated or observation situation names to new ones, as defined in 
  # corresp (=c(newName1="oldName1", ...))
  # if corresp (=c(oldName1="newName1", ...)), then set invert to TRUE
  
  if (invert) {
    tmp <- names(corresp)
    names(tmp) <- corresp
    corresp <- tmp
  }
  
  idx <- names(simobs) %in% corresp
  names(simobs)[idx] <-
    names(corresp[match(names(simobs[idx]),corresp)])
  return(simobs)
}

rename_var <- function(simobs, corresp, invert=FALSE) {
  # Change simulated or observation variables names to new ones, as defined in 
  # corresp (=c(newName1="oldName1", ...))
  # if corresp (=c(oldName1="newName1", ...)), then set invert to TRUE
  corresp <- corresp[names(corresp)!=corresp]
  if (length(corresp)==0) return(simobs)
  
  if (invert) {
    tmp <- names(corresp)
    names(tmp) <- corresp
    corresp <- tmp
  }
  for (sit in names(simobs)) {
    to_remove <- NULL
    for (new_varName in names(corresp)) {
      if (corresp[new_varName] %in% names(simobs[[sit]])) {
        simobs[[sit]][,new_varName] <- simobs[[sit]][,corresp[new_varName]]
        to_remove <- c(to_remove,corresp[new_varName])
      }
    }
    simobs[[sit]] <- select(simobs[[sit]], -all_of(to_remove))
    # old_varNames <- intersect(names(simobs[[sit]]), corresp)
    # if (length(old_varNames)>0) {
    #   new_varNames <- names(corresp)[match(old_varNames,corresp)]
    #   simobs[[sit]][,new_varNames] <- simobs[[sit]][,old_varNames] 
    # }
  }
  return(simobs)
}

set_units <- function(simobs, var_units) {
  # Set variables units as defined in var_units (=c(varName1="unit", ...))
  for (sit in names(simobs)) {
    for (var in intersect(names(simobs[[sit]]),names(var_units))) {
      units(simobs[[sit]][[var]]) <- var_units[[var]]
    }
  }
  return(simobs)
}

convert_units <- function(simobs, var_units) {
  # Convert simulated or observed variables units in new units as given by 
  # var_units=c(varName1="unit", ...)
  for (sit in names(simobs)) {
    for (var in intersect(names(simobs[[sit]]),names(var_units))) {
      units(simobs[[sit]][[var]]) <- var_units[[var]]
    }
  }
  return(simobs)
}

