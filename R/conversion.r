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
    if (any(names(corresp) %in% colnames(simobs[[sit]]))) {
      stop(paste("Error converting simulated variables names in observed variables names: variable(s)",
                 paste(intersect(names(corresp),colnames(simobs[[sit]])), collapse = ","),
                 "already there in simulated variables names. Please check corresp."))
    }
    idx <- colnames(simobs[[sit]]) %in% corresp
    colnames(simobs[[sit]])[idx] <- 
      names(corresp[match(colnames(simobs[[sit]])[idx],corresp)])
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

