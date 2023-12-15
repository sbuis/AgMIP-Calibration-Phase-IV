install_load <- function() {
  
  if(!require("dplyr")){
    install.packages("dplyr")
    library("dplyr")
  }
  if(!require("lubridate")){
    install.packages("lubridate")
    library("lubridate")
  }
  if(!require("units")){
    install.packages("units")
    library("units")
  }
  if(!require("data.table")){
    install.packages("data.table")
    library("data.table")
  }
  if(!require("readxl")){
    install.packages("readxl")
    library("readxl")
  }
  if(!require("CroptimizR")) {
    devtools::install_github("SticsRPacks/CroptimizR@*release")
    library("CroptimizR")
  } else if (!is_version_ok("CroptimizR", "0.6.0")) {
    detach("package:CroptimizR", unload = TRUE)
    devtools::install_github("SticsRPacks/CroptimizR@*release")
    library("CroptimizR")
  }
  if(!require("CroPlotR")) {
    devtools::install_github("SticsRPacks/CroPlotR@*release")
    library("CroPlotR")
  } else if (!is_version_ok("CroPlotR", "0.9.0")) {
    detach("package:CroPlotR", unload = TRUE)
    devtools::install_github("SticsRPacks/CroPlotR@*release")
    library("CroPlotR")
  }
  if(!require("rstudioapi")){
    install.packages("rstudioapi")
    library("rstudioapi")
  }
  if(!require("truncnorm")){
    install.packages("truncnorm")
    library("truncnorm")
  }
  
  
  invisible(lapply(list.files(file.path(here(),"R"), full.names=TRUE), function(x) source(x)))
  
}

is_version_ok <- function(pkg_name, min_version) {
  cur_version = packageVersion(pkg_name)
  if(cur_version < min_version) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}