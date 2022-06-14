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
  if(!require("CroptimizR")){
    devtools::install_github("SticsRPacks/CroptimizR@*release")
    library("CroptimizR")
  }
  if(!require("CroPlotR")){
    devtools::install_github("SticsRPacks/CroPlotR@*release")
    library("CroPlotR")
  }
  
  source(file.path(here(),"R/check_user_inputs.r"))
  source(file.path(here(),"R/conversion.r"))
  source(file.path(here(),"R/load_obs.r"))
  source(file.path(here(),"R/post_treat.r"))
  source(file.path(here(),"R/run_wrapper.r"))
  
}