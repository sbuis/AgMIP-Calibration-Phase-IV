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
  if(!require("CroptimizR")){
    devtools::install_github("SticsRPacks/CroptimizR@AgMIP_phaseIV")
    library("CroptimizR")
  }
  if(!require("CroPlotR")){
    devtools::install_github("SticsRPacks/CroPlotR@4018d25")
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