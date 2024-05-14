args=commandArgs(trailingOnly=TRUE)
if (length(args>0)) {

  test_case <- as.character(args[1])
  variety <- as.character(args[2])
  workspace <- as.character(args[3])
  nb_cores <- as.integer(args[4])
  seed <- as.integer(args[5])
  
  if (length(args)>5) {
    debug <- as.logical(args[6])
    use_obs_synth <- as.logical(args[7])
    beta <- as.numeric(args[8])
    noise_sd <- as.numeric(args[9])
    flag_eos <- as.logical(args[10])
  } else {
    debug <- FALSE
    use_obs_synth <- FALSE
    beta <- 0
    noise_sd <- 0
    flag_eos <- FALSE
  }
  
  library("dplyr")
  library("lubridate")
  library("units")
  library("data.table")
  library("readxl")
  library("CroptimizR")
  library("CroPlotR")
  library("SticsRFiles")
  library("SticsOnR")
  library("here")
  library("truncnorm",lib="/home/buiss/work_inra_ea/emmah/sbuis/myRlibs")
  
  invisible(lapply(list.files(file.path(workspace,"R"), full.names=TRUE), function(x) source(x)))
  
  javastics_path=file.path(workspace,"JavaSTICS-1.40-stics-8.50")
  data_dir= file.path(workspace,"SticsData", test_case,"TxtFiles")
  
  flag_cluster <- TRUE
  
} else {

  library("here")
  source(file.path(here(),"R/install_load.r"))
  install_load()
  library(SticsOnR)
  library(SticsRFiles)
  
  # Define the test case ("French" or "Australian") and variety (only used for French dataset)
  test_case <- "Australian"
  variety <- "Janz"
  test_case <- "French"
  variety <- "Apache"  # "Apache" or "Bermude"
  
  javastics_path="D:\\Home\\sbuis\\Documents\\OUTILS-INFORMATIQUE\\STICS\\JavaSTICS-1.40-stics-8.50"
  data_dir= file.path("D:\\Home\\sbuis\\Documents\\PROJETS\\AgMIP\\AgMIP Calibration\\PhaseIV\\WORK\\SticsData",
                      test_case,"TxtFiles")
  nb_cores <- 4
  workspace <- here()
  
  # DEBUG mode (set to TRUE to test the protocol with limited number of situations, repetitions and evaluations, 
  #                    FALSE otherwise)
  debug <- TRUE
  
  # Synthetic observation mode (set to TRUE to test the protocol using synthetic observations 
  #                    FALSE otherwise)
  use_obs_synth <- TRUE
  beta <- 0.6 # 0.3 or 0.6
  noise_sd <- 0 # 0 or 0.1 (sd of gaussian noise, percentage)

  flag_eos <- TRUE # TRUE to compare simulated and observed final values of biomass and Yield on 31/12/harvestYear, 
                   # FALSE to compare them at harvest date
    
  flag_cluster <- FALSE

  seed <- 1234
}

out_dir <- file.path(workspace,"results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

print(getwd())
print(list.files())

if (flag_cluster) {
	file.copy(from=main_script_cluster.R, to=out_dir, overwrite = TRUE)
} else {
	file.copy(from=rstudioapi::getSourceEditorContext()$path, to=out_dir, overwrite = TRUE)
}				