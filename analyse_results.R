# Results path
results_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\results\\French\\REAL_data\\A_results_protocol_descr_initSoissons_stressdev02_20240328"
results_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\results\\French\\REAL_data\\A_results_protocol_descr_initSoissons_stressdev02_modifBenjaminV1_20240410"
results_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\results\\French\\REAL_data\\A_results_protocol_descr_initSoissons_stressdev02_modifBenjaminV4_20240415"
results_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\results\\French\\REAL_data\\French_example_STICS_BUIS_Soissons_stressdev02_rfpi_new_20240419_v3_imatsPlus10/"
results_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\results\\French\\REAL_data\\French_example_STICS_BUIS_Soissons_stressdev02_rfpi_new_20240419_v1/"
results_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\results\\French\\REAL_data\\A_V1_ini_N_240305"
results_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\results\\French\\REAL_data\\A_V1_codeir1_ini_N_240305"
results_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\results\\French\\REAL_data\\A_V1_codeir1_irmax_ini_N_240305"

  library("here")
  source(file.path(here(),"R/install_load.r"))
  install_load()
  library(SticsOnR)
  library(SticsRFiles)
  library(ggplot2)
  
  # Define the test case ("French" or "Australian") and variety (only used for French dataset)
  test_case <- "Australian"
  variety <- "Janz"
  test_case <- "French"
  variety <- "B"  # "Apache" or "Bermude"
  variety <- "A"  # "Apache" or "Bermude"
  
  javastics_path="D:\\Home\\sbuis\\Documents\\OUTILS-INFORMATIQUE\\STICS\\JavaSTICS-1.40-stics-8.50"
  data_dir= file.path("D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\WORK\\SticsData_ANONYM",
                      test_case,"TxtFiles")
  data_dir= file.path("D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\WORK\\SticsData_ANONYM",
                      test_case,"TxtFiles_ini_N_240305")
  nb_cores <- 4
  workspace <- here()
  
  # DEBUG mode (set to TRUE to test the protocol with limited number of situations, repetitions and evaluations, 
  #                    FALSE otherwise)
  debug <- FALSE
  
  # REGRESSION TEST MODE
  regtest <- FALSE

  # Synthetic observation mode (set to TRUE to test the protocol using synthetic observations 
  #                    FALSE otherwise)
  use_obs_synth <- FALSE
  beta <- 0.6 # 0.3 or 0.6
  noise_sd <- 0 # 0 or 0.1 (sd of gaussian noise, percentage)

  flag_eos <- TRUE # TRUE to compare simulated and observed final values of biomass and Yield on 31/12/harvestYear, 
                   # FALSE to compare them at harvest date

  if (regtest) {
    debug <- TRUE
    use_obs_synth <- TRUE
    beta <- 0.6 # 0.3 or 0.6
    noise_sd <- 0 # 0 or 0.1 (sd of gaussian noise, percentage)
    
    flag_eos <- TRUE # TRUE to compare simulated and observed final values of biomass and Yield on 31/12/harvestYear, 
  }
  
  flag_cluster <- FALSE

  seed <- 1234

# Checkpoint-restart mode (set to TRUE will save temporary results in Rdata files 
# so that in case of crash the script can be re-run from the last successful step,
# see Details in https://github.com/sbuis/AgMIP-Calibration-Phase-IV description)
# However, this may result in large files (up to several hundreds MBytes) being stored.
checkpoint_restart  <- FALSE
if (debug) {
  checkpoint_restart <- TRUE
}
  
#################################################
# Set TRUE to remove Minnipa obs, FALSE otherwise
data_without_Minnipa <- TRUE
#################################################

################################################################################
###### Initialization step => tp be adapted to your case #######################



# Set-up your model wrapper and name
model_wrapper <- stics_wrapper

# Define model_options depending on your model wrapper
model_options= stics_wrapper_options(javastics=javastics_path, workspace = data_dir, 
                                     parallel=TRUE, cores = nb_cores)

# Set-up output results folder
out_dir <- file.path(workspace,"results",test_case,variety)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Define model output transformation(s) if necessary.
# Useful if one (or several) observed variable is not directly comparable to a 
# simulated one but if one can compute an equivalent from the simulated variables.
# In the following example, N_in_biomassHarvest (observed in French dataset) is not 
# directly computed by the model.
# The model simulates a variable, called QNplante, which is the N in biomass in kg ha-1, 
# and the biomass, called masec_n, in t ha-1.
# N_in_biomassHarvest (in %) can be computed from these simulated variables,
# by dividing QNplante by (masec_n*10).
# To do that, we created a transform_sim function that will be automatically run after each 
# model simulations in estim_param.
# If you define a transform_sim function, list in tranform_outputs the variables 
# generated by transform_sim, and in tranform_inputs those required in input of 
# transform_sim to compute them.
# If no transformation required, let transform_sim, transform_outputs and transform_inputs 
# to NULL
transform_sim <- NULL
transform_outputs <- NULL
transform_inputs <- NULL
if (test_case=="French") {
  
  if (!regtest) {
    
    transform_sim <- function(model_results, ...) {
      
      # Create the new variables for each situation included in model_results$sim_list
      for (sit in names(model_results$sim_list)) {
        model_results$sim_list[[sit]]$N_in_biomassHarvest <- 
          model_results$sim_list[[sit]]$QNplante / (10 * model_results$sim_list[[sit]]$masec_n)
        model_results$sim_list[[sit]]$ProteinContentGrain <- 
          model_results$sim_list[[sit]]$CNgrain * 5.7
      }
      return(model_results)  
    }
    
    transform_outputs <- c("N_in_biomassHarvest", "ProteinContentGrain")
    transform_inputs <- c("QNplante", "CNgrain")
    
  }
   
 }
# Set the name of the protocol description files
  xls_path <- file.path(results_path,"results","French_example_STICS_BUIS.xlsx")

# Give here the type of reference date used for computing julian days for phenological stage
# should be equal to "SowingYear" if julian days are computed from the beginning of the sowing year
# or "SowingDate" if julian days are computed from the sowing date
descr_ref_date <- "SowingYear"

###### end of initialization step => the following should not be changed #######
################################################################################

set.seed(seed)


# check descr_ref_date
if (!(descr_ref_date %in% c("SowingYear","SowingDate"))) {
  stop("descr_ref_date must be equal to \"SowingYear\" or to \"SowingDate\", please correct its definition.")
}

# check path to the protocol description file
if (!file.exists(xls_path)) {
  stop("The path given for the protocol description file does not correspond to an existing file. Please check it.")
}					  
# Load the protocol description file (xls file) 
protocol_descr <- load_protocol(xls_path, transform_outputs, use_obs_synth, beta)
sitNames_corresp <- protocol_descr$sitNames_corresp 
varNames_corresp <- protocol_descr$varNames_corresp 
simVar_units <- protocol_descr$simVar_units 
param_info <- protocol_descr$param_info
forced_param_values <- protocol_descr$default_param_values 
param_group <- protocol_descr$param_group # list of params to estimate per group
obsVar_group <- protocol_descr$obsVar_group # groups of observed variables used in the calibration
converted_obsVar_group <- setNames(obsVar_group,nm=varNames_corresp[names(obsVar_group)])
true_param_values <- protocol_descr$true_param_values 

# Load the observations
suffix <- NULL
if (test_case=="French") suffix <- paste0("_",variety) 
if (test_case=="Australian" & data_without_Minnipa) {
  obs_data_folder <- "data_without_Minnipa_eos"
} else {
  obs_data_folder <- "data"
}
obs_data_path <- file.path(here(),obs_data_folder,paste0("cal_4_obs_",test_case,suffix,".txt"))
obs_unit_path <- file.path(here(),obs_data_folder,paste0("cal_4_obs_",test_case,"_units.csv"))
## Get the reference date for each situation
template_path <- file.path(here(),"data",paste0("simulations_template.txt"))
ref_date <- get_reference_date(descr_ref_date, template_path)

obs <- load_obs(obs_data_path, obs_unit_path, varNames_corresp, 
                sitNames_corresp, simVar_units, obsVar_group, flag_eos, ref_date)

obs_list <- obs$obs_list  # list of observation as defined in the observation file
obsVar_names <- obs$obsVar_names # Names of the observed variables as defined in the observation file
obsVar_units <- obs$obsVar_units # Units of the observed variables as defined in the observation file
obsVar_used <- obs$obsVar_used # NAmes of the observed variables used in the current protocol application
converted_obs_list <- obs$converted_obs_list # list of observation in th emodel space (i.e. with name of situation and variables as in the model_wrapper, and units as defined in model outputs)
harvest_year <- obs$harvest_year

# Get the list of variables for which the user must provide 
# results in the cal_4_results_***.txt file)
template_df <- read.table(template_path,
                          header = TRUE, stringsAsFactors = FALSE)
resVar_names <- setdiff(names(template_df),c("Number","Site","HarvestYear",
                                             "Date_sowing", "SowingDate", 
                                             "Variety","Date"))

## Check that the list of "observed and required variables" as defined in the protocol 
## description xls file is included in the union of observed and required variables
## as they are defined in the cal_4_obs and cal_4_reslts files.
if (!all(names(varNames_corresp) %in% unique(c(obsVar_names, resVar_names)))) 
  stop(paste0("Unknown variable(s) ",
              paste(setdiff(names(varNames_corresp), unique(c(obsVar_names, resVar_names))), collapse = ","), 
              "\nPlease modify sheet \"variables\", column \"Name of the observed or required variable\" of the file:\n",
              xls_path,
              "\nThe variables included in this column must be included in the list of variables defined in files:\n",
              template_path, "\nand\n",obs_data_path))

# Compute the list of required variables in output of the wrapper
# (list of variables for which there is a correspondance with observed and results variables + 
#  transform_inputs - transform_outputs)
reqVar_Wrapper <- setdiff(c(varNames_corresp,transform_inputs),transform_outputs)

# Generate synthetic observations if required  
sim_true <- NULL
if (use_obs_synth) {
  
  obs_synth <- generate_obs_synth(true_param_values=c(true_param_values), 
                                  model_wrapper, 
                                  model_options, sitNames_corresp, 
                                  reqVar_Wrapper, converted_obs_list, transform_sim,
                                  simVar_units, varNames_corresp, obsVar_units,  
                                  obs_list, obsVar_used, noise_sd, descr_ref_date,
                                  flag_eos)
  obs_list <- obs_synth$obs_list
  converted_obs_list <- obs_synth$converted_obs_list
  sim_true <- obs_synth$sim_true
  
}
														

# In debug mode reduce number of evaluations, situations, repetitions, candidate parameters, ...
if (debug) {
  cat("\nDebug mode ...\n")
  sit_list <- names(converted_obs_list)[1:6]
  converted_obs_list <- filter_obs(converted_obs_list,
                                       situation = sit_list,
                                       include=TRUE)
  obs_list <- filter_obs(obs_list,
                         situation = names(obs_list)[1:6],
                         include=TRUE)
  for (gr in names(param_group)) { # only keep the 1st candidate
    if (!is.null(param_group[[gr]]$candidates)) 
      param_group[[gr]]$candidates <- param_group[[gr]]$candidates[1]
  }
}

# Save configuration
if (checkpoint_restart) {
  save.image(file=file.path(out_dir,"config.Rdata"))
}								  


# Initialize some local variables 
flag_checkpoint <- FALSE
igr <- 0
res_it1 <- list(); res_it1_tmp <- NULL; res_it2 <- NULL;
weight_it2 <- NULL;
complem_info <- list(it1=list(), it2=list())
												
if (file.exists(file.path(out_dir,"checkpoint.Rdata"))) {
  load(file.path(out_dir,"checkpoint.Rdata"))
  flag_checkpoint <- TRUE
  if (file.exists(file.path(out_dir,"config.Rdata"))) {
    load(file.path(out_dir,"config.Rdata"))
  }
  if (file.exists(file.path(out_dir,"complementary_info.Rdata"))) {
    load(file.path(out_dir,paste0("complementary_info.Rdata")))
  }
}

# Initialize tranqsformation function
transform_var <- NULL
transform_var_converted <- NULL
if ("Biomass" %in% names(varNames_corresp)) {
  transform_var <- eval(parse(text=paste0("c(",varNames_corresp[["Biomass"]],
                                          "=function(x) log(x+.Machine$double.xmin))")))
  transform_var_converted <- transform_var
  names(transform_var_converted) <- 
    names(varNames_corresp)[match(names(transform_var),varNames_corresp)]
}
  

# load results
load(file.path(results_path,"results\\step7\\optim_results.Rdata"))

reqVar_Wrapper <- c(reqVar_Wrapper,"p1000grain","lai_n", "AZnit_1", "AZnit_2", "AZnit_3")

# Run model wrapper using parameter values estimated in step 7
sim_it2 <- run_wrapper(model_wrapper = model_wrapper,
                       model_options=model_options,
                       param_values=c(res$final_values, res$forced_param_values),
                       situation=sitNames_corresp, var=reqVar_Wrapper, 
                       obs_list=converted_obs_list,
                       transform_sim=transform_sim, transform_var=NULL)

# compute QNplante and grain_dry_weight_mg
# masec_n : t/ha
# N_in_biomassHarvest : %
# QNplante sim : kg/ha
new_obs <- lapply(converted_obs_list,function(x) {
  x$QNplante <- x$masec_n * 10 * x$N_in_biomassHarvest
  x$p1000grain <- x$mafruit * 1e+6 * 1000 / (x$chargefruit * 10000)
  x
})

plot(sim_it2$sim_list, obs=new_obs, type="scatter", shape_sit="symbol")
ggsave(filename=file.path(results_path,"results","scatterplot_symbol.png"))

plot(sim_it2$sim_list, obs=new_obs, var=c("QNplante", "p1000grain"), type="scatter")
ggsave(filename=file.path(results_path,"results","scatterplots_p1000grain_QNplante.png"))

plot(sim_it2$sim_list, obs=new_obs, var=c("mafruit", "masec_n", "lai_n"), select_dyn = "all")
ggsave(filename=file.path(results_path,"results","dynamic_plot.png"))

lai <- sapply(sim_it2$sim_list, function(x) max(x$lai_n))

png(file=file.path(results_path,"results","boxplot_max_lai.png"))
boxplot(lai)
title("max(lai)")
dev.off()


source("D:\\Home\\sbuis\\Documents\\RESEAUX\\EPS\\CopiesDeTravail\\Depot_TEST\\Stics_Tests\\trunk\\R\\IDESticsR\\generate_eval_files.R")
generate_eval_files(file.path(results_path,"results","EvalFiles"),sim_it2$sim_list,new_obs)
