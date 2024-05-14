# Install and load the needed libraries
if(!require("here")){
  install.packages("here")
  library("here")
}
source(file.path(here(),"R/install_load.r"))
install_load()

################################################################################
###### Initialization step => to be adapted to your case #######################
###### please read the comments and give the required information
################################################################################

# DEBUG mode (set to TRUE to test the protocol with limited number of situations, repetitions and evaluations, 
#                    FALSE to run phaseIV exercise)
debug <- TRUE

# Checkpoint-restart mode (set to TRUE will save temporary results in Rdata files 
# so that in case of crash the script can be re-run from the last successful step,
# see Details in https://github.com/sbuis/AgMIP-Calibration-Phase-IV description)
# However, this may result in large files (up to several hundreds MBytes) being stored.
checkpoint_restart  <- FALSE

# Define the test case ("French" or "Australian") and variety
test_case <- "French"
variety <- "Apache"  # "Apache" or "Bermude"
# test_case <- "Australian"
# variety <- "Janz"

# Set the path to the protocol description file to use (EXCEL file that describe the tables)
## replace path_to_protocol_description_file in the following line by the actual path to 
## the protocol description file (including its name).  
xls_path <- "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\data\\French_example_STICS_BUIS_test.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\APSIM_Dean Holzworth_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\DSSAT_Fallah_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\pyGecros_AndresBerger_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\NWHEAT_Mehmood_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\Hermes2Go_SCHULZ_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\French_template_DSSAT-NWHEAT_KIM_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\French_template_CROPSYST_MORIONDO_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\DSSAT-CERES_Jing&Qian_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\CROPSIM_Mehmood_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\CERES_Mehmood_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\02. French_template_SSM_FERRISE_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\French_template_AgroC_Weihermueller_Lutz_HM_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\French_example_AquaCrop_SOUNDHARAJAN_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\M30_French_APSIM_Ke Liu&Matthew Tom Harrison_revised 10 Jan 2024_SB_TMP.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\M1_AgroC_Weihermueller_Lutz_HM_SB_LW_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\French_template_MONICA_BAATZ_ROLAND-_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\French_template_DSSAT-NWHEAT_KIM (1)_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\DSSAT-CERES_Jing&Qian-1_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\DSSAT_Fallah (1)_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\CROPSIM_Mehmood_v2_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\NWHEAT_Mehmood_v2_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\CERES_Mehmood_v2_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\French_template_Lintul5_Amit_SB_25012024_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\SQ_MARTRE_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\M30_French_APSIM_Ke Liu&Matthew Tom Harrison_revised 10 Jan 2024_SB_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\M1_AgroC_Weihermueller_Lutz_2024_02_07_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\DSSAT-NWHEAT_KIM_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\2ndRound\\DSSAT_Fallah_2024_02_05_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\CERES_Mehmood_v3_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\CROPSIM_Mehmood_v3_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\DSSAT-NWHEAT_KIM_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\Expert-N_GECROS_RETTIE_WEBER_GAYLER_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\Expert-N_SPASS_RETTIE_WEBER_GAYLER_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\M17_MONICA_BAATZ_ROLAND-_SB_RB_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\M33_DSSAT_Fallah_2024_02_26_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\M35_OLC_thabo_SB_thabo_2024_01_15_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\3rdRound\\NWHEAT_Mehmood_v3_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\4thRound\\CoupModel_Lewan_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\4thRound\\French_example_CERES_SHELIA_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\4thRound\\M9_Copia di M9_CROPSYST_MORIONDO_SB_REV_2024_03_13_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\4thRound\\M16_French_template_Lintul5_Amit_SB_25012024_SB_2024_02_28.XLSX"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\4thRound\\PANORAMIX.CHN_ANDRIANASOLO_v2.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\4thRound\\Expert-N_CERES_RETTIE_WEBER_GAYLER.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\5thRound\\M31_pyGecros_AndresBerger_SB_AB_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\5thRound\\M35_OLC_thabo_SB.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\6thRound\\PANORAMIX.CHN_ANDRIANASOLO_v3.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\6thRound\\M17_MONICA_BAATZ_ROLAND-_SB_RB_SB_2024_03_22.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\7thRound\\PANORAMIX.CHN_ANDRIANASOLO_v3.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\7thRound\\NWHEAT_Mehmood_v4.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\7thRound\\CROPSIM_Mehmood_v4.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\7thRound\\CERES_Mehmood_v4.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\7thRound\\M17_MONICA_BAATZ_ROLAND-_SB_RB_SB_2024_03_08.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\8thRound\\PANORAMIX.CHN_ANDRIANASOLO_v4.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\8thRound\\M12_French_example_CERES_SHELIA_2024_03_06_SB_VSH.xlsx"
xls_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\Review Protocol Desc\\8thRound\\APSIM_Jing Wang.xlsx"

# Set-up your model wrapper
## Give here the name of your model (just used for naming some output files)
model_name <- "TEST"

# Give here the type of reference date used for computing julian days for phenological stages
# should be equal to "SowingYear" if julian days are computed from the beginning of the sowing year
# or "SowingDate" if julian days are computed from the sowing date.
descr_ref_date <- "SowingYear"

transform_sim <- NULL
transform_outputs <- NULL
transform_inputs <- NULL


# Set-up output results folder (OPTIONAL, set to results/test_case/variety by default)
# the following lines will set it to project_path/results/test_case/variety
# can be changed if needed
out_dir <- file.path(here(),"results",test_case,variety)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


# Synthetic observation mode (set to TRUE to test the protocol using synthetic observations 
#                                    FALSE to apply phaseIV protocol to real data)
# The following lines should not be changed if you want to run phaseIV exercise.
use_obs_synth <- FALSE
beta <- 0.3 # level of perturbation of parameters values : 0.3 or 0.6
noise_sd <- 0 # sd of gaussian noise added to synthetic observations (in percentage, set 0 or 0.1)
seed <- 1234 # seed for random number generation. Set it to a constant value for an exact replicate of the experiment. Change its value to change random number generation and thus synthetic observations and default parameters values.

flag_eos <- TRUE # TRUE to compare simulated and observed final values of biomass and Yield on 31/12/harvestYear, 
# FALSE to compare them at harvest date
# Set TRUE to remove Minnipa observations from Australian dataset, FALSE otherwise
data_without_Minnipa <- TRUE


################################################################################
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
