# Results path

  library("here")
  library(SticsOnR)
  library(SticsRFiles)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Define the test case ("French" or "Australian") and variety (only used for French dataset)
  test_case <- "Australian"
  variety <- "Janz"
  test_case <- "French"
  variety <- "B"  # "Apache" or "Bermude"
  variety <- "A"  # "Apache" or "Bermude"
  
  javastics_path="D:\\Home\\sbuis\\Documents\\OUTILS-INFORMATIQUE\\STICS\\JavaSTICS-1.40-stics-8.50"
  nb_cores <- 4
  workspace <- here()
  
# Set-up your model wrapper and name
model_wrapper <- stics_wrapper

# Define model_options depending on your model wrapper
data_dir= file.path("D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\WORK\\SticsData_ANONYM",
                    test_case,"TxtFiles_snu_end_sowing")
model_options= stics_wrapper_options(javastics=javastics_path, workspace = data_dir, 
                                     parallel=TRUE, cores = nb_cores)

res_ORI <- stics_wrapper(model_options = model_options, param_values = c(variete=1), 
                     var = c("HR_1","HR_2","HR_3","HR_4","HR_5","AZnit_1",
                             "AZnit_2","AZnit_3","AZnit_4","AZnit_5"))

data_dir= file.path("D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\WORK\\SticsData_ANONYM",
                    test_case,"TxtFiles_snu_ini_N_240305")
model_options= stics_wrapper_options(javastics=javastics_path, workspace = data_dir, 
                                     parallel=TRUE, cores = nb_cores)

res_new_N_ini <- stics_wrapper(model_options = model_options, param_values = c(variete=1), 
                               var = c("HR_1","HR_2","HR_3","HR_4","HR_5","AZnit_1",
                                       "AZnit_2","AZnit_3","AZnit_4","AZnit_5"))

plot(ORI=res_ORI$sim_list, new_N_ini=res_new_N_ini$sim_list)
