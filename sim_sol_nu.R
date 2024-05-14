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
  data_dir= file.path("D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\WORK\\SticsData_ANONYM",
                      test_case,"TxtFiles_snu")
  nb_cores <- 4
  workspace <- here()
  
  
# generate snu plant file in txt format
convert_xml2txt(file=file.path(javastics_path,"plant","baresoil_plt.xml"), 
                out_dir = data_dir)
# copy it in the different USM folders
dirlist <- list.dirs(path = data_dir, full.names = TRUE, recursive = FALSE)  
for (dir in dirlist) {
  file.copy(from=file.path(data_dir,"ficplt1.txt"), to=dir, overwrite = TRUE)
}
# change variete number
for (dir in dirlist) {
  set_tec_txt(file = file.path(dir,"fictec1.txt"), param = "variete", value = 1)
}

# Set-up your model wrapper and name
model_wrapper <- stics_wrapper

# Define model_options depending on your model wrapper
model_options= stics_wrapper_options(javastics=javastics_path, workspace = data_dir, 
                                     parallel=TRUE, cores = nb_cores)

res <- stics_wrapper(model_options = model_options, param_values = c(variete=1), 
                     var = c("HR_1","HR_2","HR_3","HR_4","HR_5","lai_n", "masec_n"))

cal_inputs <- read.csv2(file = "D:\\Home\\sbuis\\Documents\\GitHub\\AgMIP-Calibration-Phase-IV\\data\\cal_4_input_data_French_A_SB.txt",
          header = TRUE, stringsAsFactors = FALSE, sep = "\t")
cal_inputs$SowingDate <- as.Date(cal_inputs$SowingDate, format=c("%d/%m/%Y"))
# cal_inputs$SowingDate <- as.POSIXct(cal_inputs$SowingDate, format=c("%d/%m/%Y"), tz = "UTC")

HR <- list()
start_day<- list()
xml_path <- "D:\\Home\\Partage\\PROJETS\\AgMIP Calibration PhaseIV\\WORK\\SticsData_ANONYM\\French\\XmlFiles-NEW_ini_sowing_date"
for (sit in cal_inputs$Situation.Name) {
  sowingDate <- filter(cal_inputs, Situation.Name==sit) %>% select(SowingDate)
  print(sit)
  res$sim_list[[sit]]$Date <- as.Date(res$sim_list[[sit]]$Date)
  print(HR[[sit]] <- res$sim_list[[sit]] %>% select(Date,HR_1,HR_2,HR_3,HR_4,HR_5) %>% 
          filter(between(Date,sowingDate[[1]],sowingDate[[1]])))
  start_day[[sit]] <- compute_day_from_date(date=sowingDate[[1]], 
                                            start_date=as.Date(paste0(lubridate::year(sowingDate[[1]]),"-01-01")))
  set_param_xml(file=file.path(xml_path,"usms.xml"), param = "datedebut",
                values = start_day[[sit]], select = "usm", select_value = sit,
                overwrite = TRUE)
}