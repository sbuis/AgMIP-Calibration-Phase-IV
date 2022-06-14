# Install and load the needed libraries
if(!require("here")){
  install.packages("here")
  library("here")
}
source(file.path(here(),"R/install_load.r"))
install_load()

# Define the test case ("French" or "Australian") and variety (only used for French dataset)
# test_case <- "French"
test_case <- "Australian"
# variety <- "Apache"  # "Apache" or "Bermude"

# Set-up your model wrapper
library(SticsOnR)
library(SticsRFiles)
model_wrapper <- stics_wrapper 

# Define model_options depending on your model wrapper
javastics_path="D:\\Home\\sbuis\\Documents\\OUTILS-INFORMATIQUE\\STICS\\JavaSTICS-1.40-stics-8.50"
data_dir= file.path("D:\\Home\\sbuis\\Documents\\PROJETS\\AgMIP\\AgMIP Calibration\\PhaseIV\\WORK\\SticsData",
                    test_case,"TxtFiles")
model_options= stics_wrapper_options(javastics=javastics_path, workspace = data_dir, 
                                     parallel=TRUE, cores = 4)


# Set-up output results folder
out_dir <- file.path(here(),"results",test_case)

## Describe the correspondance between observed and simulated situations and variables
# Correspondance between situation Number, as defined in the input data set, and 
# situations names as defined in the model_wrapper, for all situations (calibration 
# and evaluation).
# If situations names defined in the wrapper are the same numbers, just set 
# sitNames_corresp to NULL

if (test_case=="French") {
  sitNames_corresp <- c(`1`="FORE-Ap-12-122210", `3`="FORE-Ap-13-124093",
                        `5`="FORE-Ap-14-126441", `7`="FORE-Ap-14-126640",
                        `9`="FORE-Ap-15-128565", `11`="MERY-Ap-10-116375",
                        `13`="MERY-Ap-11-119630", `15`="MERY-Ap-13-124090", 
                        `17`="MERY-Ap-14-126416", `19`="MERY-Ap-15-128539", 
                        `21`="ROUV-Ap-12-122200", `23`="ROUV-Ap-13-124097",
                        `25`="ROUV-Ap-14-126421", `27`="ROUV-Ap-15-128544",
                        `29`="Cess-Ap-11-119440", `31`="IVIL-Ap-12-122207",
                        `33`="VILL-Ap-13-124385", `35`="EPRE-Ap-14-126435", 
                        `37`="CRES-Ap-15-128556", 
                        `39`="OUZO-Ap-11-119948", `41`="OUZO-Ap-12-122218",
                        `43`="OUZO-Ap-13-124108", `45`="OUZO-Ap-14-126436",
                        `47`="OUZO-Ap-15-128559", `49`="BIGN-Ap-10-116364",
                        `51`="BIGN-Ap-11-119434", `53`="BIGN-Ap-12-122105",
                        `55`="BIGN-Ap-13-124085", 
                        `57`="BIGN-Ap-14-126443", `59`="BIGN-Ap-15-128534",
                        `61`="BOIG-Ap-12-122153", `63`="BOIG-Ap-13-124104", 
                        `65`="BOIG-Ap-14-126424", 
                        `67`="BOIG-Ap-15-128547", `69`="BOIG-Ap-16-131778")
} else {
  sitNames_corresp <- c(`1`="Bung-2012-TOS1", `2`="Bung-2012-TOS2", `3`="Bung-2012-TOS3", 
                        `4`="Corr-2010-TOS1", `5`="Corr-2010-TOS2", `6`="Corr-2010-TOS3", 
                        `7`="Corr-2011-TOS1",
                        `8`="Corr-2011-TOS2", `9`="Corr-2011-TOS3", 
                        `10`="Corr-2012-TOS1",
                        `11`="Corr-2012-TOS2", `12`="Corr-2012-TOS3", 
                        `13`="Erad-2010-TOS1", `14`="Erad-2010-TOS2", `15`="Erad-2010-TOS3", 
                        `16`="Erad-2011-TOS1", `17`="Erad-2011-TOS2",
                        `18`="Erad-2011-TOS3", 
                        `19`="Erad-2012-TOS1", `20`="Erad-2012-TOS2",
                        `21`="Erad-2012-TOS3", 
                        `22`="Lake-2010-TOS1", `23`="Lake-2010-TOS2", `24`="Lake-2010-TOS3", 
                        `25`="Lake-2011-TOS1", `26`="Lake-2011-TOS2", `27`="Lake-2011-TOS3",
                        `28`="Minn-2010-TOS1", `29`="Minn-2010-TOS2", `30`="Minn-2010-TOS3",
                        `31`="Minn-2011-TOS1", `32`="Minn-2011-TOS2", `33`="Minn-2011-TOS3", 
                        `34`="Minn-2012-TOS1", `35`="Minn-2012-TOS2", `36`="Minn-2012-TOS3", 
                        `37`="Nang-2012-TOS1",
                        `38`="Nang-2012-TOS2", `39`="Nang-2012-TOS3", 
                        `40`="Spri-2010-TOS1",
                        `41`="Spri-2010-TOS2", `42`="Spri-2010-TOS3", 
                        `43`="Spri-2011-TOS1", `44`="Spri-2011-TOS2", `45`="Spri-2011-TOS3", 
                        `46`="Temo-2011-TOS1", `47`="Temo-2011-TOS2",
                        `48`="Temo-2011-TOS3", 
                        `49`="Temo-2012-TOS1", `50`="Temo-2012-TOS2",
                        `51`="Temo-2012-TOS3", 
                        `52`="Turr-2011-TOS1", `53`="Turr-2011-TOS2", `54`="Turr-2011-TOS3", 
                        `55`="Turr-2012-TOS1", `56`="Turr-2012-TOS2", `57`="Turr-2012-TOS3",
                        `58`="Walp-2010-TOS1", `59`="Walp-2010-TOS2", `60`="Walp-2010-TOS3",
                        `61`="Walp-2011-TOS1", `62`="Walp-2011-TOS2", `63`="Walp-2011-TOS3", 
                        `64`="Walp-2012-TOS1", `65`="Walp-2012-TOS2", `66`="Walp-2012-TOS3")
}


  # Give correspondence between observed variables names and simulated variables names
# (including the ones computed in transform_sim), set NULL if no correspondence
if (test_case=="French") {
  varNames_corresp <- c(Harvest_Date="imats", Date_BBCH30="iamfs", Date_BBCH55="ilaxs", 
                        Biomass="masec_n", EarsPerSqM=NULL, Wt1000_Grain="p1000grain", 
                        ProteinContentGrain=NULL, N_in_biomassHarvest="N_in_biomassHarvest",
                        Grain_Biomass="mafruit")
} else {
  varNames_corresp <- c(HarvestDate="imats", Date_BBCH10="ilevs", Date_BBCH30="iamfs", Date_BBCH55="ilaxs", 
                        Date_BBCH65="iflos", Date_BBCH71="idrps", Biomass="masec_n", 
                        Wt1000_Grain="p1000grain", Grain_Number="chargefruit",
                        Yield="mafruit")
}

# Give units of simulated variables listed in varNames_corresp
# Units always need to be specified in quotation marks (i.e. ""). 
# A full list of available units can be accessed through `units::valid_udunits()`. 
# Two different ways of composing base units are supported: a fraction notation 
# where exponents are proceeded by `^` (eg. "m/s" or "kg\*m/s^2") and a factor 
# notation where exponents are directly attached to the unit symbols 
# (eg. "m s-1" or "kg m s-2"). 
# Importantly, these two notations should not be mixed (eg. do not write "kg\*m/s2" 
# or "kg m/s^2", the former misses an `*` and the latter misses a `*`).
if (test_case=="French") {
  simVar_units <- c(imats="d", iamfs="d", ilaxs="d", masec_n="t ha-1", 
                  p1000grain="g", N_in_biomassHarvest="%")
} else {
  simVar_units <- c(imats="d", ilevs="d", iamfs="d", ilaxs="d", iflos="d", idrps="d", masec_n="t ha-1", 
                    p1000grain="g", chargefruit="m-2", mafruit="t ha-1")
}
# In this example, N_in_biomassHarvest is not direclty computed by the model.
# The model simulates a variable, called QNplante, which is the N in biomass in kg ha-1, 
# and the biomass, called masec_n, in t ha-1.
# The model results must thus be explicitly transformed to compute N_in_biomassHarvest in %,
# by dividing QNplante by (masec_n*10)
# To do that we create a transform_sim function that will be automatically run after each 
# model simulations in estim_param
# List in tranform_outputs the variables generated by transform_sim, and in 
# tranform_inputs those required in input of transform_sim to compute them.
# If no transformation required, set them to NULL
transform_sim <- NULL
transform_outputs <- NULL
transform_inputs <- NULL
if (test_case=="French") {
  transform_sim <- function(model_results, ...) {
    
    # Create the new variable for each situation included in model_results$sim_list
    for (sit in names(model_results$sim_list)) {
      model_results$sim_list[[sit]]$N_in_biomassHarvest <- 
        model_results$sim_list[[sit]]$QNplante / (10 * model_results$sim_list[[sit]]$masec_n)
    }
    return(model_results)  
  }
  
  transform_outputs <- c("N_in_biomassHarvest")
  transform_inputs <- c("QNplante")
  
}



# Describe the parameters to estimate and give the list of obligatory and candidates per group

## Bounds of the parameters
if (test_case=="French") {
  param_info <- list(lb=c(stlevamf=1, stamflax=1, jvc=0, efcroiveg=1, efcroirepro=1, pgrainmaxi=1),
                     ub=c(stlevamf=1000, stamflax=1000, jvc=80, efcroiveg=5, efcroirepro=6, pgrainmaxi=4))
  
  ## Default values of the parameters
  forced_param_values <- c(jvc=20)
} else {
  param_info <- list(lb=c(stlevamf=1, stamflax=1, stlevdrp=1, stflodrp=1, stdrpmat=1, jvc=0, 
                          efcroiveg=1, efcroirepro=1, pgrainmaxi=1),
                     ub=c(stlevamf=1000, stamflax=1000, stlevdrp=1000, stflodrp=1000, stdrpmat=1000, 
                          jvc=80, efcroiveg=5, efcroirepro=6, pgrainmaxi=4))
  
  ## Default values of the parameters
  forced_param_values <- c(jvc=0)
}


group <- list()
## Set NULL to both obligatory and candidates if the group should not be considered
## BE CAREFUL the order of definition of the groups will determine their order during 
## the parameter estimation process

if (test_case=="French") {
  group$phenology$obligatory <- c("stlevamf", "stamflax", "stdrpmat")
} else {
  group$phenology$obligatory <- c("stlevamf", "stamflax", "stlevdrp", "stflodrp", "stdrpmat")
}
group$phenology$candidates <- c("jvc")

group$lai$obligatory <- NULL
group$lai$candidates <- NULL

group$`soil water`$obligatory <- NULL
group$`soil water`$candidates <- NULL

group$`soil nitrogen`$obligatory <- NULL
group$`soil nitrogen`$candidates <- NULL

group$`plant biomass`$obligatory <- c("efcroiveg", "efcroirepro")
group$`plant biomass`$candidates <- NULL

group$`plant N content`$obligatory <- NULL
group$`plant N content`$candidates <- NULL

group$yield$obligatory <- c("pgrainmaxi")
group$yield$candidates <- NULL

group$`seed protein`$obligatory <- NULL
group$`seed protein`$candidates <- NULL


# Check information given by the user
check_user_inputs(simVar_units, varNames_corresp, transform_outputs)

# Load the observations
suffix <- NULL
if (test_case=="French") suffix <- paste0("_",variety) 
obs_data_path <- file.path(here(),"data",paste0("cal_4_obs_",test_case,suffix,".txt"))
obs_unit_path <- file.path(here(),"data",paste0("cal_4_obs_",test_case,"_units.csv"))
obs <- load_obs(obs_data_path, obs_unit_path, varNames_corresp, 
                sitNames_corresp, simVar_units)

######## A ENLEVER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if (test_case=="French") {
  obs$converted_obs_list <- filter_obs(obs$converted_obs_list, 
                           situation = c("MERY-Ap-14-126416", "MERY-Ap-11-119630",
                                         "MERY-Ap-15-128539"), 
                           include=TRUE)
} else {
  obs$converted_obs_list <- filter_obs(obs$converted_obs_list, 
                                       situation = c("Lake-2011-TOS2", "Minn-2011-TOS3",
                                                     "Erad-2010-TOS1"), 
                                       include=TRUE)
}
######## A ENLEVER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


flag_checkpoint <- FALSE
igr <- 0
res_it1 <- list(); res_it1_tmp <- NULL; res_it2 <- NULL; res_it3 <- NULL
weight_it2 <- NULL; weight_it3 <- NULL
complem_info <- list(it1=list(), it2=list(), it3=list())
crt_forced_param_values <- forced_param_values

if (file.exists(file.path(out_dir,"checkpoint.Rdata"))) load(file.path(out_dir,"checkpoint.Rdata"))

## Parameter Estimation, first iteration

cat("\n----- Parameter estimation Iteration 1\n")
cat("--------------------------------------\n")

############ A ENLEVER #######################################
optim_options=list(nb_rep=2, maxeval=3, ranseed=1234)
############ A ENLEVER #######################################

transform_var <- eval(parse(text=paste0("c(",varNames_corresp[["Biomass"]],
                                        "=function(x) log(x+.Machine$double.xmin))")))
wrapper_outputs <- setdiff(c(names(simVar_units),transform_inputs),transform_outputs)

crit_function <- function(sim_list, obs_list) {
  weight <- summarise(tibble(bind_rows(obs_list)), 
                      across(everything(),mean, 
                             na.rm=TRUE))
  alpha <- 0.1
  weight_it1 <<- unique(bind_rows(weight_it1, alpha*select(weight,-Date)))
  crit_wls(sim_list, obs_list, 
           weight=weight,
           alpha=alpha)
}

while (igr < length(group)) {
  
  igr <- igr+1
  weight_it1 <- NULL
  gr <- names(group)[igr]
  cat(paste("\n---------------- Group",gr,"\n"))
  
  ## Filter observations to use for the current group
  crt_var_list <- varNames_corresp[intersect(obs$obsVar_used,
                                             obs$obsVar_names[grep(gr, obs$obsVar_groups)])]
  crt_obs_list <- filter_obs(obs_list=obs$converted_obs_list, var=crt_var_list, include=TRUE)
  
  ## Filter information on the parameters to estimate for the current group
  crt_params <- c(group[[gr]]$obligatory, group[[gr]]$candidates)
  crt_param_info <- lapply(param_info,function(x) x[crt_params])

  optim_options$out_dir <- file.path(out_dir,"Iteration1",paste0("group_",gr))
  
  if (!is.null(res_it1_tmp)) crt_forced_param_values[names(res_it1_tmp$final_values)] <- res_it1_tmp$final_values
  
  res_it1_tmp <- estim_param(obs_list=crt_obs_list, 
                     crit_function = crit_function,
                     model_function=stics_wrapper,
                     model_options=model_options,
                     optim_options=optim_options,
                     param_info=crt_param_info, candidate_param=group[[gr]]$candidates,
                     forced_param_values=crt_forced_param_values, 
                     transform_var=transform_var,
                     transform_sim=transform_sim, var=wrapper_outputs)
  
  res_it1[[gr]] <- res_it1_tmp
  
  save(res_it1_tmp, res_it1, igr, crt_forced_param_values, 
       file = file.path(out_dir,paste0("checkpoint_it1_gr",igr,".Rdata")))
  
  complem_info$it1[[gr]] <- list(forced_param_values=crt_forced_param_values,
                                 obsVar_used=crt_var_list,
                                 weight=weight_it1)
  save(complem_info, 
       file = file.path(out_dir,paste0("complementary_info.Rdata")))
  
}



## Parameter Estimation, Second iteration

cat("\n----- Parameter estimation Iteration 2\n")
cat("--------------------------------------\n")

############ A ENLEVER #######################################
optim_options=list(nb_rep=2, maxeval=3, ranseed=1234)
############ A ENLEVER #######################################

final_params <- unlist(lapply(res_it1, function(x) names(x$final_values)))
final_param_info <- lapply(param_info,function(x) x[final_params])
final_forced_param_values <- forced_param_values[setdiff(names(forced_param_values),
                                                         final_params)]
if (is.null(res_it2)) {
 
  optim_options$out_dir <- file.path(out_dir,"Iteration2")
  
  crit_function <- function(sim_list, obs_list) {
    weight <- summarise(tibble(bind_rows(obs_list)), 
                        across(everything(),mean, 
                               na.rm=TRUE))
    alpha <- 0.1
    weight_it2 <<- unique(bind_rows(weight_it2,alpha*select(weight,-Date)))
    crit_wls(sim_list, obs_list, 
             weight=weight,
             alpha=alpha)
  }
  
  res_it2 <- estim_param(obs_list=obs$converted_obs_list, 
                         crit_function = crit_function,
                         model_function=stics_wrapper,
                         model_options=model_options,
                         optim_options=optim_options,
                         param_info=final_param_info,
                         forced_param_values=final_forced_param_values,
                         transform_var=transform_var,
                         transform_sim=transform_sim, var=wrapper_outputs)

  save(res_it1, igr, res_it2, 
       file = file.path(out_dir,paste0("checkpoint_it2.Rdata")))
  
  complem_info$it2 <- list(forced_param_values=final_forced_param_values,
                           obsVar_used=varNames_corresp[varNames_corresp %in% unlist(lapply(obs$converted_obs_list,names))],
                           weight=weight_it2)
  save(complem_info, 
       file = file.path(out_dir,paste0("complementary_info.Rdata")))
  
}



## Parameter Estimation, Third iteration

cat("\n----- Parameter estimation Iteration 3\n")
cat("--------------------------------------\n")

if (is.null(res_it3)) {
  
  ############ A ENLEVER #######################################
  optim_options=list(nb_rep=2, maxeval=3, ranseed=1234)
  ############ A ENLEVER #######################################
  
  optim_options$out_dir <- file.path(out_dir,"Iteration3")
  
  sim_it2 <- run_wrapper(model_options=model_options,
                           param_values=c(final_forced_param_values,res_it2$final_values),
                           situation=sitNames_corresp, var=wrapper_outputs, 
                           obs_list=obs$converted_obs_list,
                           transform_sim=transform_sim, transform_var=transform_var)

  crit_function <- function(sim_list, obs_list) {
    weight <- setNames(
      summary(sim_it2$sim_list, obs=obs_list, stats = c("RMSE"))$RMSE,
      nm=summary(sim_it2$sim_list, obs=obs_list, stats = c("RMSE"))$variable)
    alpha <- 1
    weight_it3 <<- unique(bind_rows(weight_it3,alpha*weight[names(weight)!="Date"]))
    crit_wls(sim_list, obs_list, 
             weight=weight,
             alpha=alpha)
  }
  
  res_it3 <- estim_param(obs_list=obs$converted_obs_list, 
                         crit_function = crit_function,
                         model_function=stics_wrapper,
                         model_options=model_options,
                         optim_options=optim_options,
                         param_info=final_param_info,
                         forced_param_values=final_forced_param_values, 
                         transform_var=transform_var,
                         transform_sim=transform_sim, var=wrapper_outputs)
  
  save(res_it1, igr, res_it2, res_it3, 
       file = file.path(out_dir,paste0("checkpoint_it3.Rdata")))
  
  complem_info$it3 <- list(forced_param_values=final_forced_param_values,
                           obsVar_used=varNames_corresp[varNames_corresp %in% unlist(lapply(obs$converted_obs_list,names))],
                           weight=weight_it3)
  save(complem_info, 
       file = file.path(out_dir,paste0("complementary_info.Rdata")))
  
}  
  
## Generating diagnostics and results files using CroPlotR
suffix <- NULL
if (test_case=="French") suffix <- paste0("_",variety) 
template_path <- file.path(here(),"data",paste0("cal_4_results_",test_case,suffix,"_numerical_modelName_contact_person.txt"))
post_treat(model_options, final_forced_param_values, res_it3,
           sitNames_corresp, wrapper_outputs, obs, 
           transform_sim, template_path, out_dir, test_case, variety)
