# AgMIP-Calibration-Phase-IV
Scripts and functions to apply AgMIP Calibration phase IV protocol with CroptimizR and CroPlotR 

This repository has been created to gather useful functions, scripts, examples and possibly data for participants of the AgMIP Calibration Phase IV protocol who will use the [CroptimizR](https://github.com/SticsRPacks/CroptimizR) and [CroPlotR](https://github.com/SticsRPacks/CroPlotR) R packages. 

## What does it contain?

* AgMIP-Calibration-Phase-IV.Rproj, the R project. Open it in RStudio before working on your application.
* main_script.R, a template to be filled for the application of the AgMIP calibration phase IV protocol on your own model and configuration. To use it you need a model wrapper for the CroptimizR package for your model (refer to the [Guidelines for implementing a crop model R wrapper for CroptimizR](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).
* R folder: contains functions used in main_script.R
* wrapper folder: contains wrappers developed for AgMIP Calibration phase IV protocol.

## What does it provide?

main_script.R automatically applies and chains the different steps of the AgMIP phase IV protocol. It automatically generates the results files and diagnostics required in the protocol.

## How to proceed?

* If, from your knowledge, your model has not yet been interfaced with CroptimizR, please send an email to "samuel DOT buis AT inrae DOT fr" precising that you are interested in using CroptimizR for the AgMIP phase IV exercise and giving the name and version of your model (maybe some colleagues have already developped an interface for CroptimizR or would like to, I can update you). Some model wrappers developped in the frame of this exercise are provided in the wrapper folder as examples. Guidelines for implementing a crop model R wrapper for CroptimizR can be found [here](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).

* If your model has already been interfaced with CroptimizR:

  * download the repository (green button "Code" => "Download zip") and unzip it.
  
  * open the R project AgMIP-Calibration-Phase-IV in RStudio (using RStudio is mandatory for using the provided scripts)
  
  * install appropriate versions of CroptimizR and CroPlotR by executing the commands:
  
    * devtools::install_github("SticsRPacks/CroptimizR@AgMIP_phaseIV")
    * devtools::install_github("SticsRPacks/CroPlotR@*release")
  
  * define the protocol_descr_****.xls files as indicated in the word document sent by the co-leaders
  
  * Check that your model wrapper is able to provide the simulated variables you listed in the protocol description files.
  
  * adapt beginning of main_script.R to your case
  
  * in main_script.R, set debug variable to TRUE (see at the beginning of the script) and execute main_script.R in Rstudio => check if everything seems fine (e.g., no error + look at warning messages ...)
  
  * when everything seems to be OK, set debug variable to FALSE and execute main_script.R in Rstudio ... this may take several days depending on the computation time needed by your model. Solutions for improving performances exist if needed (e.g. run situations in parallel in your wrapper, refer to the [wrapper implementation guidelines](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).
 
## Results

A set of files containing results is automatically generated:

* **protocol_descr_\*\*\*.xlsx**: description of the protocol as given by the user (in the case of synthetic experiment, default values given in this file are used as TRUE values for the parameters)
* **cal_4_results_Australian_numerical_ModelName.txt**:	the simulated values for both calibration and evaluation datasets for the variables required in the protocol
* **scatterPlots_default.pdf**: scatter plots of simulated VS observed values of the variables used on the calibration dataset before iteration1 (i.e. simulated values obtained using default values of the parameters)
* **scatterPlots_it\#.pdf** (one file per iteration): scatter plots of simulated VS observed values of the variables used on the calibration dataset after the given iteration
* **Table_parameters_iteration\#.csv** (one file per iteration), includes information on the estimaetd parameters (name, default value, selected initial value, estimated value), for the given iteration and for each estimated parameter
* **Table_variables_iteration\#.csv** (one file per iteration), includes values of goodness-of-fit criteria computed using default and final estimated values of the parameters on the calibration dataset, for each observed variable used
* **Table_steps_iteration\#.csv** (one file per iteration), includes information about the estimation procedure (names of the estimated parameters, number of starting values tested, ...), for the given iteration and for each step within the iteration
* folders **Iteration\#**: one folder per iteration including the standard outputs generated by the CroptimizR package

If the code is run on synthetic experiments (see associated option at the beginning of main_script.R), the following additional files are generated:

* **synth_almost_additive_parameters_beta.csv** and **synth_candidate_parameters_beta.csv**: description of the parameters used in the synthetic experiments (extraction of tabs provided in protocol_descr_\*\*\*.xlsx, BUT with perturbed values for the default values of the parameters)
* **cal_4_results_Australian_true_values_ModelName.txt**:	the true values generated for both calibration and evaluation datasets for the variables required in the protocol
* **cal_4_obs_Australian_true_values_ModelName.txt**  and **cal_4_obs_Australian_noisy_values_SDnoise_ModelName.txt**:	the true and noisy values generated for the observations used



