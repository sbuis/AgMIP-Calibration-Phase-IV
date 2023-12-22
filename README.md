# AgMIP-Calibration-Phase-IV
Scripts and functions to apply [AgMIP Calibration phase IV protocol](https://www.biorxiv.org/content/10.1101/2023.10.26.564162v2.full.pdf) with CroptimizR and CroPlotR. 

This repository has been created to gather useful functions, scripts, examples and data for participants of the AgMIP Calibration Phase IV protocol who will use the software solution based on the [CroptimizR](https://github.com/SticsRPacks/CroptimizR) and [CroPlotR](https://github.com/SticsRPacks/CroPlotR) R packages. 

## What does it contain?

* AgMIP-Calibration-Phase-IV.Rproj: the R project. Open it in RStudio before working on your application.
* main_script.R: a template to be filled for the application of the AgMIP calibration phase IV protocol on your own model and configuration. To use it you need a model wrapper for the CroptimizR package for your model (refer to the [Guidelines for implementing a crop model R wrapper for CroptimizR](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).
* R folder: includes functions used in main_script.R
* data folder: includes observed data of the calibration dataset, description of associated units, examples and templates of protocol description EXCEL file and template of results files.

## What does it provide?

main_script.R automatically applies and chains the different steps of the AgMIP phase IV protocol. It automatically generates the results files and diagnostics required in the protocol.

## How to proceed?

* If to your knowledge there is no wrapper for your model for CroptimizR yet, please send an email to "samuel DOT buis AT inrae DOT fr" precising that you are interested in using CroptimizR for the AgMIP phase IV exercise and giving the name and version of your model (maybe some colleagues have already developped a wrapper or would like to, I can update you). Guidelines for implementing a crop model R wrapper for CroptimizR can be found [here](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)). Once it is developed, please test your wrapper using the test_wrapper function, as explained in the guidelines.

* If you have an R wrapper of your model for CroptimizR:

  * Download the repository (green button "Code" => "Download zip"), **in a path whose folder names do not contain any space**, and unzip it.
  
  * Open the R project AgMIP-Calibration-Phase-IV in RStudio (using RStudio is mandatory for using the provided scripts).
  
  * Adapt the beginning of main_script.R to your case.
  
  * In main_script.R, set debug variable to TRUE (see at the beginning of the script) and execute main_script.R in Rstudio => check if everything seems fine (e.g. no error + look at warning messages and results files - see next section for description ...)
  
  * When everything seems to be OK, set debug variable to FALSE and execute main_script.R in Rstudio ... this may take several days depending on the computation time needed by your model. Solutions for improving performances exist if needed (e.g. run situations in parallel in your wrapper, refer to the [wrapper implementation guidelines](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).
 
## Results

A set of files containing results is automatically generated (by default in folder results/test_case/variety, where test_case=="French" or "Australian"):

* **cal_4_results_test_case_cultivar_default_values_ModelName.txt**:	the simulated values for both calibration and evaluation datasets for the variables required in the protocol before iteration1 (i.e. simulated values obtained using default values of the parameters)
* **cal_4_results_test_case_cultivar_numerical_it\#_ModelName.txt**:	the simulated values for both calibration and evaluation datasets for the variables required in the protocol after iteration number \#
* **scatterPlots_default.pdf**: scatter plots of simulated VS observed values of the variables used on the calibration dataset before iteration1 (i.e. simulated values obtained using default values of the parameters)
* **scatterPlots_it\#.pdf** (one file per iteration): scatter plots of simulated VS observed values of the variables used on the calibration dataset after iteration number \#
* **Table_parameters_iteration\#.csv** (one file per iteration), includes information on the estimaetd parameters (name, default value, selected initial value, estimated value), for the given iteration and for each estimated parameter
* **Table_variables_iteration\#.csv** (one file per iteration), includes values of goodness-of-fit criteria computed using default and final estimated values of the parameters on the calibration dataset, for each observed variable used
* **Table_steps_iteration\#.csv** (one file per iteration), includes information about the estimation procedure (names of the estimated parameters, number of starting values tested, ...), for the given iteration and for each step within the iteration
* folders **Iteration\#**: one folder per iteration including the standard outputs generated by the CroptimizR package

If the code is run on synthetic experiments (see associated option at the beginning of main_script.R), the following additional files are generated:

* **synth_almost_additive_parameters_beta.csv** and **synth_candidate_parameters_beta.csv**: description of the parameters used in the synthetic experiments (extraction of tabs provided in protocol_descr_\*\*\*.xlsx, BUT with perturbed values for the default values of the parameters)
* **cal_4_results_test_case_true_values_ModelName.txt**:	the true values generated for both calibration and evaluation datasets for the variables required in the protocol
* **cal_4_obs_test_case_true_values_ModelName.txt**  and **cal_4_obs_test_case_noisy_values_SDnoise_ModelName.txt**:	the true and noisy values generated for the observations used



