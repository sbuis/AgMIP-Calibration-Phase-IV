# AgMIP-Calibration-Phase-IV
Scripts and functions to apply the [AgMIP Calibration phase IV protocol (Wallach et al., 2024)](https://www.biorxiv.org/content/10.1101/2023.10.26.564162v2.full.pdf) to the datasets provided in the AgMIP Calibration phase IV exercises, using CroptimizR and CroPlotR R packages. 

This repository has been created to gather useful functions, scripts, examples and data for participants of the AgMIP Calibration Phase IV protocol who will use the software solution based on the [CroptimizR](https://github.com/SticsRPacks/CroptimizR) and [CroPlotR](https://github.com/SticsRPacks/CroPlotR) R packages with their own model. The features implemented in these functions and scripts are partly specific to the dataset used and will be integrated in a generic way (i.e. independent from the dataset used) into future versions of the CroptimizR package.

## What does it contain?

* AgMIP-Calibration-Phase-IV.Rproj: the R project. Open it in RStudio before working on your application.
* main_script.R: a template to be filled for the application of the AgMIP calibration phase IV protocol on your own model and configuration. To use it you need a model wrapper for the CroptimizR package for your model (refer to the [Guidelines for implementing a crop model R wrapper for CroptimizR](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).
* R folder: includes functions used in main_script.R
* data folder: includes observed data of the calibration dataset, description of associated units, examples and templates of protocol description EXCEL file and template of results files.

## What does it provide?

main_script.R automatically applies and chains the different steps of the AgMIP phase IV protocol. It automatically generates the results files and diagnostics required in the protocol.

## How to proceed?

* If to your knowledge there is no wrapper for your model for CroptimizR yet, please send an email to "samuel DOT buis AT inrae DOT fr" precising that you are interested in using CroptimizR for the AgMIP phase IV exercise and giving the name and version of your model (maybe some colleagues have already developped a wrapper or would like to, I can update you). Guidelines for implementing a crop model R wrapper for CroptimizR can be found [here](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)), and **it is also important to take into account the particular requirements listed in the following section**. Once it is developed, please test your wrapper using the test_wrapper function, as explained in the guidelines.

* If you have an R wrapper of your model for CroptimizR:

  * **Check that your model wrapper takes into account the particular requirements listed in the following section**. If not, update it.

  * Download the repository (green button "Code" => "Download zip"), **in a path whose folder names do not contain any space**, and unzip it.
  
  * Open the R project AgMIP-Calibration-Phase-IV in RStudio (using RStudio is mandatory for using the provided scripts).
  
  * Adapt the beginning of main_script.R to your case. Running this script requires (i) that you have created the input files needed to run your model on the given environments and (ii) that you have filled the protocol description EXCEL file.
  
  * In main_script.R, set `debug` variable to TRUE (see at the beginning of the script) and execute main_script.R in Rstudio => check if everything seems fine (e.g. no error + look at warning messages and results files - see next section for description ...). In case of error, or if you have any doubt, please send an email to "samuel DOT buis AT inrae DOT fr".
  
  * If everything seems to be OK, set  `debug` variable to FALSE and execute main_script.R in Rstudio ... this may take several days depending on the computation time needed by your model. Solutions for improving performances exist if needed (e.g. run situations in parallel in your wrapper, refer to the [wrapper implementation guidelines](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)). If there are errors, see following section "In case of errors".

## Specific requirements for the models and associated wrappers

* For this specific exercise, it is necessary for the model to provide simulated values until the last day of the harvest year (harvestYear/12/31). If this is not possible with your model, this can be handled in the model wrapper, replicating the simulated values at maturity up to harvestYear/12/31.

* In case some phenological stages used in the parameter estimation process (i.e. included as variables in the phenology variable group) are not reached before the end simulated day, it is also strongly advised to set their simulated value to the last day of the harvest year, within the model wrapper.

## In case of errors

Please check warning and error messages and follow the given instructions.

If you need assistance, please send an email to "samuel DOT buis AT inrae DOT fr" including the text of error and warning messages.

## Results

### Results files automatically generated

A set of files containing results is automatically generated (by default in folder results/test_case/variety, where test_case=="French" or "Australian"):

* tables of simulated results:

  * **simulations_default.txt**: the simulated values for both calibration and evaluation datasets for the variables required in the protocol before iteration1 (i.e. simulated values obtained using default values of the parameters)
  * **simulations_after_GROUP.txt**: the simulated values for both calibration and evaluation datasets for the variables required in the protocol after estimation using variable group GROUP in step 6
  * **simulations_after_step\#.txt**: the simulated values for both calibration and evaluation datasets for the variables required in the protocol after step number \# (6 and 7)

* scatter plots:
 
  * **scatterplots_default.pdf**: scatter plots of simulated VS observed values of the variables used on the calibration dataset before iteration1 (i.e. simulated values obtained using default values of the parameters)
  * **scatterplots_after_step\#.pdf**: scatter plots of simulated VS observed values of the variables used on the calibration dataset after step number \# (6 and 7)
  * **scatterplots_after_GROUP.pdf**: scatter plots of simulated VS observed values of the variables used on the calibration dataset after estimation using variable group GROUP in step 6

* **parameters.csv**: includes information on the estimated parameters (group, default value, estimated value after step 6, estimated value after step 7)
* **variables_CRITERION.csv**: include values of goodness-of-fit criteria (one file per criterion) computed on the calibration dataset from model simulations using default parameters values, estimated values of the parameters after step 6 and estimated values of the parameters after step 7.
* **summary_step\#.csv**: include information about the estimation procedure (names of the estimated parameters, number of starting values tested, ...), for step number \# 
* folders **step\#**: one folder per step (6 and 7) including the standard outputs generated by the CroptimizR package
* folder **DailyOutputs**: daily simulated values for each situation (one file per situation), using default values of the parameters (subfolder Default) and using estimated values of the parameters after steps number \# (subfolder step\#)  

If the code is run on synthetic experiments (see associated option at the beginning of main_script.R), the following additional files are generated:

* **synth_almost_additive_parameters_beta.csv** and **synth_candidate_parameters_beta.csv**: description of the parameters used in the synthetic experiments (extraction of tabs provided in protocol_descr_\*\*\*.xlsx, BUT with perturbed values for the default values of the parameters)
* **simulations_true_values.txt**:	the true values generated for both calibration and evaluation datasets for the variables required in the protocol
* **cal_4_obs_true_values_ModelName.txt**  and **cal_4_obs_noisy_values_SDnoise_ModelName.txt**:	the true and noisy values generated for the observations used

### Verification of results 

Once you have done the calibration, please look at the outputs to verify that the results seem reasonable. You can for example use the graphs in the scatterplots_\*\*\*.pdf files to check your choice for the order of variable groups, and your choice of major parameters.  The order of variable groups should be such that estimating parameters for one group has little or no effect on the fit to groups used previously. This should be visible from the graphs. For example, the graphs after estimation of the candidates for plant_N_content should show that the fit to variables previously treated should be about the same as before using plant_N_content. Also, the role of the major parameters is to approximately eliminate bias. This should appear in the graphs. For example, overall bias of variable Grain_Yield after estimating the major parameters for group yield, should be small compared to the bias using the default parameter values. 

### Results files to provide for AgMIP Calibration Phase IV exercise

Please copy the files simulations_\*\*\*.txt, variables_\*\*\*.csv, scatterplots_\*\*\*.pdf, parameters.csv, summary_step\*.csv and the folder DailyOutputs, in a single folder named “Model_yourName” (e.g.” STICS_Buis”) and upload the folder [here](https://uni-bonn.sciebo.de/s/1j1T5LdjZxTpe4J).

# References

Wallach, D., Buis, S., Seserman, D. M., Palosuo, T., Thorburn, P. J., Mielenz, H.,  Justes, E., Kersebaum, K-C, Dumont, B., Launay, M., Seidel, S. J. (2024). A calibration protocol for soil-crop models. Environmental Modelling & Software, 180, 106147.
