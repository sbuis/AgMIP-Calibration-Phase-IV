# AgMIP-Calibration-Phase-IV
Scripts and functions to apply AgMIP Calibration phase IV protocol with CroptimizR and CroPlotR 

This repository has been created to gather useful functions, scripts, examples and possibly data for participants of the AgMIP Calibration Phase IV protocol who will use the [CroptimizR](https://github.com/SticsRPacks/CroptimizR) and [CroPlotR](https://github.com/SticsRPacks/CroPlotR) R packages. 

What does it contain?

* AgMIP-Calibration-Phase-IV.Rproj, the R project. Open it in RStudio before working on your application.
* main_script.R, a template to be filled for the application of the AgMIP calibration phase IV protocol on your own model and configuration. To use it you need a model wrapper for the CroptimizR package for your model (refer to the [Guidelines for implementing a crop model R wrapper for CroptimizR](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).
* R folder: contains functions used in main_script.R
* wrapper folder: contains wrappers developed for AgMIP Calibration phase IV protocol.

What does it provide?

main_script.R automatically applies and chains the different steps of the AgMIP phase IV protocol. It automatically generates the results files and diagnostics required in the protocol.

How to proceed?

* If, from your knowledge, your model has not yet been interfaced with CroptimizR, please send an email to "samuel DOT buis AT inrae DOT fr" precising that you are interested in using CroptimizR for the AgMIP phase IV exercise and giving the name and version of your model (maybe some colleagues have already developped an interface for CroptimizR or would like to, I can update you). Some model wrappers developped in the frame of this exercise are provided in the wrapper folder as examples. Guidelines for implementing a crop model R wrapper for CroptimizR can be found [here](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).

* If your model has already been interfaced with CroptimizR:

  * download the repository (green button "Code")
  
  * adapt main_script.R to your case
  
  * execute main_script.R in Rstudio by setting maxeval to a low value (see comment in main_script.R) and check if everything seems fine (e.g., no error nor warning messages ...)
  
  * execute main_script.R in Rstudio setting maxeval to a high value (>500) ... this may take some hours or days depending on the computation time needed by your model. Solutions for improving performances exist if needed (e.g. run situations in parallel in your wrapper, refer to the wrapper implementation guidelines).
 