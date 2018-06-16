


load_packages <- function()
{
  require("FactoMineR")
  require("mvtnorm")
  require("MVN")
  require("mvnormtest")
  require("TunePareto") # for generateCVRuns()
  require("tree")
  require(chemometrics)
  
  
}

source_scripts <- function()
{
  source("01_eda.R")
  
  source("02_datapreparation_david.R")
  source("02_datapreparation_manual.R")
  
  source("03_unsupervised_analysis.R")
  
  source("04_linear_regressions_fitting.R")
  source("04_model_fitting_ridge_lasso_regression.R")
  source("04_model_ridge_lasso_functions.R")
  source("04_trees_methods_fitting.R")
  
  source("Cross_validation.R")
  
}