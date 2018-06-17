
load_install_packages <- function(packageName)
{
  if(!require(packageName, character.only=TRUE)){
    print(paste(packageName, "not exist, installation in process.." ))
    install.packages(packageName)
    if(!require(packageName, character.only=TRUE))
      print(paste("failed to install:", packageName ))
  }
}

load_packages <- function()
{
  load_install_packages("FactoMineR")
  load_install_packages("pls")
  load_install_packages("mvtnorm")
  load_install_packages("MVN")
  load_install_packages("mvnormtest")
  load_install_packages("TunePareto") # for generateCVRuns()
  load_install_packages("tree")
  load_install_packages("chemometrics")
  load_install_packages("MASS")
  load_install_packages("corrplot")
  load_install_packages("randomForest")
  load_install_packages("rpart")
  load_install_packages("rpart.plot")
  #load_install_packages("rattle")
  load_install_packages("lars")
  load_install_packages("glmnet")
  load_install_packages("corrplot")
  
}

source_scripts <- function()
{
  source("01_eda.R")
  
  source("02_datapreparation_david.R")
  source("02_datapreparation_manual.R")
  
  source("03_unsupervised_analysis.R")
  
  source("04_linear_regressions_fitting.R")
  source("04_ridge_regression_fitting_methods.R")
  source("04_lasso_regression_fitting_methods.R")
  source("04_trees_methods_fitting.R")
  
  source("Cross_validation.R")
  source("Test_error.R")
  
  
}