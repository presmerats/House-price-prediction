
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
  load_install_packages("lars")
  load_install_packages("glmnet")
  load_install_packages("xtable")
  
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
  source("04_pcr_fitting.R")
  
  source("Cross_validation.R")
  source("Test_error.R")
  
  
}

# 2. Utils--------------------
get.featureset.names <- function(folderpath){
  #file.list <- dir(folderpath)
  file.list <- Filter(function(x){substring(x,nchar(x)-3,nchar(x))==".Rda"},dir(folderpath))
  return(lapply(file.list,function(x){gsub(".Rda","",x)}))
}


create.Latex.Table <- function(){
  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  
  results = read.csv("../Analysis Results/model_results.csv", header = TRUE, sep = ";")
  
  variables <- c("Input", "Model","Training.NRMSE", "Validation.NRMSE", "Testing.NRMSE")
  subset = results[,variables]
  
  subset = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  
  colnames(subset) <- c("Model","Feature set","Training.NRMSE", "Validation.NRMSE", "Testing.NRMSE")
  
  tli.table <- xtable(subset)
  align(tli.table) <- rep("c", ncol(tli.table)+1)
  
  print(tli.table, file = "../Analysis Results/model_results.tex")
}