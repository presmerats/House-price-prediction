

load_packages <- function()
{
  require("FactoMineR")
  require("mvtnorm")
  require("MVN")
  require("mvnormtest")
  require("TunePareto") # for generateCVRuns()
  
}

source_scripts <- function()
{
  source("unsupervised_analysis.R")
  source("linear_regressions_fitting.R")
  source("Cross_validation.R")
  source("01_datapreparation_david.R")
}