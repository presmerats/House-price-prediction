

load_packages <- function()
{
  require("FactoMineR")
  require("mvtnorm")
  require("MVN")
  require("mvnormtest")
}

source_scripts <- function()
{
  source("unsupervised_analysis.R")
  source("linear_regressions_fitting.R")
}