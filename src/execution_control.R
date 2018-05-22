rm(list = ls())
gc()


#### 1 - Set up environment ####

a = Sys.info()

david_wd <- "~/MIRI-Data Science/ML - Machine Learning/Project/mlproject/src"
asaf_wd <- ""
pau_wd <- ""

if (a["user"] == "david") {
  wd <- david_wd
} else if (a["user"] == "pau") {
  wd <- pau_wd
} else {
  wd <- asaf_wd
}
setwd(wd)

# source scripts

source("aux_functions.R")

source_scripts()

load_packages()


source(main_datasets_generator.R)
source(main_unsupervised_analysis.R)
source(main_experiments.R)