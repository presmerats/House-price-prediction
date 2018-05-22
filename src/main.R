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





#### 2 - Read data ####

if (a[1] == "Linux"){
  # Linux reading file
  data <- read.csv(file="../Dataset/kc_house_data.csv", header=TRUE, sep=",")
  
} else {
  # windows reading file
  data <- read.csv(file="../Dataset/kc_house_data.csv", header=TRUE, sep=",")
}

#### 2.1 - Create datasets to be used ####

raw_continuous_dataset <- raw_continuous_vars_selection(data)
save(file = "../Dataset/raw_continuous_dataset.Rda", raw_continuous_dataset)
rm(raw_continuous_dataset)

#### 3 - Unsupervised analysis of the data ####

# test normality. 

# test-normality()

# perform PCA

pca_analysis()
pca_analysis_2()


# perform clustering





#### 4 - Linear Models fitting ####

load(file="../Dataset/raw_continuous_dataset.Rda")
linear_regression_fitting(raw_continuous_dataset, dataset_id = "raw_continuous_vars")

