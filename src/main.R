rm(list = ls())
gc()


#### 1 - Set up environment ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# source scripts

source("aux_functions.R")

source_scripts()

load_packages()





#### 2 - Read data ####

data <- read.csv(file="../Dataset/kc_house_data.csv", header=TRUE, sep=",")
  
#### 2.1 - Create datasets to be used ####

raw_continuous_dataset <- raw_continuous_vars_selection(data)
save(file = "../Dataset/raw_continuous_dataset.Rda", raw_continuous_dataset)
rm(raw_continuous_dataset)

preprocessed.data01 <- basic.preprocessing(data)
#preprocessed.data01 <- basic.preprocessing.short(data)
#preprocessed.data01 <- basic.preprocessing.short2(data)
save(file = "../Dataset/preproceessed01.Rda", preprocessed.data01)
rm(preprocessed.data01)



#### 3 - Unsupervised analysis of the data ####

# test normality. 

# test-normality()

# perform PCA

pca_analysis()
pcr_model(data = data)
pca_analysis_2()


# perform clustering





#### 4 - Linear Models fitting ####

load(file="../Dataset/raw_continuous_dataset.Rda")
linear_regression_fitting(raw_continuous_dataset, dataset_id = "raw_continuous_vars")



