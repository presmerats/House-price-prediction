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

#### 2.1 - EDA (must be used in the report to explain our eda) ####
#basic.eda(data)


#### 2.2 - Create datasets to be used ####

raw_continuous_dataset <- raw_continuous_vars_selection(data)
save(file = "../Dataset/raw_continuous_dataset.Rda", raw_continuous_dataset)
rm(raw_continuous_dataset)

featureset_allmanual <- featureset.all(data)
save(file = "../Dataset/featureset_allmanual.Rda", featureset_allmanual)
rm(featureset_allmanual)

featureset_original_nooutliers <- featureset.original.nooutliers(data)
save(file = "../Dataset/featureset_original_nooutliers.Rda", featureset_original_nooutliers)
rm(featureset_original_nooutliers)

featureset_logs <- featureset.logs(data)
save(file = "../Dataset/featureset_logs.Rda", featureset_logs)
rm(featureset_logs)

featureset_ratios <- featureset.ratios(data)
save(file = "../Dataset/featureset_ratios.Rda", featureset_ratios)
rm(featureset_ratios)

featureset_logratios <- featureset.logratios(data)
save(file = "../Dataset/featureset_logratios.Rda", featureset_logratios)
rm(featureset_logratios)

featureset_nocorrelation01 <- featureset.nocorr.manual01(data)
save(file = "../Dataset/featureset_nocorrelation01.Rda", featureset_nocorrelation01)
rm(featureset_nocorrelation01)


featureset_nocorrelation02 <- featureset.nocorr.manual02(data)
save(file = "../Dataset/featureset_nocorrelation02.Rda", featureset_nocorrelation02)
rm(featureset_nocorrelation02)


featureset_nocorrelation03_logs <- featureset.nocorr.log.manual03(data)
save(file = "../Dataset/featureset_nocorrelation03_logs.Rda", featureset_nocorrelation03_logs)
rm(featureset_nocorrelation03_logs)


featureset_nocorrelation04_ratios <- featureset.nocorr.ratios.manual04(data)
save(file = "../Dataset/featureset_nocorrelation04_ratios.Rda", featureset_nocorrelation04_ratios)
rm(featureset_nocorrelation04_ratios)

#### 3 - Unsupervised analysis of the data ####

# test normality. 

# test-normality()

# perform PCA

pca_analysis()
pcr_model(data = data)
pca_analysis_2()


# perform clustering





#### 4 - Models fitting ####

load(file="../Dataset/raw_continuous_dataset.Rda")
linear_regression_fitting(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
rm(raw_continuous_vars)

load(file="../Dataset/raw_continuous_dataset.Rda")
mass.ridge(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
rm(raw_continuous_vars)

load(file="../Dataset/featureset_nocorrelation04_ratios.Rda")
mass.ridge(featureset_nocorrelation04_ratios, dataset_id = "featureset_nocorrelation04_ratios")
rm(featureset_nocorrelation04_ratios)


load(file="../Dataset/raw_continuous_dataset.Rda")
glmnet.ridge(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
rm(raw_continuous_vars)

load(file="../Dataset/raw_continuous_dataset.Rda")
glmnet.lasso(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
rm(raw_continuous_vars)




load(file="../Dataset/raw_continuous_dataset.Rda")
lars.lasso(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
rm(raw_continuous_vars)
