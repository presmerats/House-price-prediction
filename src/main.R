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

featureset.allmanual <- featureset.all(data)
save(file = "../Dataset/featureset_allmanual.Rda", featureset.allmanual)
rm(featureset.allmanual)

featureset.original.nooutliers <- featureset.original.nooutliers(data)
save(file = "../Dataset/featureset_original_nooutliers.Rda", featureset.original.nooutliers)
rm(featureset.original.nooutliers)


featureset.logs <- featureset.logs(data)
save(file = "../Dataset/featureset_logs.Rda", featureset.logs)
rm(featureset.logs)

featureset.ratios <- featureset.ratios(data)
save(file = "../Dataset/featureset_ratios.Rda", featureset.ratios)
rm(featureset.ratios)


featureset.logratios <- featureset.logratios(data)
save(file = "../Dataset/featureset_logratios.Rda", featureset.logratios)
rm(featureset.logratios)


featureset.nocorrelation <- featureset.nocorr.manual01(data)
save(file = "../Dataset/featureset_nocorrelation01.Rda", featureset.nocorrelation)
rm(featureset.nocorrelation)


featureset.nocorrelation02 <- featureset.nocorr.manual02(data)
save(file = "../Dataset/featureset_nocorrelation02.Rda", featureset.nocorrelation02)
rm(featureset.nocorrelation02)


featureset.nocorrelation03.logs <- featureset.nocorr.log.manual03(data)
save(file = "../Dataset/featureset_nocorrelation03_logs.Rda", featureset.nocorrelation03.logs)
rm(featureset.nocorrelation03.logs)


featureset.nocorrelation04.ratios <- featureset.nocorr.ratios.manual04(data)
save(file = "../Dataset/featureset_nocorrelation04_ratios.Rda", featureset.nocorrelation04.ratios)
rm(featureset.nocorrelation04.ratios)

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

load(file="../Dataset/raw_continuous_dataset.Rda")
mass.ridge(raw_continuous_dataset, dataset_id = "raw_continuous_vars")



