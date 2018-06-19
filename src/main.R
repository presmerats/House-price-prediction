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

# # this approach contains outliers
# raw_continuous_dataset <- raw_continuous_vars_selection(data)
# featureset_pca <- featureset_pca(raw_continuous_dataset)
# this contains outliers too
featureset_pca <- featureset_pca(data)
# # without outliers
# load(file="../Dataset/featureset_original_nooutliers.Rda")
# featureset_pca <- featureset_pca(featureset_original_nooutliers)
save(file = "../Dataset/featureset_pca.Rda", featureset_pca)
rm(featureset_pca)



#### 3 - Unsupervised analysis of the data ####

# test normality. 

# test-normality()

# perform PCA

pca_analysis()
pcr_model(data = data)
pca_analysis_2()


# perform clustering





#### 4 - Models fitting ####


# ----- continuous vars only --------------------------------------------#
load(file="../Dataset/raw_continuous_dataset.Rda")
linear_regression_fitting02(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
mass.ridge(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
glmnet.ridge(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
glmnet.lasso(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
lars.lasso(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
pcr_model(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
regression_rpart_tree_fitting(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
classification_rpart_tree_fitting(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
regression_treelib_tree_fitting(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
regression_randomforest(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
rm(raw_continuous_vars)


# ----- PCA extracted features --------------------------------------------#
load(file="../Dataset/featureset_pca.Rda")
linear_regression_fitting02(featureset_pca, dataset_id = "featureset_pca_normal")
mass.ridge(featureset_pca, dataset_id = "featureset_pca_normal")
glmnet.ridge(featureset_pca, dataset_id = "featureset_pca_normal")
glmnet.lasso(featureset_pca, dataset_id = "featureset_pca_normal")
lars.lasso(featureset_pca, dataset_id = "featureset_pca_normal")
#pcr_model(featureset_pca, dataset_id = "featureset_pca_normal")
regression_rpart_tree_fitting(featureset_pca, dataset_id = "featureset_pca_normal")
classification_rpart_tree_fitting(featureset_pca, dataset_id = "featureset_pca_normal")
regression_treelib_tree_fitting(featureset_pca, dataset_id = "featureset_pca_normal")
regression_randomforest(featureset_pca, dataset_id = "featureset_pca_normal")
rm(featureset_pca)
# load(file="../Dataset/featureset_pca.Rda")
# linear_regression_fitting02(featureset_pca, dataset_id = "featureset_pca_nooutliers")
# mass.ridge(featureset_pca, dataset_id = "featureset_pca_nooutliers")
# glmnet.ridge(featureset_pca, dataset_id = "featureset_pca_nooutliers")
# glmnet.lasso(featureset_pca, dataset_id = "featureset_pca_nooutliers")
# lars.lasso(featureset_pca, dataset_id = "featureset_pca_nooutliers")
# rm(featureset_pca)


# ----- logs --------------------------------------------#


# ----- ratios --------------------------------------------#


# ----- logs and ratios --------------------------------------------#


# ----- uncorrelated selection 1 --------------------------------------------#


# ----- uncorrelated selection 2 --------------------------------------------#


# ----- uncorrelated selection 3 --------------------------------------------#


# ----- uncorrelated selection 4 --------------------------------------------#
load(file="../Dataset/featureset_nocorrelation04_ratios.Rda")
mass.ridge(featureset_nocorrelation04_ratios, dataset_id = "featureset_nocorrelation04_ratios")
rm(featureset_nocorrelation04_ratios)




