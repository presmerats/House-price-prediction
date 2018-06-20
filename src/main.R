rm(list = ls())
gc()


#### 1 - Set up environment ####


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# source scripts

source("aux_functions.R")
source_scripts()
load_packages()
load.models.list()





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


featureset_base_ratios <- featureset.base.ratios.manual01(data)
save(file = "../Dataset/featureset_base_ratios.Rda", featureset_base_ratios)
rm(featureset_base_ratios)

# # this approach contains outliers
# raw_continuous_dataset <- raw_continuous_vars_selection(data)
# featureset_pca <- featureset_pca(raw_continuous_dataset)
# PCA this contains outliers too
load(file="../Dataset/raw_continuous_dataset.Rda")
featureset_pca <- featureset_pca(raw_continuous_dataset)
save(file = "../Dataset/featureset_pca.Rda", featureset_pca)
rm(featureset_pca)

# # without outliers but taking only continuous data inside
load(file="../Dataset/featureset_original_nooutliers.Rda")
featureset_pca <- featureset_pca2(featureset_original_nooutliers)
save(file = "../Dataset/featureset_pca_nooutliers.Rda", featureset_pca)
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

start_time = Sys.time()
regression_randomforest(raw_continuous_dataset, dataset_id = "raw_continuous_vars")
end_time = Sys.time()
print(end_time - start_time)
rm(raw_continuous_vars)

source("aux_functions.R")
source_scripts()
load_packages()

# ----- PCA extracted features --------------------------------------------#
load(file="../Dataset/featureset_pca.Rda")
linear_regression_fitting02(featureset_pca, dataset_id = "featureset_pca_normal")
mass.ridge(featureset_pca, dataset_id = "featureset_pca_normal")
glmnet.ridge(featureset_pca, dataset_id = "featureset_pca_normal")
glmnet.lasso(featureset_pca, dataset_id = "featureset_pca_normal")
lars.lasso(featureset_pca, dataset_id = "featureset_pca_normal")
#pcr_model(featureset_pca, dataset_id = "featureset_pca_normal")
regression_rpart_tree_fitting(featureset_pca, dataset_id = "featureset_pca_normal")
#classification_rpart_tree_fitting(featureset_pca, dataset_id = "featureset_pca_normal")
#regression_treelib_tree_fitting(featureset_pca, dataset_id = "featureset_pca_normal")
start_time = Sys.time()
regression_randomforest(featureset_pca, dataset_id = "featureset_pca_normal")
end_time = Sys.time()
print(end_time - start_time)
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




#### 5 - Experiments ####

### 5.1 - Option 3) PCA feature selection , model selection, feature selection ###

# select feature set based on analysis of the data set variables
# we select the continuous variables that are not correlated
# result: featureset_nocorrelation01

# perform model selection -> train all models over this data set, and select the one with smallest va error

#models.list <- c(linear_regression_fitting02,mass.ridge,glmnet.ridge, glmnet.lasso,lars.lasso,pcr_model)
load(file="../Dataset/featureset_nocorrelation01.Rda")
lapply(models.list, do.call, args=list(featureset_nocorrelation01, dataset_id = "O1_model_selection", comment="featureset_nocorrelation01"))
# result: ridge regression glmnet

# create.Latex.Table2(filein = "../Analysis Results/model_results.csv",
#                     variables = c("Model","Training.RMSE", "Validation.RMSE", "Testing.RMSE"),
#                     fileout  = "../Analysis Results/ap1_results.tex")
# 
# 
# create.Latex.Table2(filein = "../Analysis Results/model_results.csv",
#                     variables = c("Model","Training.RMSE", "Validation.RMSE", "Testing.RMSE","Training.NRMSE", "Validation.NRMSE", "Testing.NRMSE"),
#                     fileout  = "../Analysis Results/ap1_results_02.tex")

# create.Latex.Table2(filein = "../Analysis Results/model_results.csv",
#                     variables = c("Model","Validation.RMSE", "Testing.RMSE","Validation.NRMSE", "Testing.NRMSE"),
#                     fileout  = "../Analysis Results/ap1_results_03.tex")

source("aux_functions.R")
source_scripts()
load_packages()
create.Latex.Table4.sorted(fileout  = "../Analysis Results/ap1_results_04.tex")







### 5.2 - Option 2) for each model candidate feature selection ###

rm(list = ls())
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
source("aux_functions.R")
source_scripts()
load_packages()
load.models.list()

# Solution space exploration
lapply(models.list, model.selection.func, folder="../Dataset/",id="O2_feature_mode_exploration")
# result: lars lasso regression , featureset_allmanual



create.Latex.Table4.sorted(filein = "../Analysis Results/model_results.csv",
                    fileout  = "../Analysis Results/exp_models_vs_featuresets_all.tex")

### 5.3 - Option 3) PCA feature selection , model selection, feature selection ###

# perform PCA, -> select PC's, prepare data set
# this is already done in the featureset
load(file="../Dataset/featureset_pca.Rda")
#load(file="../Dataset/featureset_pca_nooutliers.Rda")

# perform model selection -> train all models over this data set, and select the one with smallest va error

lapply(models.list, do.call, args=list(featureset_pca, dataset_id = "O3_pca_model_selection", comment="featureset_pca"))

linear_regression_fitting02(featureset_pca, dataset_id = "O3_pca_model_selection")
mass.ridge(featureset_pca, dataset_id = "O3_pca_model_selection")
glmnet.ridge(featureset_pca, dataset_id = "O3_pca_model_selection")
glmnet.lasso(featureset_pca, dataset_id = "O3_pca_model_selection")
lars.lasso(featureset_pca, dataset_id = "O3_pca_model_selection")
#pcr_model(featureset_pca, dataset_id = "O3_pca_model_selection")

rm(featureset_pca)

# perform feature selection with this selected model
# we read the results file and we get the min validation NRMSE is  for the model
# minimum va NRMSE 202981.063059123 for glmnet ridge regression model

# result is feature and model


create.Latex.Table3(filein = "../Analysis Results/model_results.csv",
                    fileout  = "../Analysis Results/ap3_results_04.tex")

### 5.4 - Option 4) pick BASELINE MODEL (linear regre?); feature selection; model selection ###

# we select the baseline model
# result: linear regression

# perform feature selection with this baseline model model
#model.selection.func(linear_regression_fitting02,"../Dataset/","O4_feature_selection")
model.selection.func(mass.ridge,folder="../Dataset/",id="O4_feature_selection")
#result: featureset_nocorrelation04_ratios (with linear_regression_fitting)
#result: featureset_ratios (with mass ridge regression)

# perform model selection -> train all models over this data set, and select the one with smallest va error
load(file="../Dataset/featureset_ratios.Rda")
lapply(models.list, do.call, args=list(featureset_ratios, dataset_id = "O4_model_selection", comment="featureset_ratios"))
# result: mass ridge
load(file="../Dataset/featureset_nocorrelation04_ratios.Rda")
lapply(models.list, do.call, args=list(featureset_nocorrelation04_ratios, dataset_id = "O4_model_selection", comment="featureset_nocorrelation04_ratios"))
# result: lasso



create.Latex.Table3(filein = "../Analysis Results/model_results.csv",
                    fileout  = "../Analysis Results/ap4_results_04.tex")

create.Latex.Table3(filein = "../Analysis Results/model_results.csv",
                    fileout  = "../Analysis Results/ap5_results_04.tex")

featurespace = get.featureset.names("../Dataset/")

for(i in 1:length(featurespace)){
  #get clean name
  filename = paste("../Dataset/",featurespace[i],".Rda",sep="")
  newobjects = load(file=filename)
  auxvar <- get(newobjects[1])
  glmnet.ridge(auxvar, dataset_id = "O3_feature_selection", comment=featurespace[i])
  rm(auxvar)
}



### 5.5  All models vs all datasets
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
source("aux_functions.R")
source_scripts()
load_packages()
load.models.list()
# Solution space exploration
lapply(models.list, model.selection.func, folder="../Dataset/",id="O2_feature_mode_exploration")
# result: lars lasso regression , featureset_allmanual



create.Latex.Table4.sorted(filein = "../Analysis Results/model_results.csv",
                           fileout  = "../Analysis Results/exp_models_vs_featuresets_all.tex")


#create.Latex.Table5.sorted()


### 5.6 - Sequential Forward Selection ###

# featureset_glmnet_lasso_sfs = SFS(glmnet.lasso, featureset_base_ratios, dataset_id = "featureset_base_ratios", baseset = c(1:13), extra = c(14:26), method = "glmnet_lasso")
# save(file = "../Dataset/featureset_glmnet_lasso_sfs.Rda", featureset_glmnet_lasso_sfs)
# rm(featureset_glmnet_lasso_sfs)

load(file="../Dataset/featureset_logs.Rda")
featureset_regression_randomforest_sfs = SFS(regression_randomforest, featureset_logs, dataset_id = "featureset_logs", baseset = c(1,7,8), extra = c(13:20), method = "regression_randomforest")
save(file = "../Dataset/featureset_regression_randomforest_sfs.Rda", featureset_regression_randomforest_sfs)
#(1,7,8,13,16,17,20,21,24:35)
create.Latex.Table4(filein = "../Analysis Results/SFS/_regression_randomforest_featureset_logs_sfs.csv", fileout  = "../Analysis Results/SFS/_regression_randomforest_featureset_logs_sfs.tex")
rm(featureset_regression_randomforest_sfs)

load(file="../Dataset/featureset_nocorrelation03_logs.Rda")
featureset_regression_rpart_tree_fitting_sfs = SFS(regression_rpart_tree_fitting, featureset_nocorrelation03_logs, dataset_id = "featureset_nocorrelation03_logs", baseset = c(1:8), extra = c(9:13), method = "regression_rpart_tree_fitting")
save(file = "../Dataset/featureset_regression_rpart_tree_fitting_sfs.Rda", featureset_regression_rpart_tree_fitting_sfs)
#c(1,7,8,13,16,17,20,21,24:35)
create.Latex.Table4(filein = "../Analysis Results/SFS/_regression_rpart_tree_fitting_featureset_nocorrelation03_logs_sfs.csv", fileout  = "../Analysis Results/SFS/_regression_rpart_tree_fitting_featureset_nocorrelation03_logs_sfs.tex")
rm(featureset_regression_rpart_tree_fitting_sfs)


load(file="../Dataset/featureset_allmanual.Rda")
featureset_glmnet_lasso_sfs = SFS(glmnet.lasso, featureset_allmanual, dataset_id = "featureset_allmanual", baseset = c(1:8), extra = c(9:35), method = "glmnet_lasso")
save(file = "../Dataset/featureset_glmnet_lasso_sfs.Rda", featureset_glmnet_lasso_sfs)
#(1,7,8,13,16,17,20,21,24:35)
create.Latex.Table4(filein = "../Analysis Results/SFS/_glmnet_lasso_featureset_allmanual_sfs.csv", fileout  = "../Analysis Results/SFS/_glmnet_lasso_featureset_allmanual_logs_sfs.tex")
rm(featureset_glmnet_lasso_sfs)

### 5.7 fitting the final model ###







 
