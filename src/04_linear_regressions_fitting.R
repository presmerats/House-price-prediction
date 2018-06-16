
linear_regression_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Linear Model Fitting/")
{
  
  
  linear_regreesion.10x10.CV <- MODEL.CV.OVER.Ks(data,10,"linear_regression")
  
  browser()
    
  ## Test assumptions
  # # test assumptions, return invalid results if not passed. 
  # lmtest::dwtest(my_lr_tr, alternative="two.sided") # Independance among all the samples - Durbin Watson Test.
  # shapiro.test(residuals(my_lr_tr)[1:4990]) # All populations are normal - Saphiro Test
  # lmtest::bgtest(my_lr_tr) # Equal variances - Homogeneity of variances. 
  # 
  
  # write results to results file.  
  
  function_script <- "linear_regressions_fitting"
  comment <- "testing the workflow"
  Input <- dataset_id
  Model <- "simple linear regression"
  Trainin_error_CV <- round(linear_regreesion.10x10.CV[1],2)
  Testing_error_CV <- round(linear_regreesion.10x10.CV[2],2)
  
  result <- cbind(function_script, comment, Input, Model, Trainin_error_CV, Testing_error_CV)
  
  write.table(result, file="../Analysis Results/model_results.csv", append = TRUE, sep=";", col.names = FALSE, row.names = FALSE)
  

}