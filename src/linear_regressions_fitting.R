
linear_model_fitting_original_data <- function(data, output_results = "../Analysis Results/Linear Model Fitting/")
{
  
  
  browser()

  # remove categorical vars

  
  linear_regreesion.10x10.CV <- mean(replicate(10,MODEL.CV.OVER.Ks(data,10,"linear_regression")))
  
  
  RSS <- sum(residuals(lm_model)^2)
  RSE <- sqrt(RSS/nrow(data)) # average error made on a prediction. 
  mean_price <- mean(data$price)
  TSS <- sum((data$price - mean_price)^2)
  R_sqared <- (TSS - RSS)/TSS 
  
  
    
  ## Test assumptions

  lmtest::dwtest(lm_model, alternative="two.sided") # Independance among all the samples - Durbin Watson Test.
  shapiro.test(residuals(lm_model)[1:4990]) # All populations are normal - Saphiro Test
  lmtest::bgtest(lm_model) # Equal variances - Homogeneity of variances. 
  
   
  filename <- paste(output_results, "Results.txt", sep = "")
  
  fileConn<-file(filename)
  result <- paste("lm_fitting_continuous_original_variables_result: R-squared:", round(R_sqared, 2), sep="")
  writeLines(result, fileConn)
  close(fileConn)
  
}