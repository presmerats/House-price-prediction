
linear_model_fitting_original_data <- function(data, output_results = "../Analysis Results/Linear Model Fitting/")
{
  # remove categorical vars
  categorical_vars <- c("id", "date", "waterfront", "view", "yr_built", "yr_renovated", "zipcode", "lat", "long")
  removed_vars <- names(data) %in% categorical_vars
  data <- data[,!removed_vars]
  
  lm_model <- lm(price ~ ., data=data)
  
  
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