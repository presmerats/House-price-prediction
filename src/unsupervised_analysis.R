
normality_tests <- function(data)
{
  # remove categorical vars
  categorical_vars <- c("id", "date", "waterfront", "view", "yr_built", "yr_renovated", "zipcode", "lat", "long")
  removed_vars <- names(data) %in% categorical_vars
  data <- data[,!removed_vars]
  
  # test multivariate normality
  mshapiro.test(as.matrix(data[1:5000,]))
  
  
}

## 

pca_analysis <- function(output_results = "../Analysis Results/Unsupervised Analysis/", data)
{
  # remove categorical vars
  categorical_vars <- c("id", "date", "waterfront", "view", "yr_built", "yr_renovated", "zipcode", "lat", "long")
  removed_vars <- names(data) %in% categorical_vars
  data <- data[,!removed_vars]
  
  
  filename <- paste(output_results, "PCA_variables_projection_first_factorial_plane.jpeg", sep = "")
  
  jpeg(filename = filename, width = 1000, height=1000)
  pca_result <- PCA(data, quanti.sup = c(1))
  dev.off()
  
  
  filename <- paste(output_results, "results.txt", sep = "")
  
  fileConn<-file(filename)
  result <- c("PCA_analysis result: several highly correlated variables, explaining the price. Recommended to identify latent factors and use them for prediction")
  writeLines(result, fileConn)
  close(fileConn)
  
}


