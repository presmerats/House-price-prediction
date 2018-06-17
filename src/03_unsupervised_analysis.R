
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
  
  
  result <- c("\n\n******\n\n PCA_analysis result: several highly correlated variables, explaining the price. Recommended to identify latent factors and use them for prediction\n\n******\n\n ")
  write(result, filename, append = TRUE)

}

featureset_pca <- function(data){
  # the imput must be continuous data with no outliers
  pca_result <- PCA(data, quanti.sup = c(1), ncp=6)
  dev.off()
  # select num prin components -> 90% variance for 6
  # pca_result$eig
  
  # current features
  data_pca <- data.frame(
    cbind(target=data[,1],
      pca_result$ind$coord)
  )
  
  
  return(data_pca)
}



pcr_model <- function(output_results = "../Analysis Results/Unsupervised Analysis/", data)
{
  
  set.seed(2018)
  # remove categorical vars
  labels(data)
  
  categorical_vars <- c("id", "date", "waterfront", "view", "yr_built", "yr_renovated", "zipcode", "lat", "long")
  removed_vars <- names(data) %in% categorical_vars
  data <- data[,!removed_vars]
  

  train = round(9*nrow(data)/10)
  
  trainidx = 1:train
  testidx  = (train+1):nrow(data)
  
  pcr.fit = pcr(price~., data=data[trainidx,] ,scale=TRUE , validation ="CV")
  
  result.summery = capture.output(summary(pcr.fit))
  
  # We started with 11 variable and the pcr considered all of them
  # We leard that with 5 components we can explain 85% of the variance, and with 6 we will explain more than 90%.
  
  # lets plot the cross-validation MSE
  filename <- paste(output_results, "PCR_CV_MSE.jpeg", sep = "")
  jpeg(filename = filename, width = 1000, height=1000)
  validationplot(pcr.fit ,val.type="MSEP", main = "PCR cross-validation")
  dev.off()
  
  #predplot(pcr.fit)
  #coefplot(pcr.fit)
  #coef(pcr.fit, intercept=TRUE)
  
  filename <- paste(output_results, "results.txt", sep = "")
  
  result <- c("\n\n\ *******\n\n pcr_analysis result: As learned in PCA analysis several highly correlated variables, Using PCR we can see that with 6 PCs the model's explained variance is higher than 90%. Though, it is hard to analyze the importance of the variables.\n\n*****\n\n")
  
  write(result, filename, append=TRUE)
  write(result.summery, filename, append=TRUE)
  
  pcr.pred=predict(pcr.fit ,data[testidx ,-which(colnames(data)=="price")], ncomp = 6)
  RMSE = sqrt(mean((pcr.pred -data$price[testidx])^2))
  
  result.pred <- paste(c("\n\n\ *******\n\n The result of the prediction with PCR yield RMSE of "), RMSE, "\n\n\ *******\n\n" )
  write(result.pred, filename, append = TRUE)
}

