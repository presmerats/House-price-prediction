
linear_regression_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Linear Model Fitting/", comment = "testing")
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
  comment <- comment
  Input <- dataset_id
  Model <- "simple linear regression"
  Trainin_error_CV <- round(linear_regreesion.10x10.CV[1],2)
  Testing_error_CV <- round(linear_regreesion.10x10.CV[2],2)
  
  result <- cbind(function_script, comment, Input, Model, Trainin_error_CV, Testing_error_CV)
  
  write.table(result, file="../Analysis Results/model_results.csv", append = TRUE, sep=";", col.names = FALSE, row.names = FALSE)
  

}



linear_regression_fitting02 <- function(data, dataset_id, output_results = "../Analysis Results/Linear Model Fitting/", comment = "testing")
{
  
  # in each data frame, 
  # assume the target is in column 1, and target is called data$target
  # assume the features are in columns 2:ncol(df1)  <- no feature selection here!
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]]
  
  # Cross validation for model selection (no hyperparamteres here!)
  my_lr_tr <- lm(target ~ ., data=train)
  # refit
  # no need to refit, no hyperparameters
  
  # training, validation and test errors
  tr.pred <- predict(my_lr_tr, data = train)
  tr.se <- 0.5*sum((tr.pred - train$target)^2)
  tr.MSE <- mean((tr.pred - train$target)^2)
  tr.NRMSE <- sqrt(tr.MSE)  
  
  # Validation error (we just compute it for later model selection)
  valist <- linear.regression.CV(10,train)
  va.se <- valist[1]
  va.MSE <- valist[2]
  va.NRMSE <- valist[3]
  
  # generalisation error
  te.pred <- predict.lm(my_lr_tr, newdata  = test)
  te.se <- 0.5*sum((te.pred - test$target)^2)
  te.MSE <- mean((te.pred - test$target)^2)
  te.NRMSE <- sqrt(te.MSE)
  
  # write results to results file.  
  function_script <- "linear_regressions_fitting"
  comment <- comment
  Input <- dataset_id
  Model <- "simple linear regression"
  Training_error <- tr.se
  Training_MSE <- tr.MSE
  Training_NRMSE <- tr.NRMSE
  Validation_error <- va.se
  Validation_MSE <- va.MSE
  Validation_NRMSE <- va.NRMSE
  Testing_error <- te.se
  Testing_MSE <- te.MSE
  Testing_NRMSE <- te.NRMSE
  
  result <- cbind(
    function_script, 
    comment, 
    Input, 
    Model, 
    Training_error,
    Training_MSE,
    Training_NRMSE,
    Validation_error, 
    Validation_MSE,
    Validation_NRMSE,
    Testing_error,
    Testing_MSE,
    Testing_NRMSE)
  
  write.table(
    result, 
    file="../Analysis Results/model_results.csv", 
    append = TRUE, 
    sep=";", 
    col.names = FALSE, 
    row.names = FALSE)

}



linear.regression.CV <- function (k,data)
{
  CV.folds <- generateCVRuns(data$target, ntimes=1, nfold=k, stratified=TRUE)
  
  thenames <- c("k","fold","TR error", "TR MSE", "TR NRMSE","VA error","VA MSE","VA NRMSE")
  cv.results <- matrix (rep(0,length(thenames)*k),nrow=k)
  colnames (cv.results) <- thenames
  
  cv.results[,"TR error"] <- 0
  cv.results[,"VA error"] <- 0
  cv.results[,"TR MSE"] <- 0
  cv.results[,"VA MSE"] <- 0
  cv.results[,"TR NRMSE"] <- 0
  cv.results[,"VA NRMSE"] <- 0
  cv.results[,"k"] <- k
  
  for (j in 1:k)
  {
    # get VA data
    va <- unlist(CV.folds[[1]][[j]])
    train = data[-va,]
    validation = data[va,]
    
    # fit model
    my_lr_tr <- lm(target ~ ., data=train)
    
    # predict TR data
    tr.pred <- predict(my_lr_tr, data = train)
    tr.se <- 0.5*sum((tr.pred - train$target)^2)
    tr.MSE <- mean((tr.pred - train$target)^2)
    tr.NRMSE <- sqrt(tr.MSE) 
    cv.results[j,"TR error"]  <- tr.se
    cv.results[j,"TR MSE"]  <- tr.MSE
    cv.results[j,"TR NRMSE"]  <- tr.NRMSE
    
    # predict VA data
    cv.pred <- predict(my_lr_tr, data = validation)
    cv.se <- 0.5*sum((cv.pred - validation$target)^2)
    cv.MSE <- mean((cv.pred - validation$target)^2)
    cv.NRMSE <- sqrt(cv.MSE) 
    
    cv.results[j,"VA error"] <- cv.se
    cv.results[j,"VA MSE"] <- cv.MSE
    cv.results[j,"VA NRMSE"] <- cv.NRMSE
    
    cv.results[j,"fold"] <- j
  }
  
  va.se.mean <- mean(cv.results[,"VA error"])
  va.MSE.mean <- mean(cv.results[,"VA MSE"])
  va.NRMSE.mean <- mean(cv.results[,"VA NRMSE"])
  return(c(va.se.mean, va.MSE.mean, va.NRMSE.mean))
  # return everything: mean training error, mean va error?
}

