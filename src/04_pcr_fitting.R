pcr_model <- function(data, dataset_id, output_results = "../Analysis Results/PCR/", comment = "testing")
{
  
  set.seed(2018)
  # remove categorical vars
  # labels(data)
  categorical_vars <- c("id", "date", "waterfront", "view", "yr_built", "yr_renovated", "zipcode", "lat", "long")
  removed_vars <- names(data) %in% categorical_vars
  data <- data[,!removed_vars]
  
  # in each data frame, 
  # assume the target is in column 1, and target is called data$target
  # assume the features are in columns 2:ncol(df1)  <- no feature selection here!
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]]
  
  # model fit
  pcr.fit = pcr(target ~., data=train ,scale=TRUE , validation ="CV")
  
  # Cross validation
  # it is done internally, so later we recompute it for global model selection
  
  # refit
  # so already done
  
  # training, validation and test errors
  tr.pred=predict(pcr.fit ,train[ ,-which(colnames(data)=="target")], ncomp = 6)
  tr.se <- 0.5*sum((tr.pred - train$target)^2)
  tr.MSE <- mean((tr.pred - train$target)^2)
  tr.NRMSE <- sqrt(tr.MSE)  
  
  # Validation error
  valist <- pcr.CV(10,train,pcr.fit, 6)
  va.se <- valist[1]
  va.MSE <- valist[2]
  va.NRMSE <- valist[3]
  
  # generalisation error
  te.pred=predict(pcr.fit ,test[ ,-which(colnames(data)=="target")], ncomp = 6)
  te.se <- 0.5*sum((te.pred - test$target)^2)
  te.MSE <- mean((te.pred - test$target)^2)
  te.NRMSE <- sqrt(te.MSE) 
  
  # write results to results file.  
  
  function_script <- "pcr_regression"
  comment <- comment
  Input <- dataset_id
  Model <- "PCR"
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


pcr.CV <- function (k,data,pcr.fit,ncomponents)
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
    test = data[va,]
    
    # train on TR data
    # no need to train again the model 
    
    # predict TR 
    tr.pred=predict(pcr.fit ,train[ ,-which(colnames(data)=="target")], ncomp = ncomponents)
    tr.se <- 0.5*sum((tr.pred - train$target)^2)
    tr.MSE <- mean((tr.pred - train$target)^2)
    tr.NRMSE <- sqrt(tr.MSE)  
    cv.results[j,"TR error"]  <- tr.se
    cv.results[j,"TR MSE"]  <- tr.MSE
    cv.results[j,"TR NRMSE"]  <- tr.NRMSE
    
    # Validation error
    cv.pred=predict(pcr.fit ,test[ ,-which(colnames(data)=="target")], ncomp = ncomponents)
    cv.se <- 0.5*sum((cv.pred - test$target)^2)
    cv.MSE <- mean((cv.pred - test$target)^2)
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

