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
  error = Prediction.errors(tr.pred,train$target)
  tr.se <- error[["se"]]
  tr.MSE <-  error[["mse"]]
  tr.RMSE <-  error[["rmse"]]  
  tr.NRMSE <-  error[["nrmse"]]  
  tr.R2 <-  error[["r2"]] 
  
  # Validation error
  valist <- pcr.CV(10,train,pcr.fit, 6)
  va.se <- valist[["se"]]
  va.MSE <- valist[["mse"]]
  va.RMSE <- valist[["rmse"]]
  va.NRMSE <- valist[["nrmse"]]
  va.R2 <- valist[["r2"]]
  
  # generalisation error
  te.pred=predict(pcr.fit ,test[ ,-which(colnames(data)=="target")], ncomp = 6)
  error = Prediction.errors(te.pred,test$target)
  te.se <- error[["se"]]
  te.MSE <-  error[["mse"]]
  te.RMSE <-  error[["rmse"]]  
  te.NRMSE <-  error[["nrmse"]]  
  te.R2 <-  error[["r2"]]
  
  # write results to results file.  
  
  function_script <- "pcr_regression"
  comment <- comment
  Input <- dataset_id
  Model <- "PCR"
  
  result <- cbind(
    function_script, 
    comment, Input, Model, 
    tr.se, tr.MSE, tr.RMSE, tr.NRMSE, tr.R2,
    va.se, va.MSE, va.RMSE, va.NRMSE, va.R2,
    te.se, te.MSE, te.RMSE, te.NRMSE, te.R2
  )
  
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
  
  thenames <- c("k","fold","TR error", "TR MSE", "TR NRMSE","VA error","VA MSE","VA RMSE","VA NRMSE","VA R2")
  cv.results <- matrix (rep(0,length(thenames)*k),nrow=k)
  colnames (cv.results) <- thenames
  
  cv.results[,"TR error"] <- 0
  cv.results[,"TR MSE"] <- 0
  cv.results[,"TR NRMSE"] <- 0
  
  cv.results[,"VA error"] <- 0
  cv.results[,"VA MSE"] <- 0
  cv.results[,"VA RMSE"] <- 0
  cv.results[,"VA NRMSE"] <- 0
  cv.results[,"VA R2"] <- 0
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
    # tr.se <- 0.5*sum((tr.pred - train$target)^2)
    # tr.MSE <- mean((tr.pred - train$target)^2)
    # tr.NRMSE <- sqrt(tr.MSE)  
    # cv.results[j,"TR error"]  <- tr.se
    # cv.results[j,"TR MSE"]  <- tr.MSE
    # cv.results[j,"TR NRMSE"]  <- tr.NRMSE
    
    # Validation error
    cv.pred=predict(pcr.fit ,test[ ,-which(colnames(data)=="target")], ncomp = ncomponents)
    error = Prediction.errors(cv.pred,test$target)
    cv.results[j,"VA error"] <- error[["se"]]
    cv.results[j,"VA MSE"] <- error[["mse"]]
    cv.results[j,"VA RMSE"] <- error[["rmse"]] 
    cv.results[j,"VA NRMSE"] <- error[["nrmse"]] 
    cv.results[j,"VA R2"] <- error[["r2"]] 
    
    cv.results[j,"fold"] <- j
  }
  
  va.se.mean <- mean(cv.results[,"VA error"])
  va.MSE.mean <- mean(cv.results[,"VA MSE"])
  va.RMSE.mean <- mean(cv.results[,"VA RMSE"])
  va.NRMSE.mean <- mean(cv.results[,"VA NRMSE"])
  va.R2.mean <- mean(cv.results[,"VA R2"])
  return(list(se=va.se.mean, mse=va.MSE.mean, 
              rmse=va.RMSE.mean, nrmse=va.NRMSE.mean,
              r2=va.R2.mean))
}

