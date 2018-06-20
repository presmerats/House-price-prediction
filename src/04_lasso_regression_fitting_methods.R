

# 3. Lasso regression from Lars ...................................................

lars.lasso <- function(data, dataset_id, output_results = "../Analysis Results/Lasso/", comment = "testing"){
  
  # in each data frame, 
  # assume the target is in column 1, and target is called data$target
  # assume the features are in columns 2:ncol(df1)  <- no feature selection here!
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]]

  t <- as.numeric(train[,1])
  x <- as.matrix(train[,c(2:ncol(train))])
  t.test <- as.numeric(test[,1])
  x.test <- as.matrix(test[,c(2:ncol(test))])
      
  # model fitting
  model.lasso <- lars(x, t, type="lasso")
  lambda.lasso <- c(model.lasso$lambda,0)
  beta.lasso <- coef(model.lasso)
  
  # Cross validation
  # for all values of lambda, obtain the validation error then select the smaller one
  output=lars.lasso.CV.total(10,train,model.lasso)
  bestlam=output[["bestlam"]]
  valist=output[["vaerrors"]]
  # Validation error
  va.se <- valist[["se"]]
  va.MSE <- valist[["mse"]]
  va.RMSE <- valist[["rmse"]]
  va.NRMSE <- valist[["nrmse"]]
  va.R2 <- valist[["r2"]]
  
  # refit
  # no need to refit, just use s=bestlam in subsequen models
  
  # training, validation and test errors
  tr.pred = predict(model.lasso, x, s=bestlam, type="fit")$fit
  error = Prediction.errors(tr.pred,t)
  tr.se <- error[["se"]]
  tr.MSE <-  error[["mse"]]
  tr.RMSE <-  error[["rmse"]]  
  tr.NRMSE <-  error[["nrmse"]]  
  tr.R2 <-  error[["r2"]]


  

  
  # generalisation error
  te.pred = predict(model.lasso, x.test, s=bestlam, type="fit")$fit
  error = Prediction.errors(te.pred,t.test)
  te.se <- error[["se"]]
  te.MSE <-  error[["mse"]]
  te.RMSE <-  error[["rmse"]]  
  te.NRMSE <-  error[["nrmse"]]  
  te.R2 <-  error[["r2"]]
  # te.se <- 0.5*sum((te.pred - t.test)^2)
  # te.MSE <- mean((te.pred - t.test)^2)
  # te.RMSE <- sqrt(te.MSE)
  # te.NRMSE <- sqrt(te.MSE/var(t.test))  
  # te.R2 <- 1 - te.NRMSE^2

  # write results to results file.  
  
  function_script <- "Lasso_regression_larss"
  comment <- comment
  Input <- dataset_id
  Model <- "Lasso regression LARS"
  
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



lars.lasso.CV.total <- function (k,data,model.lasso)
{
 
  # model fitting
  #model.lasso <- lars(x, t, type="lasso")
  lambda.lasso <- c(model.lasso$lambda,0)
  
  
  # Cross validation
  # for all values of lambda, obtain the validation error then select the smaller one
  nlambdas <- length(lambda.lasso)
  va.errors <- rep(0,nlambdas)
  for (i in 1:nlambdas){
    # valist <- lars.lasso.CV(10,data,model.lasso, i)[1]
    # va.se <- valist[1]
    # va.MSE <- valist[2]
    # va.NRMSE <- valist[3]
    # we get the squared error
    va.errors[i] <- lars.lasso.CV(10,data,model.lasso, i)[[1]]
  }
  
  # index of the bestlambda (the one with less validation error)
  bestlam=which.min(va.errors)
  # specific tune, return bestlamda and its associated validation error
  return(list(bestlam=bestlam, vaerrors=va.errors))
 
}


lars.lasso.CV <- function (k,data,lasso.mod,bestlam)
{
  set.seed(2018)
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
    x = model.matrix(target ~.,data[-va,])[,-1]
    t = data$target[-va]
    x.test = model.matrix(target ~.,data[va,])[,-1]
    t.test = data$target[va]
    
    # train on TR data
    # no need to train again the model 
    
    # predict TR data
    tr.pred = predict(lasso.mod, x, s=bestlam, type="fit")$fit
    # tr.se <- 0.5*sum((tr.pred - t)^2)
    # tr.MSE <- mean((tr.pred - t)^2)
    # tr.NRMSE <- sqrt(tr.MSE) 
    # cv.results[j,"TR error"]  <- tr.se
    # cv.results[j,"TR MSE"]  <- tr.MSE
    # cv.results[j,"TR NRMSE"]  <- tr.NRMSE
    
    # predict VA data
    cv.pred = predict(lasso.mod, x.test, s=bestlam, type="fit" )$fit
    error = Prediction.errors(cv.pred,t.test)
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





# 5. Lasso with glmnet...............................................

glmnet.lasso <- function(data, dataset_id, output_results = "../Analysis Results/Lasso/", comment = "testing"){
  
  # in each data frame, 
  # assume the target is in column 1, dada$target
  # assume the features are in columns 2:ncol(df1)
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]] 
  
  # recommended setup
  x = model.matrix(target ~.,train)[,-1]
  t = train$target
  x.test = model.matrix(target ~.,test)[,-1]
  t.test = test$target

  # model selection with internal cv
  set.seed(17)
  cv.out=cv.glmnet(x,t,alpha=1, type.measure="mse")
  bestlam = cv.out$lambda.min
  bestlam.index = match(bestlam,cv.out$lambda)
  
  # Validation error
  verror = cv.out$cvm[bestlam.index] 
  # depending on the type of verror, perform different transformation
  error = Prediction.errors2(verror,x,t)
  va.se <- error[["se"]]
  va.MSE <-  error[["mse"]]
  va.RMSE <-  error[["rmse"]]  
  va.NRMSE  <-  error[["nrmse"]]
  va.R2 <-  error[["r2"]]
  
  # refit to training set
  # fit
  grid=10^seq(10 , -2 , length=100)
  lasso.mod = glmnet(x,t, alpha=1, lambda=grid)
  
  # training, validation and test errors
  tr.pred = predict(lasso.mod, s=bestlam, newx=x)
  error = Prediction.errors(tr.pred,t)
  tr.se <- error["se"]
  tr.MSE <-  error["mse"]
  tr.RMSE <-  error["rmse"]  
  tr.NRMSE <-  error["nrmse"]  
  tr.R2 <-  error["r2"] 
  

  
  # generalisation error
  te.pred = predict(lasso.mod, s=bestlam, newx=x.test)
  error = Prediction.errors(te.pred,t.test)
  te.se <- error["se"]
  te.MSE <-  error["mse"]
  te.RMSE <-  error["rmse"]  
  te.NRMSE <-  error["nrmse"]  
  te.R2 <-  error["r2"]
  
  # write results to results file.  
  
  function_script <- "lasso_regressions_glmnet"
  comment <- comment
  Input <- dataset_id
  Model <- "lasso regression GLMNET"
  
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



# glmnet.lasso.CV <- function (k,data,lasso.mod, bestlam)
# {
#   return(glmnet.ridge.CV(k,data,lasso.mod,bestlam))
# 
#   #   set.seed(2018)
#   # CV.folds <- generateCVRuns(data$target, ntimes=1, nfold=k, stratified=TRUE)
#   # 
#   # thenames <- c("k","fold","TR error", "TR MSE", "TR NRMSE","VA error","VA MSE","VA NRMSE")
#   # cv.results <- matrix (rep(0,length(thenames)*k),nrow=k)
#   # colnames (cv.results) <- thenames
#   # 
#   # cv.results[,"TR error"] <- 0
#   # cv.results[,"VA error"] <- 0
#   # cv.results[,"TR MSE"] <- 0
#   # cv.results[,"VA MSE"] <- 0
#   # cv.results[,"TR NRMSE"] <- 0
#   # cv.results[,"VA NRMSE"] <- 0
#   # cv.results[,"k"] <- k
#   # 
#   # for (j in 1:k)
#   # {
#   #   # get VA data
#   #   va <- unlist(CV.folds[[1]][[j]])
#   #   x = model.matrix(target ~.,data[-va,])[,-1]
#   #   t = data$target[-va]
#   #   x.test = model.matrix(target ~.,data[va,])[,-1]
#   #   t.test = data$target[va]
#   #   
#   #   # train on TR data
#   #   # no need to train again the model 
#   #   
#   #   # predict TR data
#   #   tr.pred = predict(ridge.mod, s=bestlam, newx=x)
#   #   tr.se <- 0.5*sum((tr.pred - t)^2)
#   #   tr.MSE <- mean((tr.pred - t)^2)
#   #   tr.NRMSE <- sqrt(tr.MSE) 
#   #   cv.results[j,"TR error"]  <- tr.se
#   #   cv.results[j,"TR MSE"]  <- tr.MSE
#   #   cv.results[j,"TR NRMSE"]  <- tr.NRMSE
#   #   
#   #   # predict VA data
#   #   cv.pred = predict(ridge.mod, s=bestlam, newx=x.test)
#   #   cv.se <- 0.5*sum((cv.pred - t.test)^2)
#   #   cv.MSE <- mean((cv.pred - t.test)^2)
#   #   cv.NRMSE <- sqrt(cv.MSE) 
#   #   
#   #   cv.results[j,"VA error"] <- cv.se
#   #   cv.results[j,"VA MSE"] <- cv.MSE
#   #   cv.results[j,"VA NRMSE"] <- cv.NRMSE
#   #   
#   #   cv.results[j,"fold"] <- j
#   # }
#   # 
#   # va.se.mean <- mean(cv.results[,"VA error"])
#   # va.MSE.mean <- mean(cv.results[,"VA MSE"])
#   # va.NRMSE.mean <- mean(cv.results[,"VA NRMSE"])
#   # return(c(va.se.mean, va.MSE.mean, va.NRMSE.mean))
#   # # return everything: mean training error, mean va error?
# }



# NRMSE ------------------
# 
# compute.NRMSE <- function(model, test, typemodel="ridge"){
#   # assume test[,1] is the target
#   # assume test[,-1] are the features
#   
#   t.new <- test[,1]
#   x <- as.matrix(test[,c(-1)])
#   xdf <- test[,c(-1)]
#   N.test <- nrow(x)
#   
#   if (typemodel=="ridge"){
#     beta.ridgereg.FINAL = coef(model)
#     (pred.ridgereg <- sum((t.new - beta.ridgereg.FINAL[1] - x %*%beta.ridgereg.FINAL[-1])^2)/N.test)
#     (nrmse.ridgereg <- sqrt(pred.ridgereg/((N.test-1)*var(t.new))))
#     return(nrmse.ridgereg)
#   } else if (typemodel=="lasso1") {
#     #lasso type
#     # s¿
#     predictions <- predict(model, newx=x, s=2.7)
#     
#     (pred.lasso <- sum(( t.new - predictions$fit )^2)/N.test)
#     (nrmse.lasso <- sqrt(pred.lasso/((N.test-1)*var(t.new))))
#   } else if (typemodel=="lasso2") {
#     #lasso type
#     # s¿
#     predictions <- predict(model, newx=x, s=2.7)
#     (pred.lasso <- sum(( t.new - predictions )^2)/N.test)
#     (nrmse.lasso <- sqrt(pred.lasso/((N.test-1)*var(t.new))))
#   }
#   
# }



