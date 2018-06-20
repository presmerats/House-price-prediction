####################################################################
# Machine Learning Project
# Pau Rodriguez

# Ridge & Lasso Regression
# 08/05/2018
####################################################################


# 0. loading--------------------------------------------------
# no longer required
# loading the different datasets (feature subsets)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#rm(list = ls())





# 2. Ridge regression from MASS-------------------------------------


mass.ridge <-  function(data, dataset_id, output_results = "../Analysis Results/Ridge/", comment = "testing"){
  
  # in each data frame, 
  # assume the target is in column 1, and target is called data$target
  # assume the features are in columns 2:ncol(df1) 
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]]
  
  model.ridge <- lm.ridge(target ~ ., data=train, lambda = seq(0,10,0.1))
  #plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
  #     xlab=expression(lambda), ylab="GCV")

  # The optimal lambda is given, we refit our model
  lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)]
  gcv <- min(model.ridge$GCV)
  print("gcv")
  print(gcv)

  # validation error
  #valist <- mass.ridge.CV(10,train,lambda.ridge)
  valist <- Prediction.errors2(gcv,train,train$target)
  va.se <- valist[["se"]]
  va.MSE <- valist[["mse"]]
  va.RMSE <- valist[["rmse"]]
  va.NRMSE <- valist[["nrmse"]]
  va.R2 <- valist[["r2"]]
  
  
  ## So we refit our final ridge regression model using the best lambda
  model.ridgereg.FINAL <- lm.ridge(target ~ ., data=train, lambda = lambda.ridge)
  beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL)
  

  
  # training error
  tr.pred <- beta.ridgereg.FINAL[1] + as.matrix(train[,2:ncol(train)])%*%beta.ridgereg.FINAL[-1]
  #tr.pred <- as.matrix(cbind(const=1,train[,2:ncol(train)])) %*% coef(model.ridgereg.FINAL)
  error = Prediction.errors(tr.pred,train$target)
  tr.se <- error[["se"]]
  tr.MSE <-  error[["mse"]]
  tr.RMSE <-  error[["rmse"]]  
  tr.NRMSE <-  error[["nrmse"]]  
  tr.R2 <-  error[["r2"]]
  

  # generalization error
  te.pred <- beta.ridgereg.FINAL[1] + as.matrix(test[,2:ncol(train)])%*%beta.ridgereg.FINAL[-1]
  error = Prediction.errors(te.pred,test$target)
  te.se <- error[["se"]]
  te.MSE <-  error[["mse"]]
  te.RMSE <-  error[["rmse"]]  
  te.NRMSE <-  error[["nrmse"]]  
  te.R2 <-  error[["r2"]]
  
  #--------------------------------------------------------------------------------------
  # write results to results file.  
  
  function_script <- "ridge_regressions_mass"
  comment <- comment
  Input <- dataset_id
  Model <- "ridge regression MASS"
  
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



# Simplified CV function version
mass.ridge.CV <- function (k,data, lambda.ridge)
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
    
    # train on TR data
    #my.da.TR <- lm(target ~ X1 + X2, data = data[-va,], prior=priors, CV=FALSE) 
    model.ridgereg.FINAL <- lm.ridge(target ~ ., data=data[-va,], lambda = lambda.ridge)
    beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL)
    
    # predict TR data
    #pred.va <- predict (my.da.TR)$class
    tr.pred <- beta.ridgereg.FINAL[1] + as.matrix(data[-va,2:ncol(data)])%*%beta.ridgereg.FINAL[-1]
    # tr.se <- sum((tr.pred - data$target[-va])^2)*0.5
    # cv.results[j,"TR error"]  <- tr.se
    # tr.MSE <- 2*tr.se/nrow(data[-va])
    # cv.results[j,"TR MSE"]  <- tr.MSE
    # cv.results[j,"TR NRMSE"]  <- sqrt(tr.MSE)
    # which one? se? MSE? NRMSE? in the end it will be averaged

    
    # predict VA data
    cv.pred <- beta.ridgereg.FINAL[1] + as.matrix(data[va,2:ncol(data)])%*%beta.ridgereg.FINAL[-1]
    error = Prediction.errors(cv.pred,data$target[va])
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





# 4. Ridge with glmnet...............................................

glmnet.ridge <- function(data, dataset_id, output_results = "../Analysis Results/Ridge/", comment = "testing"){
  
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
  
  # K-fold CV, by default it is 10-fold cross validation
  # cv.out=cv.glmnet(x,t,alpha=0, lambda=seq(0,10,0.1))
  # #plot(cv.out)
  # bestlam = cv.out$lambda.min
  # #bestlam # 5.1
  
  # # nrow(x)/20-fold CV
  # cv.out=cv.glmnet(x,t,alpha=0, lambda=seq(0,10,0.1), nfolds=nrow(x)/20)
  # #plot(cv.out)
  # bestlam = cv.out$lambda.min
  # bestlam # 3.3
  
  # # LOOCV with no alpha indication, longer but better cuz lamb is not predefined?
  cv.out=cv.glmnet(x,t,alpha=0)
  #plot(cv.out)
  bestlam = cv.out$lambda.min
  bestlam.index = match(bestlam,cv.out$lambda)
  
  
  # Validation error
  verror = cv.out$cvm[bestlam.index] 
  # depending on the type of verror, perform different transformation
  error = Prediction.errors2(verror,x,t)
  va.se <- error["se"]
  va.MSE <-  error["mse"]
  va.RMSE <-  error["rmse"]  
  va.NRMSE  <-  error["nrmse"]
  va.R2 <-  error["r2"]
  
  
  # refit
  grid=10^seq(10 , -2 , length=100)
  ridge.mod = glmnet(x,t, alpha=0, lambda=grid)
  set.seed(17) 
  
  # training, validation and test errors
  tr.pred = predict(ridge.mod, s=bestlam, newx=x)
  error = Prediction.errors(tr.pred,t)
  tr.se <- error[["se"]]
  tr.MSE <-  error[["mse"]]
  tr.RMSE <-  error[["rmse"]]  
  tr.NRMSE <-  error[["nrmse"]]  
  tr.R2 <-  error[["r2"]]
   
  # generalisation error
  te.pred = predict(ridge.mod, s=bestlam, newx=x.test)
  error = Prediction.errors(te.pred,t.test)
  te.se <- error[["se"]]
  te.MSE <-  error[["mse"]]
  te.RMSE <-  error[["rmse"]]  
  te.NRMSE <-  error[["nrmse"]]  
  te.R2 <-  error[["r2"]]
  
  # write results to results file.  
  
  function_script <- "ridge_regressions_glmnet"
  comment <- comment
  Input <- dataset_id
  Model <- "ridge regression GLMNET"
  
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


#   
# # Simplified CV function version
# glmnet.ridge.CV <- function (k,data,ridge.mod, bestlam)
# {
#   set.seed(2018)
#   CV.folds <- generateCVRuns(data$target, ntimes=1, nfold=k, stratified=TRUE)
#   
#   thenames <- c("k","fold","TR error", "TR MSE", "TR NRMSE","VA error","VA MSE","VA RMSE","VA NRMSE","VA R2")
#   cv.results <- matrix (rep(0,length(thenames)*k),nrow=k)
#   colnames (cv.results) <- thenames
#   
#   cv.results[,"TR error"] <- 0
#   cv.results[,"TR MSE"] <- 0
#   cv.results[,"TR NRMSE"] <- 0
#   
#   cv.results[,"VA error"] <- 0
#   cv.results[,"VA MSE"] <- 0
#   cv.results[,"VA RMSE"] <- 0
#   cv.results[,"VA NRMSE"] <- 0
#   cv.results[,"VA R2"] <- 0
#   cv.results[,"k"] <- k
#   
#   for (j in 1:k)
#   {
#     # get VA data
#     va <- unlist(CV.folds[[1]][[j]])
#     x = model.matrix(target ~.,data[-va,])[,-1]
#     t = data$target[-va]
#     x.test = model.matrix(target ~.,data[va,])[,-1]
#     t.test = data$target[va]
#     
#     # train on TR data
#     # no need to train again the model 
#     
#     # predict TR data
#     tr.pred = predict(ridge.mod, s=bestlam, newx=x)
#     # tr.se <- 0.5*sum((tr.pred - t)^2)
#     # tr.MSE <- mean((tr.pred - t)^2)
#     # tr.NRMSE <- sqrt(tr.MSE) 
#     # cv.results[j,"TR error"]  <- tr.se
#     # cv.results[j,"TR MSE"]  <- tr.MSE
#     # cv.results[j,"TR NRMSE"]  <- tr.NRMSE
# 
#     # predict VA data
#     cv.pred = predict(ridge.mod, s=bestlam, newx=x.test)
#     error = Prediction.errors(cv.pred,t.test)
#     cv.results[j,"VA error"] <- error[["se"]]
#     cv.results[j,"VA MSE"] <- error[["mse"]]
#     cv.results[j,"VA RMSE"] <- error[["rmse"]] 
#     cv.results[j,"VA NRMSE"] <- error[["nrmse"]] 
#     cv.results[j,"VA R2"] <- error[["r2"]] 
# 
#     cv.results[j,"fold"] <- j
#   }
#   
#   va.se.mean <- mean(cv.results[,"VA error"])
#   va.MSE.mean <- mean(cv.results[,"VA MSE"])
#   va.RMSE.mean <- mean(cv.results[,"VA RMSE"])
#   va.NRMSE.mean <- mean(cv.results[,"VA NRMSE"])
#   va.R2.mean <- mean(cv.results[,"VA R2"])
#   return(list(se=va.se.mean, mse=va.MSE.mean, 
#               rmse=va.RMSE.mean, nrmse=va.NRMSE.mean,
#               r2=va.R2.mean))
# }



