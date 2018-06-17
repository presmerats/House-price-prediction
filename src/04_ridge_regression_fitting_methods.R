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


mass.ridge <-  function(data, dataset_id, output_results = "../Analysis Results/Ridge/"){
  
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
  # We can plot the coefficients and see how they vary as a function of lambda
  # colors <- rainbow(8)
  # matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda),
  #         ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
  # abline(v=lambda.ridge, lty=2)
  # abline(h=0, lty=2)
  # text(rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(train), pos=4, col=colors)
  
  ## So we refit our final ridge regression model using the best lambda
  model.ridgereg.FINAL <- lm.ridge(target ~ ., data=train, lambda = lambda.ridge)
  beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL)
  
  ## These are the test MSEs examples
  #(pred.linreg <- sum((t.new - predict(model.linreg.FINAL, test[,1:8]))^2)/N.test)
  #(pred.ridgereg <- sum((t.new - beta.ridgereg.FINAL[1] - as.matrix(test[,1:8])%*%beta.ridgereg.FINAL[2:9])^2)/N.test)
  #(pred.lasso <- sum((t.new - predict(model.lasso, as.matrix(test[,1:8]), s=4, type="fit")$fit)^2)/N.test)
  
  # training error
  tr.pred <- beta.ridgereg.FINAL[1] + as.matrix(train[,2:ncol(train)])%*%beta.ridgereg.FINAL[-1]
  #tr.pred <- as.matrix(cbind(const=1,train[,2:ncol(train)])) %*% coef(model.ridgereg.FINAL)
  tr.se <- sum((tr.pred - train$target)^2)*0.5
  tr.MSE <- 2*tr.se/nrow(train)
  tr.NRMSE <- sqrt(tr.MSE) 
  
  # validation error ( in ridge the Validation error is done automatically)
  # manually repeat validation error computation for later model selection
  #tr.pred <- beta.ridgereg.FINAL[1] + as.matrix(train[,2:ncol(train)])%*%beta.ridgereg.FINAL[-1]
  valist <- mass.ridge.CV(10,train,lambda.ridge)
  va.se <- valist[1]
  va.MSE <- valist[2]
  va.NRMSE <- valist[3]
  
  # generalization error
  te.pred <- beta.ridgereg.FINAL[1] + as.matrix(test[,2:ncol(train)])%*%beta.ridgereg.FINAL[-1]
  te.se <- sum((te.pred - test$target)^2)*0.5
  te.MSE <- 2*te.se/nrow(test) 
  te.NRMSE <- sqrt(te.MSE)
  
  #--------------------------------------------------------------------------------------
  # write results to results file.  
  
  function_script <- "ridge_regressions_mass"
  comment <- "testing the workflow"
  Input <- dataset_id
  Model <- "ridge regression MASS"
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



# Simplified CV function version
mass.ridge.CV <- function (k,data, lambda.ridge)
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
    
    # train on TR data
    #my.da.TR <- lm(target ~ X1 + X2, data = data[-va,], prior=priors, CV=FALSE) 
    model.ridgereg.FINAL <- lm.ridge(target ~ ., data=data[-va,], lambda = lambda.ridge)
    beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL)
    
    # predict TR data
    #pred.va <- predict (my.da.TR)$class
    tr.pred <- beta.ridgereg.FINAL[1] + as.matrix(data[-va,2:ncol(data)])%*%beta.ridgereg.FINAL[-1]
    tr.se <- sum((tr.pred - data$target[-va])^2)*0.5
    cv.results[j,"TR error"]  <- tr.se
    tr.MSE <- 2*tr.se/nrow(data[-va])
    cv.results[j,"TR MSE"]  <- tr.MSE
    cv.results[j,"TR NRMSE"]  <- sqrt(tr.MSE)
    # which one? se? MSE? NRMSE? in the end it will be averaged

    
    # predict VA data
    cv.pred <- beta.ridgereg.FINAL[1] + as.matrix(data[va,2:ncol(data)])%*%beta.ridgereg.FINAL[-1]
    
    cv.se <- sum((cv.pred - data$target[va])^2)*0.5
    cv.results[j,"VA error"] <- cv.se
    cv.MSE <-  2*cv.se/nrow(data[va,])
    cv.results[j,"VA MSE"] <- cv.MSE
    cv.results[j,"VA NRMSE"] <-  sqrt(cv.MSE)
    
    cv.results[j,"fold"] <- j
  }
  
  va.se.mean <- mean(cv.results[,"VA error"])
  va.MSE.mean <- mean(cv.results[,"VA MSE"])
  va.NRMSE.mean <- mean(cv.results[,"VA NRMSE"])
  return(c(va.se.mean, va.MSE.mean, va.NRMSE.mean))
  # return everything: mean training error, mean va error?
}





# 4. Ridge with glmnet...............................................

glmnet.ridge <- function(data, dataset_id, output_results = "../Analysis Results/Ridge/"){
  
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
  
  # fit
  grid=10^seq(10 , -2 , length=100)
  ridge.mod = glmnet(x,t, alpha=0, lambda=grid)
  set.seed(17)
  
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
  # bestlam # 3.3
  
  # refit
  # there's no refit, we use predict, with ridge.mod and tell the s=bestlam
  
  # training, validation and test errors
  tr.pred = predict(ridge.mod, s=bestlam, newx=x)
  tr.se <- 0.5*sum((tr.pred - t)^2)
  tr.MSE <- mean((tr.pred - t)^2)
  tr.NRMSE <- sqrt(tr.MSE)  
 
  # Validation error
  valist <- glmnet.ridge.CV(10,train,ridge.mod, bestlam)
  va.se <- valist[1]
  va.MSE <- valist[2]
  va.NRMSE <- valist[3]
   
  # generalisation error
  te.pred = predict(ridge.mod, s=bestlam, newx=x.test)
  te.se <- 0.5*sum((te.pred - t.test)^2)
  te.MSE <- mean((te.pred - t.test)^2)
  te.NRMSE <- sqrt(te.MSE)
  
  # write results to results file.  
  
  function_script <- "ridge_regressions_glmnet"
  comment <- "testing the workflow"
  Input <- dataset_id
  Model <- "ridge regression GLMNET"
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


  
# Simplified CV function version
glmnet.ridge.CV <- function (k,data,ridge.mod, bestlam)
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
    x = model.matrix(target ~.,data[-va,])[,-1]
    t = data$target[-va]
    x.test = model.matrix(target ~.,data[va,])[,-1]
    t.test = data$target[va]
    
    # train on TR data
    # no need to train again the model 
    
    # predict TR data
    tr.pred = predict(ridge.mod, s=bestlam, newx=x)
    tr.se <- 0.5*sum((tr.pred - t)^2)
    tr.MSE <- mean((tr.pred - t)^2)
    tr.NRMSE <- sqrt(tr.MSE) 
    cv.results[j,"TR error"]  <- tr.se
    cv.results[j,"TR MSE"]  <- tr.MSE
    cv.results[j,"TR NRMSE"]  <- tr.NRMSE

    # predict VA data
    cv.pred = predict(ridge.mod, s=bestlam, newx=x.test)
    cv.se <- 0.5*sum((cv.pred - t.test)^2)
    cv.MSE <- mean((cv.pred - t.test)^2)
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


