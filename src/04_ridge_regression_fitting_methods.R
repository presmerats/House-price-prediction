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
  tr.pred <- as.matrix(cbind(const=1,train[,2:ncol(train)])) %*% coef(model.ridgereg.FINAL)
  tr.se <- sum((tr.pred - train$target)^2)*0.5
  tr.MSE <- 2*tr.se/nrow(train)
  tr.NRMSE <- sqrt(tr.MSE) 
  
  # validation error ( in ridge the Validation error is done automatically)
  # va.pred <- predict.lm(model.ridgereg.FINAL, newdata=train[va,])
  va.se <- model.ridgereg.FINAL$GCV
  va.MSE <- 0
  va.NRMSE <- 0
  
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
  va.se <- 0
  va.MSE <- 0
  va.NRMSE <- 0
   
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


  


