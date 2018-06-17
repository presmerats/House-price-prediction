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

# 1. Test and Train sets-----------------------------------
# 
# train.test.set <- function(x){
#   #shuffling the data
#   set.seed (1714)
#   x <- x[sample.int(nrow(x)),]
#   
#   # 30% test, 70% train
#   i <- round(nrow(x)*0.3)
#   test <- x[1:i,]
#   train <- x[(i+1):nrow(x),]
#   
#   return(list(test,train))
# }



# 2. Ridge regression -------------------------------------


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
  
  

  ## These are the test MSEs
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




ridge1 <- function(data, dataset_id, output_results = "../Analysis Results/Ridge/"){

  # in each data frame, 
  # assume the target is in column 1, and target is called data$target
  # assume the features are in columns 2:ncol(df1) 
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]]
  
  
  summary(train)
  names(train)
  
  
  load_install_packages("MASS")
  model.ridge <- lm.ridge(price ~ ., data=train, lambda = seq(0,10,0.1))
  
  plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
       xlab=expression(lambda), ylab="GCV")
  
  # The optimal lambda is given by
  
  (lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)])
  
  # We can plot the coefficients and see how they vary as a function of lambda
  
  colors <- rainbow(8)
  
  matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
          ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
  abline(v=lambda.ridge, lty=2)
  abline(h=0, lty=2)
  text(rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(train), pos=4, col=colors)
  
  ## So we refit our final ridge regression model using the best lambda
  
  model.ridgereg.FINAL <- lm.ridge(price ~ ., data=train, lambda = lambda.ridge)
  
  (beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL))
  
  return(list(model.ridgereg.FINAL, beta.ridgereg.FINAL))

}


# 3. Lasso regression ...................................................


lasso1 <- function(df1){
    
  ## Recall that in the LASSO, the coefficients are penalized by the L1 norm. The 
  # optimal value for lambda is again chosen by cross-validation
  
  # in each data frame, 
  # assume the target is in column 1
  # assume the features are in columns 2:ncol(df1)  <- no feature selection here!
  
  train = df1[[1]]
  test = df1[[2]]
  load_install_packages("lars")
  
  class(train)
  train
  dim(train)
  t <- as.numeric(train[,1])
  x <- as.matrix(train[,c(2:ncol(train))])
  
  model.lasso <- lars(x, t, type="lasso")
  
  lambda.lasso <- c(model.lasso$lambda,0)
  
  beta.lasso <- coef(model.lasso)
  
  colors <- rainbow(8)
  
  # It may help visualization if you plot using the scaled X data
  
  beta.scale <- attr(model.lasso$beta, "scaled:scale")
  beta.rescaled <- beta.lasso
  # why 1:9?
  for(j in 1:9) beta.rescaled[j,] <- beta.rescaled[j,]*beta.scale
  
  matplot(lambda.lasso, beta.rescaled, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
          ylab=expression(hat(beta.lasso)), col=colors)
  text(rep(-0, 9), beta.rescaled[9,], colnames(x), pos=4, col=colors)
  
  ## suppose we decide to choose this value
  abline(v=lambda.lasso[7], lty=2)
  abline(h=0, lty=7)
  
  # we return them all and then decide
  #(beta.lasso <- beta.lasso[7,])

  return(list(model.lasso,beta.lasso))  
}


# 4. Ridge with glmnet...............................................

ridge2 <- function(df1){
  # in each data frame, 
  # assume the target is in column 1
  # assume the features are in columns 2:ncol(df1)  <- no feature selection here!
  
  train = df1[[1]]
  test = df1[[2]]
  
  # glmnet package
  
    
  lload_install_packages("glmnet")

  # recommended setup
  x = model.matrix(price~.,train)[,-1]
  t = train$price
  # fit
  ridge.mod = glmnet(x,t, alpha=0, lambda=seq(0,10,0.1))
  set.seed(17)
  cv.out=cv.glmnet(x,t,alpha=0, lambda=seq(0,10,0.1))
  plot(cv.out)
  bestlam = cv.out$lambda.min
  bestlam # 5.1
  
  cv.out=cv.glmnet(x,t,alpha=0, lambda=seq(0,10,0.1), nfolds=nrow(x)/20)
  plot(cv.out)
  bestlam = cv.out$lambda.min
  bestlam # 3.3
  
  cv.out=cv.glmnet(x,t,alpha=0)
  plot(cv.out)
  bestlam = cv.out$lambda.min
  bestlam # 3.3
  
  # NRMSE
  N <- nrow(x)
  t <- train$price
  x <- as.matrix(train[,c(-1)])
  
  model.ridge.final <- glmnet(x,t, alpha=0, lambda=3.3  ) 
  (beta.ridgereg.FINAL <- coef(model.ridge.final))

  return(list(model.ridge.final,beta.ridgereg.FINAL ))
}


  
# 5. Lasso with glmnet...............................................

lasso2 <- function(df1){

  # in each data frame, 
  # assume the target is in column 1
  # assume the features are in columns 2:ncol(df1)  <- no feature selection here!
  train = df1[[1]]
  test = df1[[2]] 
  
  # Lasso with the glmnet package
  load_install_packages("glmnet")
  # recommended setup
  x = model.matrix(price~.,train)[,c(-1)]
  t = train$price
  # fit
  lasso.mod = glmnet(x,t, alpha=1, lambda=seq(0,10,0.1))
  plot(lasso.mod)
  set.seed(17)
  cv.out=cv.glmnet(x,t,alpha=1, lambda=seq(0,10,0.1))
  plot(cv.out)
  bestlam = cv.out$lambda.min
  bestlam # 2.7  which is close to the manual computation
  
  N <- nrow(x)
  model.lasso.final <- glmnet(x,t, alpha=1, lambda=bestlam ) 
  (beta.lasso.FINAL <- coef(model.lasso.final))
  
  return(list(model.lasso.final, beta.lasso.FINAL))
   
}


# NRMSE ------------------

compute.NRMSE <- function(model, test, typemodel="ridge"){
  # assume test[,1] is the target
  # assume test[,-1] are the features
  
  t.new <- test[,1]
  x <- as.matrix(test[,c(-1)])
  xdf <- test[,c(-1)]
  N.test <- nrow(x)
  
  if (typemodel=="ridge"){
    beta.ridgereg.FINAL = coef(model)
    (pred.ridgereg <- sum((t.new - beta.ridgereg.FINAL[1] - x %*%beta.ridgereg.FINAL[-1])^2)/N.test)
    (nrmse.ridgereg <- sqrt(pred.ridgereg/((N.test-1)*var(t.new))))
    return(nrmse.ridgereg)
  } else if (typemodel=="lasso1") {
    #lasso type
    # s¿
    predictions <- predict(model, newx=x, s=2.7)
    
    (pred.lasso <- sum(( t.new - predictions$fit )^2)/N.test)
    (nrmse.lasso <- sqrt(pred.lasso/((N.test-1)*var(t.new))))
  } else if (typemodel=="lasso2") {
    #lasso type
    # s¿
    predictions <- predict(model, newx=x, s=2.7)
    (pred.lasso <- sum(( t.new - predictions )^2)/N.test)
    (nrmse.lasso <- sqrt(pred.lasso/((N.test-1)*var(t.new))))
  }
  
}




# -- Execution of the different models and their NRMSE----------------------------
# REMARK: we are kind of overfitting to the test data set!
# REMARK2: this needs some more adaptation to the framework, from trainign many models and writing to disk 

ridge_regression_fitting_group <- function(data, dataset_id, output_results = "../Analysis Results/Rigde Regression Fitting/")
{
  
  # loading the different datasets (feature subsets)
  load("../Data/pending_revision/data-preprocessed.Rdata")
  load("../Data/pending_revision/data-featureset-2.Rdata")
  load("../Data/pending_revision/data-featureset-3.Rdata")
  load("../Data/pending_revision/data-featureset-4.Rdata")
  load("../Data/pending_revision/data-featureset-5.Rdata")
  objects()
  
  df1 = train.test.set(data.new)
  df2 = train.test.set(data2)
  df3 = train.test.set(data3)
  df4 = train.test.set(data4)
  df5 = train.test.set(data5)
  #df5[[1]]
  #class(df5)
  #class(df5[1])
  #class(df5[[1]])
  
  # ridge1
  ridge1.fs1 = ridge1(df1)
  (nrmse.ridge1.fset1 <- compute.NRMSE(ridge1.fs1[[1]],df1[[2]],typemodel="ridge"))
  ridge1.fs2 = ridge1(df2)
  (nrmse.ridge1.fset2 <- compute.NRMSE(ridge1.fs2[[1]],df2[[2]],typemodel="ridge"))
  ridge1.fs3 = ridge1(df3)
  (nrmse.ridge1.fset4 <- compute.NRMSE(ridge1.fs3[[1]],df3[[2]],typemodel="ridge"))
  ridge1.fs4 = ridge1(df4)
  (nrmse.ridge1.fset4 <- compute.NRMSE(ridge1.fs4[[1]],df4[[2]],typemodel="ridge"))
  ridge1.fs5 = ridge1(df5)
  (nrmse.ridge1.fset5 <- compute.NRMSE(ridge1.fs5[[1]],df5[[2]],typemodel="ridge"))
  
  # ridge 2
  ridge2.fs1 = ridge2(df1)
  (nrmse.ridge2.fset1 <- compute.NRMSE(ridge2.fs1[[1]],df1[[2]],typemodel="ridge"))
  ridge2.fs2 = ridge2(df2)
  (nrmse.ridge2.fset2 <- compute.NRMSE(ridge2.fs2[[1]],df2[[2]],typemodel="ridge"))
  ridge2.fs3 = ridge2(df3)
  (nrmse.ridge2.fset4 <- compute.NRMSE(ridge2.fs3[[1]],df3[[2]],typemodel="ridge"))
  ridge2.fs4 = ridge2(df4)
  (nrmse.ridge2.fset4 <- compute.NRMSE(ridge2.fs4[[1]],df4[[2]],typemodel="ridge"))
  ridge2.fs5 = ridge2(df5)
  (nrmse.ridge2.fset5 <- compute.NRMSE(ridge2.fs5[[1]],df5[[2]],typemodel="ridge"))
  
  # lasso1
  lasso1.result.1 <- lasso1(df1)
  (nrmse.lasso1.fset1 <- compute.NRMSE(lasso1.result.1[[1]],df1[[2]],typemodel="lasso1"))
  lasso1.result.2 <- lasso1(df2)
  (nrmse.lasso1.fset2 <- compute.NRMSE(lasso1.result.2[[1]],df2[[2]],typemodel="lasso1"))
  lasso1.result.3 <- lasso1(df3)
  (nrmse.lasso1.fset3 <- compute.NRMSE(lasso1.result.3[[1]],df3[[2]],typemodel="lasso1"))
  lasso1.result.4 <- lasso1(df4)
  (nrmse.lasso1.fset4 <- compute.NRMSE(lasso1.result.4[[1]],df4[[2]],typemodel="lasso1"))
  lasso1.result.5 <- lasso1(df5)
  (nrmse.lasso1.fset5 <- compute.NRMSE(lasso1.result.5[[1]],df5[[2]],typemodel="lasso1"))
  
  # lasso2
  lasso2.result.1 <- lasso2(df1)
  (nrmse.lasso2.fset1 <- compute.NRMSE(lasso2.result.1[[1]],df1[[2]],typemodel="lasso2"))
  lasso2.result.2 <- lasso2(df2)
  (nrmse.lasso2.fset2 <- compute.NRMSE(lasso2.result.2[[1]],df2[[2]],typemodel="lasso2"))
  lasso2.result.3 <- lasso2(df3)
  (nrmse.lasso2.fset3 <- compute.NRMSE(lasso2.result.3[[1]],df3[[2]],typemodel="lasso2"))
  lasso2.result.4 <- lasso2(df4)
  (nrmse.lasso2.fset4 <- compute.NRMSE(lasso2.result.4[[1]],df4[[2]],typemodel="lasso2"))
  lasso2.result.5 <- lasso2(df5)
  (nrmse.lasso2.fset5 <- compute.NRMSE(lasso2.result.5[[1]],df5[[2]],typemodel="lasso2"))
  
  
  RSS <- lasso2.result.1[[1]]$
  RSE <- sqrt(RSS/nrow(data)) # average error made on a prediction. 
  mean_price <- mean(data$price)
  TSS <- sum((data$price - mean_price)^2)
  R_sqared <- (TSS - RSS)/TSS 

}