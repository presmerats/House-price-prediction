

# 3. Lasso regression from Lars ...................................................


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



lars.lasso <- function(data, dataset_id, output_results = "../Analysis Results/Lasso/"){
  
  # in each data frame, 
  # assume the target is in column 1, and target is called data$target
  # assume the features are in columns 2:ncol(df1)  <- no feature selection here!
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]]

  t <- as.numeric(train[,1])
  x <- as.matrix(train[,c(2:ncol(train))])
  
  model.lasso <- lars(x, t, type="lasso")
  lambda.lasso <- c(model.lasso$lambda,0)
  beta.lasso <- coef(model.lasso)
  
  # It may help visualization if you plot using the scaled X data
  # colors <- rainbow(8)
  # beta.scale <- attr(model.lasso$beta, "scaled:scale")
  # beta.rescaled <- beta.lasso
  # for(j in 1:length(beta.lasso)) beta.rescaled[j,] <- beta.rescaled[j,]*beta.scale
  
  # matplot(lambda.lasso, beta.rescaled, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
  #         ylab=expression(hat(beta.lasso)), col=colors)
  # text(rep(-0, 9), beta.rescaled[9,], colnames(x), pos=4, col=colors)
  
  ## suppose we decide to choose this value
  # abline(v=lambda.lasso[7], lty=2)
  # abline(h=0, lty=7)


  
  return(list(model.lasso,beta.lasso))  
}



# 5. Lasso with glmnet...............................................

glmnet.lasso <- function(data, dataset_id, output_results = "../Analysis Results/Lasso/"){
  
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
  lasso.mod = glmnet(x,t, alpha=1, lambda=grid)
  
  # model selection with internal cv
  set.seed(17)
  cv.out=cv.glmnet(x,t,alpha=1)
  bestlam = cv.out$lambda.min
  
  # refit
  # there's no refit, we use predict, with ridge.mod and tell the s=bestlam
  
  # training, validation and test errors
  tr.pred = predict(lasso.mod, s=bestlam, newx=x)
  tr.se <- 0.5*sum((tr.pred - t)^2)
  tr.MSE <- mean((tr.pred - t)^2)
  tr.NRMSE <- sqrt(tr.MSE)  
  
  # Validation error
  valist <- glmnet.lasso.CV(10,train,lasso.mod, bestlam)
  va.se <- valist[1]
  va.MSE <- valist[2]
  va.NRMSE <- valist[3]
  
  # generalisation error
  te.pred = predict(lasso.mod, s=bestlam, newx=x.test)
  te.se <- 0.5*sum((te.pred - t.test)^2)
  te.MSE <- mean((te.pred - t.test)^2)
  te.NRMSE <- sqrt(te.MSE)
  
  # write results to results file.  
  
  function_script <- "lasso_regressions_glmnet"
  comment <- "testing the workflow"
  Input <- dataset_id
  Model <- "lasso regression GLMNET"
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



glmnet.lasso.CV <- function (k,data,lasso.mod, bestlam)
{
  return(glmnet.ridge.CV(k,data,lasso.mod,bestlam))

  # CV.folds <- generateCVRuns(data$target, ntimes=1, nfold=k, stratified=TRUE)
  # 
  # thenames <- c("k","fold","TR error", "TR MSE", "TR NRMSE","VA error","VA MSE","VA NRMSE")
  # cv.results <- matrix (rep(0,length(thenames)*k),nrow=k)
  # colnames (cv.results) <- thenames
  # 
  # cv.results[,"TR error"] <- 0
  # cv.results[,"VA error"] <- 0
  # cv.results[,"TR MSE"] <- 0
  # cv.results[,"VA MSE"] <- 0
  # cv.results[,"TR NRMSE"] <- 0
  # cv.results[,"VA NRMSE"] <- 0
  # cv.results[,"k"] <- k
  # 
  # for (j in 1:k)
  # {
  #   # get VA data
  #   va <- unlist(CV.folds[[1]][[j]])
  #   x = model.matrix(target ~.,data[-va,])[,-1]
  #   t = data$target[-va]
  #   x.test = model.matrix(target ~.,data[va,])[,-1]
  #   t.test = data$target[va]
  #   
  #   # train on TR data
  #   # no need to train again the model 
  #   
  #   # predict TR data
  #   tr.pred = predict(ridge.mod, s=bestlam, newx=x)
  #   tr.se <- 0.5*sum((tr.pred - t)^2)
  #   tr.MSE <- mean((tr.pred - t)^2)
  #   tr.NRMSE <- sqrt(tr.MSE) 
  #   cv.results[j,"TR error"]  <- tr.se
  #   cv.results[j,"TR MSE"]  <- tr.MSE
  #   cv.results[j,"TR NRMSE"]  <- tr.NRMSE
  #   
  #   # predict VA data
  #   cv.pred = predict(ridge.mod, s=bestlam, newx=x.test)
  #   cv.se <- 0.5*sum((cv.pred - t.test)^2)
  #   cv.MSE <- mean((cv.pred - t.test)^2)
  #   cv.NRMSE <- sqrt(cv.MSE) 
  #   
  #   cv.results[j,"VA error"] <- cv.se
  #   cv.results[j,"VA MSE"] <- cv.MSE
  #   cv.results[j,"VA NRMSE"] <- cv.NRMSE
  #   
  #   cv.results[j,"fold"] <- j
  # }
  # 
  # va.se.mean <- mean(cv.results[,"VA error"])
  # va.MSE.mean <- mean(cv.results[,"VA MSE"])
  # va.NRMSE.mean <- mean(cv.results[,"VA NRMSE"])
  # return(c(va.se.mean, va.MSE.mean, va.NRMSE.mean))
  # # return everything: mean training error, mean va error?
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



