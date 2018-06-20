writeResults <- function(function_script, comment, Input, Model, Training_error, Training_MSE, Training_NRMSE, Testing_error, Testing_MSE, Testing_NRMSE){
  #function_script <- "ridge_regressions_glmnet"
  #comment <- "testing the workflow"
  #Input <- dataset_id
  #Model <- "ridge regression GLMNET"
  #Training_error <- tr.se
  #Training_MSE <- tr.MSE
  #Training_NRMSE <- tr.NRMSE
  Validation_error <- "-1"
  Validation_MSE <- "-1"
  Validation_NRMSE <- "-1"
  #Testing_error <- te.se
  #Testing_MSE <- te.MSE
  #Testing_NRMSE <- te.NRMSE
  
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




printResult <- function(method, dataset_id, learnError, predError )
{
  writeLines(paste("Method: ", method, "\nDataset: ", dataset_id, "\nLearner RMSE: ", learnError, "\nPrediction RMSE: ", predError, "\n**********************************"))
}

# deprecated
regression_tree_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Trees/")
{
  
  
  #regression.tree.10x10.CV <- MODEL.CV.OVER.Ks(data,10,"regression_tree")
  
  browser()
  
  ## Test assumptions
  # # test assumptions, return invalid results if not passed. 
  # lmtest::dwtest(my_lr_tr, alternative="two.sided") # Independance among all the samples - Durbin Watson Test.
  # shapiro.test(residuals(my_lr_tr)[1:4990]) # All populations are normal - Saphiro Test
  # lmtest::bgtest(my_lr_tr) # Equal variances - Homogeneity of variances. 
  # 
  
  # write results to results file.  
  
  function_script <- "trees_methods_fitting"
  comment <- "testing the workflow"
  Input <- dataset_id
  Model <- "simple regression tree"
  Trainin_error_CV <- round(regression.tree.10x10.CV[1],2)
  Testing_error_CV <- round(regression.tree.10x10.CV[2],2)
  
  result <- cbind(function_script, comment, Input, Model, Trainin_error_CV, Testing_error_CV)
  
  write.table(result, file="../Analysis Results/model_results.csv", append = TRUE, sep=";", col.names = FALSE, row.names = FALSE)
  
  
}

##
## Using Rpart library
##
classification_rpart_tree_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Trees/")
{
  priceCat<-cut(data$target, c(0,250000,300000,350000,400000,450000,525000,600000,750000,1000000,7700001), right=FALSE, labels=c(1:10)) #labels=c("low", "economic", "middle-class", "high","luxury"))
  
  table(priceCat)
  data$priceCat <- as.factor(priceCat)
  
  df1 <- train.test.set(data)
  training_data = df1[[1]]
  testing_data = df1[[2]]

  set.seed(2018)
  data.dt = rpart(priceCat~.-target, data = training_data, control = rpart.control(cp=0.001, xval=10))
  
  printcp(data.dt)
  # The root node error is    13278/15129 = 0.87765
  # Variables actually used in tree construction:
  # floors      grade       sqft_above  sqft_living sqft_lot15  view        yr_built    zipcode 
  
  # Lets plot the size vs. the Relative error, visualize cross-validation results:
  # rememeber that we used 10 partitions for the cross validation (xval=10)
  png(paste(output_results,"CV_classification_rpart_tree_plot.png",sep = ""), width = 800, height = 600, units = "px", pointsize = 20)
  plotcp(data.dt)
  dev.off()
  # We can see that the minimum error happen in tree with size 77.
  # The Complexity Parameter (CP) value for this tree is:
  get_CP <- function(cptable){
    cptable = as.data.frame(cptable)
    ind = which.min(cptable$xerror)
    
    xerr <- cptable$xerror[ind]
    xstd <- cptable$xstd[ind]
    
    i = 1
    while (cptable$xerror[i] > xerr+xstd) i = i+1
    CP = cptable$CP[i]
    # SELECTED VALUE OF THE COMPLEXITY PARAMETER
    return(CP)
  }
  
  (CP = get_CP(data.dt$cptable))
  # CP = 0.001054376
  
  browser()
  
  # Now lets plot the training tree:
  png(paste(output_results,"classification_rpart_tree_training.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  rpart.plot(data.dt, type = 4, extra = 101)
  dev.off()
  
  #next need to prune the tree
  set.seed(2018)
  prune.dt <- prune(data.dt,cp=CP)

  
  # Plot the importance of variables in the prediction.
  png(paste(output_results,"classification_rpart_pruneVarImpo.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  barplot(prune.dt$variable.importance, col=rgb(0.2,0.4,0.6,0.6), las=2 ,main="Variable importance")
  dev.off()
  

  # Compute the accuracy, precision, recall and AUC on the test individuals.
  
  # Lets Use the prune tree to predict the results of our testing data:
  pred_test = predict(prune.dt, newdata=testing_data,type="class")
  
  # Lets use the prune tree to predit the training error data
  pred_learn=predict(prune.dt, data=training_data,type="class")
  
  
  aa <- table(training_data$priceCat,pred_learn)
  accuracy.learn <-  100*sum(diag(aa))/sum(aa)
  accuracy.learn
  # 38.48
  
  aa <- table(testing_data$priceCat,pred_test)
  accuracy.test <-  100*sum(diag(aa))/sum(aa)
  accuracy.test
  # 35.73411
  #printResult("Classification_tree_rpartlib\naccuracy, not RMSE\n", dataset_id , accuracy.learn, accuracy.test)

  writeResults("Classification_tree_rpartlib", "accuracy, not RMSE", dataset_id, "Classification_tree_rpartlib", -1 , -1 ,accuracy.learn, -1, -1 , accuracy.test)
  
  
}

regression_rpart_tree_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Trees/", comment = "testing",filename = "../Analysis Results/model_results.csv")
{
  df1 <- train.test.set(data)
  training_data = df1[[1]]
  testing_data = df1[[2]]
  
  set.seed(2018)
  data.dt = rpart(target~., data = training_data, method = "anova", control = rpart.control(cp=0.001, xval=10))
  
  #printcp(data.dt)
  # Root node error: 2.0312e+15/15129 = 1.3426e+11
  # Variables actually used in tree construction:
  # fbathrooms     condition     floors        grade         sqft_above    sqft_living   sqft_living15 sqft_lot     
  # sqft_lot15    view          waterfront    yr_built      zipcode 
  
  # Lets plot the size vs. the Relative error, visualize cross-validation results:
  # rememeber that we used 10 partitions for the cross validation (xval=10)
  png(paste(output_results,"CV_regression_rpart_tree_plot.png",sep = ""), width = 800, height = 600, units = "px", pointsize = 20)
  plotcp(data.dt)
  dev.off()
  # We can see that the minimum error happen in tree with size 56.
  # The Complexity Parameter (CP) value for this tree is:
  get_CP <- function(cptable){
    cptable = as.data.frame(cptable)
    ind = which.min(cptable$xerror)
    
    xerr <- cptable$xerror[ind]
    xstd <- cptable$xstd[ind]
    
    i = 1
    while (cptable$xerror[i] > xerr+xstd) i = i+1
    CP = cptable$CP[i]
    # SELECTED VALUE OF THE COMPLEXITY PARAMETER
    return(CP)
  }

  (CP = get_CP(data.dt$cptable))
  # CP = 0.001713321
  
  tree.index = match(CP,data.dt$cptable)
  r2 <- 1 - data.dt$cptable[tree.index,4]

  error = Prediction.errors.from.r2(r2,training_data,training_data$target)
  va.se <- error[["se"]]
  va.MSE <-  error[["mse"]]
  va.RMSE <-  error[["rmse"]]  
  va.NRMSE  <-  error[["nrmse"]]
  va.R2 <-  error[["r2"]]
  
  # Now lets plot the training tree:
  png(paste(output_results,"regression_rpart_tree_training.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  rpart.plot(data.dt, type = 4, extra = 101)
  dev.off()
  
  #next need to prune the tree
  set.seed(2018)
  prune.dt <- prune(data.dt,cp=CP)
  
  
  # Plot the importance of variables in the prediction.
  png(paste(output_results,"regression_rpart_pruneVarImpo.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  barplot(prune.dt$variable.importance, col=rgb(0.2,0.4,0.6,0.6), las=2 ,main="Variable importance")
  dev.off()
  
  # Lets use the prune tree to predit the training error data
  tr.pred=predict(prune.dt, data=training_data,type="vector")
  error = Prediction.errors(tr.pred,training_data$target)
  tr.se <- error[["se"]]
  tr.MSE <-  error[["mse"]]
  tr.RMSE <-  error[["rmse"]]  
  tr.NRMSE <-  error[["nrmse"]]  
  tr.R2 <-  error[["r2"]] 
  
   
  # Lets Use the prune tree to predict the results of our testing data:
  te.pred = predict(prune.dt, newdata=testing_data,type="vector")
  error = Prediction.errors(te.pred,testing_data$target)
  te.se <- error[["se"]]
  te.MSE <-  error[["mse"]]
  te.RMSE <-  error[["rmse"]]  
  te.NRMSE <-  error[["nrmse"]]  
  te.R2 <-  error[["r2"]]
  

  
  # SE.learn = 0.5*sum((pred_learn - (training_data$target))^2)
  # MSE.learn = (mean((pred_learn - (training_data$target))^2))
  # NRMSE.learn = sqrt(MSE.learn)
  # MSE.learn
  # 
  # SE.test = 0.5*sum((pred_test - (testing_data$target))^2)
  # MSE.test = (mean((pred_test - (testing_data$target))^2))
  # NRMSE.test = sqrt(MSE.test) 
  # MSE.test
  # 
  # #writeResults("regression_tree_rpartlib", "", dataset_id, "regression_tree_rpartlib", SE.learn, MSE.learn,NRMSE.learn, SE.test, MSE.test, NRMSE.test)
  
  #printResult("regression_tree_rpartlib", dataset_id , RMSE.learn, RMSE.test)
  
  # write results to results file.  
  function_script <- "regression_tree_rpartlib"
  comment <- comment
  Input <- dataset_id
  Model <- "regression_tree_rpartlib"
  
  result <- cbind(
    function_script, 
    comment, Input, Model, 
    tr.se, tr.MSE, tr.RMSE, tr.NRMSE, tr.R2,
    va.se, va.MSE, va.RMSE, va.NRMSE, va.R2,
    te.se, te.MSE, te.RMSE, te.NRMSE, te.R2
  )
  
  write.table(
    result, 
    file=filename, 
    append = TRUE, 
    sep=";", 
    col.names = FALSE, 
    row.names = FALSE)
  return(va.NRMSE)
}



##
## Using tree library
##

# The miss-classification error for the TRAINING data is 74%. therefore,we decided to not continue exploring this path
classification_treelib_tree_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Trees/")
{
  priceCat<-cut(data$target, c(0,250000,300000,350000,400000,450000,525000,600000,750000,1000000,7700001), right=FALSE, labels=c(1:10)) 
  
  table(priceCat)
  data$priceCat <- as.factor(priceCat)
  
  df1 <- train.test.set(data)
  training_data = df1[[1]]
  testing_data = df1[[2]]
  
  set.seed(2018)
  data.tree = tree(priceCat~.-target, data = training_data)
  
  summary(data.tree)
  # Residual mean deviance:  3.909 = 59110 / 15120
  # Variables actually used in tree construction:
  # "grade"       "sqft_living" "yr_built"
  # number of tarminal nodes: 6
  # Misclassification error rate: 0.7489 = 11330 / 15129 
  # as we can see our training error is huge (74%)
  
  # Now lets plot the training tree:
  png(paste(output_results,"classification_treelib_tree_training.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  plot(data.tree)
  text(data.tree ,pretty =0)
  dev.off()
  
  
  # Lets run cross validation on the given tree
  set.seed(2018)
  cv.data.tree = cv.tree(data.tree, K=10, FUN = prune.misclass)
  
  cv.data.tree
  
  #since we have such a huge training error we decided to not continue exploring this path
  
}

regression_treelib_tree_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Trees/")
{
  df1 <- train.test.set(data)
  training_data = df1[[1]]
  testing_data = df1[[2]]
  
  set.seed(2018)
  data.tree = tree(target~., data = training_data)
  
  #summary(data.tree)
  # Residual mean deviance:   5.075e+10 = 7.672e+14 / 15120
  # Variables actually used in tree construction:
  # "grade"       "yr_built"    "sqft_living" "waterfront" 
  # number of tarminal nodes: 11
  
  # Now lets plot the training tree:
  png(paste(output_results,"regression_treelib_tree_training.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  plot(data.tree)
  text(data.tree ,pretty =0)
  dev.off()
  
  
  # Lets run cross validation on the given tree
  set.seed(2018)
  cv.data.tree = cv.tree(data.tree)
  
  cv.data.tree
  png(paste(output_results,"regression_treelib_cvtree.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  plot(cv.data.tree)
  dev.off()
  # we can see that the best tree is with 9 nodes, therefore, we will not prune the tree.
  
  minErroridx = which(cv.data.tree$dev == min(cv.data.tree$dev))
  
  bestTreeSize = cv.data.tree$size[minErroridx]
  
  prune.data.tree =prune.tree(data.tree ,best = bestTreeSize)
  
  png(paste(output_results,"regression_treelib_pronetree.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  plot(prune.data.tree)
  text(prune.data.tree ,pretty =0)
  dev.off()
  
  # Lets Use the tree to predict the results of our testing data:
  pred_test = predict(prune.data.tree, newdata=testing_data)
  
  # Lets use the prune tree to predit the training error data
  pred_learn=predict(prune.data.tree, data=training_data)
  
  SE.learn = 0.5*sum((pred_learn - (training_data$target))^2)
  MSE.learn = (mean((pred_learn - (training_data$target))^2))
  NRMSE.learn = sqrt(MSE.learn)
  #MSE.learn

  SE.test = 0.5*sum((pred_test - (testing_data$target))^2)
  MSE.test = (mean((pred_test - (testing_data$target))^2))
  NRMSE.test = sqrt(MSE.test) 
  #MSE.test
  #printResult("regression_tree_treelib", dataset_id , RMSE.learn, RMSE.test)
  
  writeResults("regression_tree_treelib", "", dataset_id, "regression_tree_treelib", SE.learn, MSE.learn,NRMSE.learn, SE.test, MSE.test, NRMSE.test)
  
}


regression_randomforest <- function(data, dataset_id, output_results = "../Analysis Results/Trees/")
{
  
  df1 <- train.test.set(data)
  training_data = df1[[1]]
  testing_data = df1[[2]]
  
  set.seed(2018)
  (ntrees <- round(10^seq(1,3,by=0.2)))
  rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
  colnames (rf.results) <- c("ntrees", "RMSE")
  rf.results[,"ntrees"] <- ntrees
  rf.results[,"RMSE"] <- 0
  
  ii <- 1
  
  for (nt in ntrees)
  { 
    print(nt)
    set.seed(2018)
    model.rf <- randomForest(target ~., data=training_data, ntree=nt, proximity=FALSE)
    # get the RMSE
    rf.results[ii,"RMSE"] = sqrt(model.rf$mse[nt])
    
    ii <- ii+1
  }
  nt
  rf.results
  
  # choose best value of 'ntrees'
  
  lowest.mse.error <- as.integer(which.min(rf.results[,"RMSE"]))
  (ntrees.best <- rf.results[lowest.mse.error,"ntrees"])
  
  ## Now refit the RF with the best value of 'ntrees'
  set.seed(2018)
  model.rf3 <- randomForest(target ~., data=training_data, ntree=ntrees.best,proximity=FALSE, importance=TRUE)
  
  # let's compute the final test error:
  
  # Lets Use the RF to predict the results of our testing data:
  pred_test = predict(model.rf3, newdata=testing_data)
  
  # Lets use the RF to predit the training error data
  pred_learn=predict(model.rf3, data=training_data)
  
  SE.learn = 0.5*sum((pred_learn - (training_data$target))^2)
  MSE.learn = (mean((pred_learn - (training_data$target))^2))
  NRMSE.learn = sqrt(MSE.learn)
  #MSE.learn
  # 132976.3

  SE.test = 0.5*sum((pred_test - (testing_data$target))^2)
  MSE.test = (mean((pred_test - (testing_data$target))^2))
  NRMSE.test = sqrt(MSE.test) 
  #MSE.test
  # 128273.6
  #printResult(paste("Random forest - tree size: ", ntrees.best) , dataset_id , RMSE.learn, RMSE.test)

  
  writeResults("Random forest", paste("tree size: ", ntrees.best), dataset_id, "Random forest", SE.learn, MSE.learn,NRMSE.learn, SE.test, MSE.test, NRMSE.test)
}