printResult <- function(method, dataset_id, learnError, predError )
{
  writeLines(paste("Method: ", method, "\nDataset: ", dataset_id, "\nLearner RMSE: ", learnError, "\nPrediction RMSE: ", predError, "\n**********************************"))
}


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
  summary(data$price)
  hist(data$price)
  priceCat<-cut(data$price, c(0,250000,300000,350000,400000,450000,525000,600000,750000,1000000,7700001), right=FALSE, labels=c(1:10)) #labels=c("low", "economic", "middle-class", "high","luxury"))
  
  table(priceCat)
  data$priceCat <- as.factor(priceCat)
  
  train = round(nrow(data)*0.7)
  
  trainidx = 1:train
  testidx  = (train+1):nrow(data)
  
  training_data = data[trainidx,]
  testing_data  = data[testidx,]
  set.seed(2018)
  data.dt = rpart(priceCat~.-price, data = training_data, control = rpart.control(cp=0.001, xval=10))
  
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
  
  # Now lets plot the training tree:
  png(paste(output_results,"classification_rpart_tree_training.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  rpart.plot(data.dt, type = 4, extra = 101)
  dev.off()
  
  #next need to prune the tree
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
  printResult("Classification_tree_rpartlib\naccuracy, not RMSE\n", dataset_id , accuracy.learn, accuracy.test)
  
}

regression_rpart_tree_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Trees/")
{
  train = round(nrow(data)*0.7)
  
  trainidx = 1:train
  testidx  = (train+1):nrow(data)
  
  training_data = data[trainidx,]
  testing_data  = data[testidx,]
  set.seed(2018)
  data.dt = rpart(price~., data = training_data, method = "anova", control = rpart.control(cp=0.001, xval=10))
  
  printcp(data.dt)
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
  
  # Now lets plot the training tree:
  png(paste(output_results,"regression_rpart_tree_training.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  rpart.plot(data.dt, type = 4, extra = 101)
  dev.off()
  
  #next need to prune the tree
  prune.dt <- prune(data.dt,cp=CP)
  
  
  # Plot the importance of variables in the prediction.
  png(paste(output_results,"regression_rpart_pruneVarImpo.png",sep=""), width = 1000, height = 1000, units = "px", pointsize = 20)
  barplot(prune.dt$variable.importance, col=rgb(0.2,0.4,0.6,0.6), las=2 ,main="Variable importance")
  dev.off()
  
  
  # Compute the accuracy, precision, recall and AUC on the test individuals.
  
  # Lets Use the prune tree to predict the results of our testing data:
  pred_test = predict(prune.dt, newdata=testing_data,type="vector")
  
  # Lets use the prune tree to predit the training error data
  pred_learn=predict(prune.dt, data=training_data,type="vector")
  
  RMSE.learn = sqrt(mean((pred_learn - training_data$price)^2))
  RMSE.learn
  # 193091.2
  
  RMSE.test = sqrt(mean((pred_test - testing_data$price)^2))
  RMSE.test
  # 216156.8
  printResult("regression_tree_rpartlib", dataset_id , RMSE.learn, RMSE.test)
  
}



##
## Using tree library
##

# The miss-classification error for the TRAINING data is 74%. therefore,we decided to not continue exploring this path
classification_treelib_tree_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Trees/")
{
  summary(data$price)
  hist(data$price)
  priceCat<-cut(data$price, c(0,250000,300000,350000,400000,450000,525000,600000,750000,1000000,7700001), right=FALSE, labels=c(1:10)) 
  
  table(priceCat)
  data$priceCat <- as.factor(priceCat)
  
  train = round(nrow(data)*0.7)
  
  trainidx = 1:train
  testidx  = (train+1):nrow(data)
  
  training_data = data[trainidx,]
  testing_data  = data[testidx,]
  set.seed(2018)
  data.tree = tree(priceCat~.-price-lat-long-date-id, data = training_data)
  
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
  cv.data.tree = cv.tree(data.tree, K=10, FUN = prune.misclass)
  
  cv.data.tree
  
  #since we have such a huge training error we decided to not continue exploring this path
  
}

regression_treelib_tree_fitting <- function(data, dataset_id, output_results = "../Analysis Results/Trees/")
{
  train = round(nrow(data)*0.7)
  
  trainidx = 1:train
  testidx  = (train+1):nrow(data)
  
  training_data = data[trainidx,]
  testing_data  = data[testidx,]
  
  set.seed(2018)
  data.tree = tree(price~., data = training_data)
  
  summary(data.tree)
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
  
  RMSE.learn = sqrt(mean((pred_learn - (training_data$price))^2))
  RMSE.learn
  # 225192.8
  # in case we used log10(price)
  # 0.1512038
  
  RMSE.test = sqrt(mean((pred_test - (testing_data$price))^2))
  RMSE.test
  # 236536.4
  # in case we used log10(price)
  # 0.1494284
  printResult("regression_tree_treelib", dataset_id , RMSE.learn, RMSE.test)
}