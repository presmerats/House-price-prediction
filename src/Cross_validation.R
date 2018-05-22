# NOTE: draft version, still working in progress


# MODEL.CV function receive data frame and the k number of groups you wish to have
# in your cross validation.
# TODO: 1. in order to use it you must change "modelTraining()" to your own model
# TODO: 2. in case you don't use the "predict()" function for your model you need to chenge it here as well.

MODEL.CV <- function (data, k=10, method, prediction_method = "generic")
{

  CV.folds <- generateCVRuns(data$target, ntimes=1, nfold=k, stratified=TRUE)
  
  cv.results <- matrix (rep(0,4*k),nrow=k)
  colnames (cv.results) <- c("k","fold","TR error","VA error")
  
  cv.results[,"TR error"] <- 0
  cv.results[,"VA error"] <- 0
  cv.results[,"k"] <- k


  for (j in 1:k)
  {
    # get VA data
    va <- unlist(CV.folds[[1]][[j]])
    
    # train on Training data
    # ----------------------------------
    # replace "modelTraining" with the calling the model you want to train
    #my.da.TR <- modelTraining()
    
    if(method == "linear_regression")
    {
      my_lr_tr <- lm(target ~ ., data=data[-va,])
      # predict on training set
      tr.pred <- predict(my_lr_tr, data = data[-va,])
      # predict on test set
      va.pred <- predict.lm(my_lr_tr, newdata  = data[va,])
    } else if (method == "regression_tree")
    {
      browser()
      
      my_tree_tr <- tree(target ~., data=data[-va,])
      # predict on training set. 
      
      
    }
    
    ## get training and valiation errors
    
    # training error:
    tr.se <- sum(((tr.pred - data[-va,]$target)^2))*0.5
    tr.nrmse <- sqrt((2*tr.se)/nrow(data[-va,]))
    cv.results[j,"TR error"] <- tr.nrmse
    
    # validation error
    va.se <- sum(((va.pred - data[va,]$target)^2))*0.5
    va.nrmse <- sqrt((2*va.se)/nrow(data[va,]))
    cv.results[j,"VA error"] <- va.nrmse
    
    cv.results[j,"fold"] <- j
    
    }

  return (apply(cv.results[,c("TR error", "VA error")], 2, mean))
}



## MODEL.CV.OVER.Ks - will run MODEL.CV with diffenet values of partitions (k).
## NOTE: you must change the MODEL.CV to run your own model.
## The function will plot the average CV error vs. number of partitions (K).
MODEL.CV.OVER.Ks <- function (data, kflod=10, method, plot=FALSE) {
  # browser()
  
  the.Ks <- 1:kflod
  res <- matrix(0, nrow = length(the.Ks)+1, ncol = 2)
  res <- as.data.frame(res)
  names(res) <- c("Training error", "Validation error")
  for (k in the.Ks)
  {
    res[k,] <- MODEL.CV(data, kflod, method)
    cat("Finishing", k, "CV iteration \n")
  }## let us see the results
  
  if(plot == TRUE)
  {
  # plot(res,type="b",xlab="Value of k",ylab="average CV error", ylim=c(0.22,0.3), xaxt="n")
  # axis(1, at=1:kflod,labels=1:kflod, las=2)
  # grid()
  }

  result <- apply(res, 2, mean)

  return(result)
}

