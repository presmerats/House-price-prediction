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
      
      # compute training error NRMSE
      tr.pred <- predict(my_lr_tr, data = data[-va,])
      tr.error <- sqrt(sum((tr.pred - data[-va,]$target)^2)/nrow(data[-va,]))
      cv.results[j,"TR error"] <- tr.error
      
      # compute testing error NRMSE
      va.pred <- predict(my_lr_tr, newdata  = data[va,])
      va.error <- sqrt(sum((va.pred - data[va,]$target)^2)/nrow(data[va,]))
      cv.results[j,"VA error"] <- va.error
      
      cv.results[j,"fold"] <- j
    }
    
    
    # predict on the Training data and calculate the training error
    # -------------------------------------------------------------
    # change the next line only!
    #pred.va <- modelPredict(my.da.TR)$class
    # uncomment the next two lines if you wish to check the the training error
    #tab <- table(data[-va,]$target, pred.va)
    #cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    # predict VA data
    # ------------------------------------------
    # Change here if you wish to use diffenet predict function!
    # pred.va <- predict(my.da.TR, newdata=data[va,])$class
    # 
    # tab <- table(data[va,]$target, pred.va)
    # cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    # 
    # cv.results[j,"fold"] <- j
  }
  ## have a look at the results ...
  # browser()
  cv.results
  apply(cv.results[,c("TR error", "VA error")], 2, mean)
}



## MODEL.CV.OVER.Ks - will run MODEL.CV with diffenet values of partitions (k).
## NOTE: you must change the MODEL.CV to run your own model.
## The function will plot the average CV error vs. number of partitions (K).
MODEL.CV.OVER.Ks <- function (data, kflod=10, method) {
  # browser()
  
  the.Ks <- 1:kflod
  res <- matrix(0, nrow = length(the.Ks)+1, ncol = 2)
  for (k in the.Ks) 
  {
    res[k,] <- MODEL.CV(data, kflod, method)
    cat("Finishing", k, "iteration \n")
  }## let us see the results
  # plot(res,type="b",xlab="Value of k",ylab="average CV error", ylim=c(0.22,0.3), xaxt="n")
  # axis(1, at=1:kflod,labels=1:kflod, las=2)
  # grid()
  
  result <- apply(res, 2, mean)
  return(result)
}

