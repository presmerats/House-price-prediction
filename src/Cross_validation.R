# NOTE: draft version, still working in progress

library(TunePareto) # for generateCVRuns()

# MODEL.CV function receive data frame and the k number of groups you wish to have
# in your cross validation.
# TODO: 1. in order to use it you must change "modelTraining()" to your own model
# TODO: 2. in case you don't use the "predict()" function for your model you need to chenge it here as well.

MODEL.CV <- function (data, k=10)
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
    my.da.TR <- modelTraining()
    
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
    pred.va <- predict(my.da.TR, newdata=data[va,])$class
    
    tab <- table(data[va,]$target, pred.va)
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    cv.results[j,"fold"] <- j
  }
  ## have a look at the results ...
  cv.results
  mean(cv.results[,"VA error"])
}



## MODEL.CV.OVER.Ks - will run MODEL.CV with diffenet values of partitions (k).
## NOTE: you must change the MODEL.CV to run your own model.
## The function will plot the average CV error vs. number of partitions (K).
MODEL.CV.OVER.Ks <- function (data, kflod=10) {
  the.Ks <- 2:kflod
  res <- vector("numeric",length(the.Ks)+1)
  res[1] <- NA
  for (k in the.Ks) res[k] <- MODEL.CV(data, k)
  ## let us see the results
  plot(res,type="b",xlab="Value of k",ylab="average CV error", ylim=c(0.22,0.3), xaxt="n")
  axis(1, at=1:20,labels=1:20, las=2)
  grid()
}

