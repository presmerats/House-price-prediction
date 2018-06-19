

# 1. Test and Train sets-----------------------------------

train.test.set <- function(x){
  #shuffling the data
  set.seed (1714)
  x <- x[sample.int(nrow(x)),]
  
  # 30% test, 70% train
  i <- round(nrow(x)*0.3)
  test <- x[1:i,]
  train <- x[(i+1):nrow(x),]
  
  return(list(test,train))
}


