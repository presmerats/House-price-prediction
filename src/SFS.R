#Sequential Forward Selection

SFS <- function(data, dataset_id, baseset = c(1:10), extra = c(11:20), output_results = "../Analysis Results/SBS/", comment = "testing"){
  #print(extra)
  #print(baseset)
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]]
  model.res <- NULL
  used = c()
  minBestError = lars.lasso(data[,baseset], dataset_id = dataset_id,filename="../Analysis Results/sfs.csv" )
  totIter = length(extra)
  for (j in 1:totIter){
    model.res <- NULL
    for(i in extra){
      #print(i)
      varList = c(baseset, used, i)
      dataid = paste(dataset_id, paste(varList, collapse="_"))
      model.res[i] = lars.lasso(data[,varList], dataid,filename="../Analysis Results/sfs.csv" )
    }
    bestNextIdx = which.min(model.res)
    #print(model.res)
    #print(minBestError)
    
    #check if adding new feature will improve our error
    # if not, return
    if(minBestError <  model.res[bestNextIdx] ) break
      
    minBestError = model.res[bestNextIdx]
    used = c(used,bestNextIdx)
    extra = extra[-which(extra %in% bestNextIdx)]
  }
  #print(used)
  return( sort(c(baseset, used)) )
}
  

#SBS(raw_continuous_dataset, dataset_id = "raw_continuous_vars", baseset = c(1:7), extra = c(8:12))


# Before calling it make sure we add headers!!!!!!!!!!
#!!!!!!!!!!!!!!********************!!!!!!!!!!
#create.Latex.Table4(filein = "../Analysis Results/sfs.csv", fileout  = "../Analysis Results/sfs.tex")
