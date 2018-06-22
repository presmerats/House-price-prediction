#Sequential backward Selection

SBS <- function(func, data, dataset_id, baseset = c(1:10), extra = c(11:20), output_results = "../Analysis Results/SBS/", filename="sbs.csv", method = "unknownMethod"){
  #print(extra)
  #print(baseset)
  output = paste(output_results, method, dataset_id, filename, sep = "_")
  df1 <- train.test.set(data)
  train = df1[[1]]
  test = df1[[2]]
  model.res <- NULL
  removed = c()
  #print(baseset)
  minBestError = do.call(func,args=list(data, dataset_id = dataset_id,filename=output ))
  totIter = length(extra)
  for (j in 1:totIter){
    model.res <- NULL
    for(i in extra){
      #print(i)
      varList = c(removed, -i)
      dataid = paste(dataset_id, paste(varList, collapse="_"))
      model.res[i] = do.call(func,args=list(data[,varList], dataid,filename=output ))
      # model.res[i] = lars.lasso(data[,varList], dataid,filename="../Analysis Results/sfs.csv" )
    }
    bestNextIdx = which.min(model.res)
    #print(model.res)
    #print(minBestError)
    
    #check if adding new feature will improve our error
    # if not, return
    if(minBestError <  model.res[bestNextIdx] ) break
      
    minBestError = model.res[bestNextIdx]
    removed = c(removed,-bestNextIdx)
    extra = extra[-which(extra %in% bestNextIdx)]
  }
  #print(used)
  variableIndex =  sort(removed)
  head(data[,variableIndex])
  #save(file = paste("../Dataset/SFS_", dataset_id, method, ".Rda", sep = ""), data[,variableIndex])
  return(data[,variableIndex])
}
  

#SFS(lars.lasso, raw_continuous_dataset, dataset_id = "raw_continuous_vars", baseset = c(1:7), extra = c(8:12), filename = "lars_raw_continuous_vars.csv")

#SFS(glmnet.lasso, raw_continuous_dataset, dataset_id = "raw_continuous_vars", baseset = c(1:7), extra = c(8:12), filename = "glmnet_lasso_raw_continuous_vars.csv")


#SFS(glmnet.lasso, raw_continuous_dataset, dataset_id = "raw_continuous_vars", baseset = c(1:7), extra = c(8:12), filename = "glmnet_lasso_raw_continuous_vars.csv")

#bla = SFS(glmnet.lasso, featureset_base_ratios, dataset_id = "featureset_base_ratios", baseset = c(1:13), extra = c(14:26), method = "glmnet_lasso")

#featureset_base_ratios
# Before calling it make sure we add headers!!!!!!!!!!
#!!!!!!!!!!!!!!********************!!!!!!!!!!
#create.Latex.Table4(filein = "../Analysis Results/sfs.csv", fileout  = "../Analysis Results/sfs.tex")
#create.Latex.Table4(filein = "../Analysis Results/SFS/glmnet_lasso_featureset_base_ratios.csv", fileout  = "../Analysis Results/SFS/glmnet_lasso_featureset_base_ratios.tex")
