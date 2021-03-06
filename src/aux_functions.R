
load_install_packages <- function(packageName)
{
  if(!require(packageName, character.only=TRUE)){
    print(paste(packageName, "not exist, installation in process.." ))
    install.packages(packageName)
    if(!require(packageName, character.only=TRUE))
      print(paste("failed to install:", packageName ))
  }
}

load_packages <- function()
{
  load_install_packages("FactoMineR")
  load_install_packages("pls")
  load_install_packages("mvtnorm")
  load_install_packages("MVN")
  load_install_packages("mvnormtest")
  load_install_packages("TunePareto") # for generateCVRuns()
  load_install_packages("tree")
  load_install_packages("chemometrics")
  load_install_packages("MASS")
  load_install_packages("corrplot")
  load_install_packages("randomForest")
  load_install_packages("rpart")
  load_install_packages("rpart.plot")
  load_install_packages("lars")
  load_install_packages("glmnet")
  load_install_packages("xtable")
  
}

source_scripts <- function()
{
  source("01_eda.R")
  
  source("02_datapreparation_david.R")
  source("02_datapreparation_manual.R")
  
  source("03_unsupervised_analysis.R")
  
  source("04_linear_regressions_fitting.R")
  source("04_ridge_regression_fitting_methods.R")
  source("04_lasso_regression_fitting_methods.R")
  source("04_trees_methods_fitting.R")
  source("04_pcr_fitting.R")
  
  source("Cross_validation.R")
  source("Test_error.R")
  source("SFS.R")
  source("SBS.R")
  
  
}

# 2. Utils--------------------
load.models.list <- function(){
  models.list <- c(linear_regression_fitting02,glmnet.ridge, glmnet.lasso,regression_rpart_tree_fitting,regression_randomforest)
  #lars.lasso,
  
  models.list2 <- c(linear_regression_fitting02,glmnet.ridge, glmnet.lasso,pcr_model,regression_rpart_tree_fitting,regression_randomforest)
  #lars.lasso,
  
  assign("models.list",models.list,.GlobalEnv)
  assign("models.list2",models.list2,.GlobalEnv)

}

get.featureset.names <- function(folderpath){
  #file.list <- dir(folderpath)
  file.list <- Filter(function(x){substring(x,nchar(x)-3,nchar(x))==".Rda"},dir(folderpath))
  return(lapply(file.list,function(x){gsub(".Rda","",x)}))
}


model.selection.func <- function(func,folder=folder,id="O3_feature_selection" ){
  featurespace = get.featureset.names(folder)
  
  for(i in 1:length(featurespace)){
    
    filename = paste(folder,featurespace[i],".Rda",sep="")
    newobjects = load(file=filename)
    auxvar <- get(newobjects[1])
    do.call(func,args=list(auxvar, dataset_id = id, comment=featurespace[i]))
    #glmnet.ridge(auxvar, dataset_id = "O3_feature_selection", comment=featurespace[i])
    rm(auxvar)
  }
}


Prediction.errors <- function(pred, t){
  tr.se <- 0.5*sum((pred - t)^2)
  tr.MSE <- mean((pred - t)^2)
  tr.RMSE <- sqrt(tr.MSE)  
  tr.NRMSE <- sqrt(tr.MSE/var(t))  
  tr.R2 <- 1 - tr.NRMSE^2
  return(list(se=tr.se, mse=tr.MSE, rmse=tr.RMSE, nrmse=tr.NRMSE, r2=tr.R2))
}


Prediction.errors2 <- function(mse,x,t){
  tr.se <- mse*nrow(x)/2
  tr.MSE <- mse
  tr.RMSE <- sqrt(tr.MSE)  
  tr.NRMSE <- sqrt(tr.MSE/var(t))  
  tr.R2 <- 1 - tr.NRMSE^2
  return(list(se=tr.se, mse=tr.MSE, rmse=tr.RMSE, nrmse=tr.NRMSE, r2=tr.R2))
}

Prediction.errors.from.mse <- function(mse,x,t){
  tr.se <- mse*nrow(x)/2
  tr.MSE <- mse
  tr.RMSE <- sqrt(tr.MSE)  
  tr.NRMSE <- sqrt(tr.MSE/var(t))  
  tr.R2 <- 1 - tr.NRMSE^2
  return(list(se=tr.se, mse=tr.MSE, rmse=tr.RMSE, nrmse=tr.NRMSE, r2=tr.R2))
}

Prediction.errors.from.nrmse <- function(nrmse,x,t){
  tr.RMSE <- nrmse  
  tr.MSE <- nrmse^2
  tr.se <- tr.MSE*nrow(x)/2
  tr.NRMSE <- sqrt(tr.MSE/var(t))  
  tr.R2 <- 1 - tr.NRMSE^2
  return(list(se=tr.se, mse=tr.MSE, rmse=tr.RMSE, nrmse=tr.NRMSE, r2=tr.R2))
}

Prediction.errors.from.r2 <- function(r2,x,t){
  
  tr.R2 <- r2
  tr.NRMSE <- sqrt(1 - tr.R2)
  tr.MSE <- tr.NRMSE^2*var(t)
  tr.se <- tr.MSE*nrow(x)/2
  tr.RMSE <- sqrt(tr.MSE)  
  return(list(se=tr.se, mse=tr.MSE, rmse=tr.RMSE, nrmse=tr.NRMSE, r2=tr.R2))
}

create.Latex.Table <- function(){
  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  
  results = read.csv("../Analysis Results/model_results.csv", header = TRUE, sep = ";")
  
  variables <- c("Input", "Model","Training.RMSE", "Validation.RMSE", "Testing.RMSE")
  subset = results[,variables]
  
  subset = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  
  colnames(subset) <- c("Model","Feature set","Training.RMSE", "Validation.RMSE", "Testing.RMSE")
  
  tli.table <- xtable(subset)
  align(tli.table) <- rep("c", ncol(tli.table)+1)
  
  print(tli.table, file = "../Analysis Results/model_results.tex")

}

create.Latex.Table2 <- function(filein = "../Analysis Results/model_results.csv",
                                variables = c("Model","Training.RMSE", "Validation.RMSE", "Testing.RMSE"),
                                fileout  = "../Analysis Results/model_results.tex"){
  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  
  results = read.csv(filein, header = TRUE, sep = ";")
  
  subset = results[,variables]
  
  # subset = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  # colnames(subset) <- c("Model","Feature set","Training.RMSE", "Validation.RMSE", "Testing.RMSE")
  
  tli.table <- xtable(subset)
  align(tli.table) <- rep("c", ncol(tli.table)+1)
  
  print(tli.table, file = fileout)
  
}


create.Latex.Table3 <- function(filein = "../Analysis Results/model_results.csv",
                               fileout  = "../Analysis Results/model_results.tex"){
  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  
  results = read.csv(filein, header = TRUE, sep = ";")
  
  variables <- c("Comment", "Model", "Validation.NRMSE", "Testing.NRMSE")
  subset = results[,variables]
  
  subset = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  
  colnames(subset) <- c("Model","Feature set", "Validation.NRMSE", "Testing.NRMSE")
  
  # we also need to sort by Validation.NRMSE
  
  tli.table <- xtable(subset)
  # we need cllccc
  align(tli.table) <- c(paste("cll",paste(rep("c", ncol(tli.table)-2), collapse = ""), sep="", collapse=""))
  
  print(tli.table, file = fileout)
  
}

create.Latex.Table4 <- function(filein = "../Analysis Results/model_results.csv",
                                fileout  = "../Analysis Results/model_results.tex"){
  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  
  results = read.csv(filein, header = TRUE, sep = ";")
  
  variables <- c("Input" ,"Model", "Validation.NRMSE", "Testing.NRMSE")
  subset = results[,variables]
  
  subset = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  
  colnames(subset) <- c("Model","Feature set", "Validation.NRMSE", "Testing.NRMSE")
  
  # we also need to sort by Validation.NRMSE
  
  tli.table <- xtable(subset)
  # we need cllccc
  align(tli.table) <- c(paste("cll",paste(rep("c", ncol(tli.table)-2), collapse = ""), sep="", collapse=""))
  
  print(tli.table, file = fileout)
  
}


create.Latex.Table4.sorted  <- function(filein = "../Analysis Results/model_results.csv",
                                        fileout  = "../Analysis Results/model_results.tex"){
  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  
  results = read.csv(filein, header = TRUE, sep = ";")
  
  variables <- c("Comment", "Model", "Validation.NRMSE", "Testing.NRMSE")
  subset = results[,variables]
  
  subsetdf = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  
  colnames(subset) <- c("Model","Feature set", "Validation.NRMSE", "Testing.NRMSE")
  
  # we also need to sort by Validation.NRMSE
  df2 <- subsetdf[order(subsetdf$Validation.NRMSE),]
  subsetdf <- df2
  
  
  tli.table <- xtable(subsetdf)
  # we need cllccc
  align(tli.table) <- c(paste("cll",paste(rep("c", ncol(tli.table)-2), collapse = ""), sep="", collapse=""))
  
  print(tli.table, file = fileout)
  
}


create.Latex.Table5.sorted  <- function(filein = "../Analysis Results/model_results.csv"){
  results = read.csv(filein, header = TRUE, sep = ";")
  
  variables <- c("Comment", "Model", "Validation.NRMSE", "Training.NRMSE","Testing.NRMSE")
  subset = results[,variables]
  
  subsetdf = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  
  colnames(subset) <- c("Model","Feature set", "Validation.NRMSE", "Training.NRMSE", "Testing.NRMSE")
  
  # we also need to sort by Validation.NRMSE
  df2 <- subsetdf[order(-subsetdf$Validation.NRMSE),]
  
  df3 = df2[-which(df2$`subset[, 2]`=="simple linear regression"),]
  plot(1:nrow(df3), df3$Validation.NRMSE, type="l", col = "blue", xlab = "", ylab = "NRMSE")
   
  lines(1:nrow(df3), df3$Training.NRMSE, type="l", pch=22, col="green")
  
  lines(1:nrow(df3), df3$Testing.NRMSE, type="l", pch=22, col="orange")
  
  x = 1:nrow(df3)
  # draw an axis on the left
  modelnames = paste(df3$`subset[, 2]`, df3$Comment)
  axis(1, at=x,labels=modelnames, col.axis="red", las=2)
  
  legend("topright", legend=c("Validation Error", "Training Error", "Testing Error"), col=c("blue", "green", "orange"), lty=1, cex=0.8)
  
}

create.Latex.Table6.SFS  <- function(filein = "../Analysis Results/SFS/_regression_randomforest_featureset_logs_sfs.t.csv"){
  results = read.csv(filein, header = TRUE, sep = ";")
  
  variables <- c("Input", "Model", "Validation.NRMSE")
  subset = results[,variables]
  
  subsetdf = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  
  colnames(subsetdf) <- c("Model","Feature set", "Validation.NRMSE")
  op <- par(mar=c(12,4,4,2)) # the 10 allows the names.arg below the barplot
  
  df3 = subsetdf

  plot(1:nrow(df3), df3$Validation.NRMSE,pch=20, col = "blue", xlab = "", ylab = "NRMSE")
  
  #lines(1:nrow(df3), df3$Training.NRMSE, type="l", pch=22, col="green")
  
  #lines(1:nrow(df3), df3$Testing.NRMSE, type="l", pch=22, col="orange")
  
  x = 1:nrow(df3)
  # draw an axis on the left
  #modelnames = paste(df3$`subset[, 2]`, df3$Comment)
  #text(x = x, y=0 ,labels=df3$`Feature set`, col="red", las=2, srt=45)
  #axis(1, at=x,labels=df3$`Feature set`, col.axis="red", las=2, srt=45)
  
  lab = rep("", length(df3$`Feature set`))
  tmp = df3$`Feature set`
  lab[5] = (tmp[5])
  lab[length(df3$`Feature set`)] = tmp[length(df3$`Feature set`)]
  text(1:nrow(df3), df3$Validation.NRMSE, labels = lab, col="red")
  abline(v=c(1.5,9.5,16.5,22.5, 27.5,31.5,34.5, 36.5), col=c("orange"), lty=c(2), lwd=c(2))
  #legend("topright", legend=c("Validation Error", "Training Error", "Testing Error"), col=c("blue", "green", "orange"), lty=1, cex=0.8)

}



create.Latex.final  <- function(filein = "../Analysis Results/final.csv",
                                        fileout  = "../Analysis Results/final.tex"){
  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  
  results = read.csv(filein, header = TRUE, sep = ";")
  
  variables <- c("Input", "Model", "Validation.NRMSE", "Training.NRMSE","Testing.NRMSE")
  subset = results[,variables]
  
  subsetdf = cbind(subset[,2], subset[,-2]) # we want to have "Model" in the first column.
  
  colnames(subsetdf) <- c("Model","Feature set", "Validation.NRMSE","Training.NRMSE", "Testing.NRMSE")
  
  # we also need to sort by Validation.NRMSE
  df2 <- subsetdf[order(subsetdf$Validation.NRMSE),]
  subsetdf <- df2
  
  
  tli.table <- xtable(subsetdf)
  # we need cllccc
  align(tli.table) <- c(paste("cll",paste(rep("c", ncol(tli.table)-2), collapse = ""), sep="", collapse=""))
  
  print(tli.table, file = fileout)
  
}
#create.Latex.final()
