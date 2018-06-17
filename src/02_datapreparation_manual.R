####################################################################
# Machine Learning Project
# Pau Rodriguez

# Data pre-processing
# 16/06/2018
####################################################################

preprocessing.create.allfeatures <- function(data)
{
  attach(data)
  zipcode <- as.factor(zipcode)
  
  
  # set id as row.name
  #row.names(data) <- data$id # duplicated names!
  data$rownames <- data$id
  for (j in 1:nrow(data)){
    data$rownames[j] = paste(data$rownames[j],substr(as.character(data$date[j]), 1,6),sep="_") 
  }
  row.names(data) <- data$rownames
  
  # 2.missing data--------------------------------------------------
 
  
  # 3. outliers--------------------------------------------------
  
  # one individual with bedrooms=33 -> clear outlier
  data <- data[data$bedrooms!=33,]
  
  # mahalanobis distance 
  # we need the assumption of Gaussianity to be checked
  data.outliers = Moutlier(data[,c(3:8,12,15,18:21)],quantile = 0.975, plot = TRUE)
  data.outliers.sorted <- sort(data.outliers$rd, decreasing=TRUE)
  # we can remove the first 8 values in the data.outliers.rd
  
  #cleaning outliers
  data.orig <- data
  data <- data[data.outliers$rd<566,2:21]
  detach(data)
  attach(data)
  
  # 4. features--------------------------------------------------
  
  # transform to months
  datelevels <- levels(date)
  for(i in 1:length(datelevels)){
    datelevels[i] <- substr(strsplit(as.character(datelevels[i]),"T"), 8,9)
  }
  #datelevels
  buymonth <- data$date
  levels(buymonth) <- datelevels
  buymonth <- as.numeric(buymonth)
  
  for(i in 1:length(datelevels)){
    if (datelevels[i] == "01" | datelevels[i]=="02" | datelevels[i]=="03"){
      datelevels[i] <-"1"  
    } else if (datelevels[i] == "04" | datelevels[i]=="05" | datelevels[i]=="06"){
      datelevels[i] <-"2"  
    } else if (datelevels[i] == "07" | datelevels[i]=="08" | datelevels[i]=="09"){
      datelevels[i] <-"3"  
    } else if (datelevels[i] == "10" | datelevels[i]=="11" | datelevels[i]=="12"){
      datelevels[i] <-"4"  
    }
  }
  #datelevels
  buyquarter <- data$date
  levels(buyquarter) <- datelevels
  buyquarter <- as.numeric(buyquarter)
  
  
  
  
  # bathrooms manually generated ranges: <1, <2,<3,<4,>=4
  #table(bathrooms)
  #hist(bathrooms)
  #bathrooms.cat <- cut(bathrooms, breaks = c(seq(-0.254,0.25),8) )  # makes no sense
  bathrooms.cat <- cut(bathrooms, breaks = seq(-0.25,8,0.25) )  # better, but it's the same as the bathrooms var
  #head(bathrooms.cat)
  #table(bathrooms.cat)
  #head(bathrooms)
  #length(bathrooms.cat[is.na(bathrooms.cat)])
  #hist(as.numeric(bathrooms.cat))
  bathrooms.range <- as.numeric(bathrooms.cat)
  
  
  # bedrooms manually generated ranges: 
  #table(bedrooms)
  #hist(bedrooms)
  bedrooms.cat <- cut(bedrooms, breaks = c(-1,0:10,33) ) 
  #head(bedrooms.cat)
  #head(bedrooms)
  #table(bedrooms.cat)
  #length(bedrooms.cat[is.na(bedrooms.cat)])
  #hist(as.numeric(bedrooms.cat))
  bedrooms.range <- as.numeric(bedrooms.cat)
  
  # sqft_living
  #hist(sqft_living)
  #hist(sqft_living, breaks=100)
  #max(sqft_living)
  sqft_living.cat <- cut(sqft_living, breaks = c(seq(0,7000,10),13540 ) )
  sqft_living.range <- as.numeric(sqft_living.cat)
  #hist(sqft_living.range)
  
  # sqft_lot
  #hist(sqft_lot)
  #hist(sqft_lot, breaks=100)
  #summary(sqft_lot)
  sqft_lot.cat <- cut(sqft_lot, breaks = c(seq(519,20000,50),1651359 ) )
  #hist(as.numeric(sqft_lot.cat)) # really long tail!
  sqft_lot.cat <- cut(sqft_lot, breaks = c(seq(519,100000,50),1651359 ) )
  #hist(as.numeric(sqft_lot.cat)) # really long tail!
  sqft_lot.cat <- cut(sqft_lot, breaks = c(seq(519,1000000,50),1651359 ) )
  #hist(as.numeric(sqft_lot.cat)) 
  #length(sqft_lot[sqft_lot>50000])
  #length(sqft_lot[sqft_lot>100000])
  #length(sqft_lot[sqft_lot>1000000])
  #length(sqft_lot[sqft_lot>1500000])
  
  
  
  
  # binary vars
  is.renovated <- as.numeric(yr_renovated > 0 )
  
  # ratios
  bathrooms.bedrooms.ratio <- bathrooms/bedrooms
  # infinite values appear
  #table(bathrooms)
  #table(bedrooms)
  # change inf to 0
  bathrooms.bedrooms.ratio[bedrooms==0] <- 0
  #summary(bathrooms.bedrooms.ratio)
  
  bedrooms.sqft.living.ratio <- bedrooms/sqft_living
  bathroom.sqft.living.ratio <- bathrooms/sqft_living
  sqft.ratio <- sqft_lot/sqft_living
  
  floor.sqft.living.ratio <- floors/sqft_living
  sqft.living.floors.ratio <- sqft_living/floors
  floor.sqft.lot.ratio <- floors/sqft_lot
  floor.bedrooms.ratio <- floors/bedrooms
  floor.bedrooms.ratio[bedrooms==0] <- 0
  
  bedrooms.floors.ratio <- bedrooms/floors
  floor.bathrooms.ratio <- floors/bathrooms
  floor.bathrooms.ratio[bathrooms==0] <- 0
  
  
  
  
  # city center
  
  # distance from city center
  # distance from high roads
  # distance from services (hospital, school, entertainment, )
  
  
  # logarithms
  
  #names(data.new)
  log.bedrooms <- log10(bedrooms+1)
  log.bathrooms <- log10(bathrooms+1)
  log.sqft_living <- log10(sqft_living)
  log.sqft_lot <- log10(sqft_lot)
  log.floors <- log10(floors)
  log.condition <- log10(condition)
  log.grade <- log10(grade)
  log.sqft_above <- log10(sqft_above)
  log.sqft_basement <- log10(sqft_basement+1)
  log.lat <- log10(lat)
  log.long <- log10(long)
  log.sqft_living15 <- log10(sqft_living15)
  log.sqft_lot15 <- log10(sqft_lot15)
  log.bathrooms.range <- log10(bathrooms.range)
  log.bedrooms.range <- log10(bedrooms.range)
  log.bathrooms.bedrooms.ratio <- log10(bathrooms.bedrooms.ratio+1)
  log.bedrooms.sqft.living.ratio <- log10(bedrooms.sqft.living.ratio+1)
  log.bathroom.sqft.living.ratio <- log10(bathroom.sqft.living.ratio+1)
  log.sqft.ratio <- log10(sqft.ratio)
  log.floor.sqft.living.ratio <- log10(floor.sqft.living.ratio+1)
  log.floor.sqft.lot.ratio <- log10(floor.sqft.lot.ratio)
  log.floor.bedrooms.ratio <- log10(floor.bedrooms.ratio+1)
  log.floor.bathrooms.ratio <- log10(floor.bathrooms.ratio+1)
  log.sqft.living.floors.ratio <- log10(sqft.living.floors.ratio)
  log.bedrooms.floors.ratio <- log10(bedrooms.floors.ratio+1)
  #log.sdft_lot <- log10(sqft_lot)
  

  # current features
  data.new <- data.frame(
    data[,2:20],
    buymonth,
    buyquarter,
    bathrooms.range,
    bedrooms.range,
    is.renovated,
    bathrooms.bedrooms.ratio,
    bedrooms.sqft.living.ratio,
    bathroom.sqft.living.ratio,
    sqft.ratio,
    floor.sqft.living.ratio,
    floor.sqft.lot.ratio,
    floor.bedrooms.ratio,
    floor.bathrooms.ratio,
    sqft.living.floors.ratio,
    bedrooms.floors.ratio,
    log.bedrooms,
    log.bathrooms,
    log.sqft_living,
    log.sqft_lot,
    log.floors,
    log.condition,
    log.grade,
    log.sqft_above,
    log.sqft_basement,
    #log.lat,
    #log.long,
    log.sqft_living15,
    log.sqft_lot15,
    log.bathrooms.range,
    log.bedrooms.range,
    log.bathrooms.bedrooms.ratio,
    log.bedrooms.sqft.living.ratio,
    log.bathroom.sqft.living.ratio,
    log.sqft.ratio,
    log.floor.sqft.living.ratio,
    log.floor.sqft.lot.ratio,
    log.floor.bedrooms.ratio,
    log.floor.bathrooms.ratio,
    log.sqft.living.floors.ratio,
    log.bedrooms.floors.ratio
    
    
  )
  detach(data.new)
  detach(data)
  attach(data.new)
  
  
  
  # 5. statistical analysis--------------------------------------------------
  
  # Gaussianity
  


  #     Gaussianization
  library(MASS)
  
  gaussianize <- function(x,y,df) {
    bx <- boxcox(I(x+1) ~ . - y, data = df,
                 lambda = seq(-0.25, 0.25, length = 10))
    lambda <- bx$x[which.max(bx$y)]
    x.BC <- (x^lambda - 1)/lambda
    return(x.BC)
  }
  
  data.new2 <- data.new[,c(3:8,12,15,18:21)]
  #gaussianize(bathrooms,price,data.new2)
  log.sqft_lot15.BC <- gaussianize(log.sqft_lot15,price,data.new2)  # maybe
  #gaussianize(log.grade,price,data.new2)
  log.sqft_lot.BC <- gaussianize(log.sqft_lot,price,data.new2)   # maybe
  #gaussianize(bedrooms.floors.ratio,price,data.new2)
  #gaussianize(bedrooms.sqft.living.ratio,price,data.new2)
  bathroom.sqft.living.ratio.BC <- gaussianize(bathroom.sqft.living.ratio,price,data.new2) # maybe
  #gaussianize(floor.sqft.living.ratio,price,data.new2)
  sqft.living.floors.ratio.BC <- gaussianize(sqft.living.floors.ratio,price,data.new2)  # maybe
  sqft_living15.BC <- gaussianize(sqft_living15,price,data.new2)  # maybe
  #gaussianize(grade,price,data.new2)
  sqft_lot.BC <- gaussianize(sqft_lot,price,data.new2)  # better, but not gaussian
  #gaussianize(bathrooms,price,data.new2)
  
  # adding selected Gaussianized variables
  data.new <- data.frame(
    data.new,
    log.sqft_lot15.BC,
    log.sqft_lot.BC,
    #bathroom.sqft.living.ratio.BC,  # contains an -Inf
    sqft.living.floors.ratio.BC,
    sqft_living15.BC,
    sqft_lot.BC
  )
  
 return(data.new) 
}


featureset.original.nooutliers <- function(data){
  data.new <- preprocessing.create.allfeatures(data)
  data2 <- data.new[,c(1:24)]
  data2 <- data2[,c(-14,-15,-22,-23)]
  names(data2)[1] <- "target"
  
  return(data2)
}

featureset.all <- function(data){
  data.new <- preprocessing.create.allfeatures(data)
  names(data.new)[1] <- "target"
  
  return(data.new)
}

featureset.logs <- function(data){
  data.new <- preprocessing.create.allfeatures(data)
  data3 <- data.new[,c(1:24,35:45)]
  data3 <- data3[,c(1,7,8,13,16,17,20,21,24:35)]
  names(data3)[1] <- "target"
  
  return(data3)
}

featureset.ratios <- function(data){
  data.new <- preprocessing.create.allfeatures(data)
  data4 <- data.new[,c(1:34)]
  data4 <- data4[,c(-2,-3,-4,-5,-6,-15,-14,-22,-23)]
  names(data4)[1] <- "target"
  
  return(data4)
}

featureset.logratios <- function(data){
  data.new <- preprocessing.create.allfeatures(data)
  data5 <- data.new[,c(1:24,48:57)]
  data5 <- data5[,c(-2,-3,-4,-5,-6,-9,-15,-16,-17)]
  names(data5)[1] <- "target"
  
  return(data5)
}


featureset.nocorr.manual01 <- function(data){
  data.all <- preprocessing.create.allfeatures(data)
  
  # original no outlier 
  data2 <- data.all[,c(1:24)]
  data2 <- data2[,c(-14,-15,-22,-23)]
  data.new <- data2
  
  # select non correlated subset
  cor(data.new)
  par(mfrow=c(1,1))
  library(corrplot)
  corrplot(cor(data.new), method = "color", addCoef.col="grey")
  #corrplot(cor(data.new[,1:21]), method = "color", addCoef.col="grey")
  #corrplot(cor(data.new[,1:30]), method = "color", addCoef.col="grey")
  #corrplot(cor(data.new[,1:40]), method = "color", addCoef.col="grey")
  
  data5 <- data.new[,c(-16,-17,-13,-12,-11,-10,-4-3)]
  
  names(data5)[1] <- "target"

  return(data5)
}


featureset.nocorr.manual02 <- function(data){
  data.all <- preprocessing.create.allfeatures(data)
  
  # original no outlier
  data2 <- data.all[,c(1:24)]
  data2 <- data2[,c(-14,-15,-22,-23)]
  data.new <- data2
  
  # non correlated subset
  cor(data.new)
  par(mfrow=c(1,1))
  library(corrplot)
  corrplot(cor(data.new), method = "color", addCoef.col="grey")

  
  data5 <- data.new[,c(-2,-3,-10,-11,-16,-17)]
  names(data5)[1] <- "target"
  
  return(data5)
}


featureset.nocorr.log.manual03 <- function(data){
  data.all <- preprocessing.create.allfeatures(data)
  
  # logs
  data3 <- data.all[,c(1:24,35:45)]
  data3 <- data3[,c(1,7,8,13,16,17,20,21,24:35)]
  data.new <- data3
  
  # no correlated subset
  cor(data.new)
  par(mfrow=c(1,1))
  library(corrplot)
  corrplot(cor(data.new), method = "color", addCoef.col="grey")
  data5 <- data.new[,c(-10,-11,-16,-17,-18,-19,-20)]
  corrplot(cor(data5), method = "color", addCoef.col="grey")
  names(data5)[1] <- "target"
  
  return(data5)
}


featureset.nocorr.ratios.manual04 <- function(data){
  data.all <- preprocessing.create.allfeatures(data)
  
  # ratios
  data4 <- data.all[,c(1:34)]
  data4 <- data4[,c(-2,-3,-4,-5,-6,-15,-14,-22,-23)]
  data.new <- data4
  
  # no correlated subset
  cor(data.new)
  par(mfrow=c(1,1))
  library(corrplot)
  corrplot(cor(data.new), method = "color", addCoef.col="grey")
  data5 <- data.new[,c(-5,-6,-11,-20,-25,-23,-21)]
  corrplot(cor(data5), method = "color", addCoef.col="grey")
  names(data5)[1] <- "target"
  return(data5)
}


manual.load <- function(data){
  
  # 0. loading--------------------------------------------------
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  a = Sys.info()
  if (a[1] == "Linux"){
    # Linux reading file
    data <- read.csv(file="../Dataset/kc_house_data.csv", header=TRUE, sep=",")
    
  } else {
    # windows reading file
    data <- read.csv(file="../Dataset/kc_house_data.csv", header=TRUE, sep=",")
  }
  attach(data)
  
  
  # debugging
  
  load(file="../Dataset/featureset_allmanual.Rda")
  load(file="../Dataset/featureset_logratios.Rda")
  load(file="../Dataset/featureset_logs.Rda")
  load(file="../Dataset/featureset_nocorrelation01.Rda")
  load(file="../Dataset/featureset_nocorrelation02.Rda")
  load(file="../Dataset/featureset_nocorrelation03_logs.Rda")
  load(file="../Dataset/featureset_nocorrelation04_ratios.Rda")
  load(file="../Dataset/featureset_original_nooutliers.Rda")
  load(file="../Dataset/featureset_ratios.Rda")
  load(file="../Dataset/raw_continuous_d.Rda")
  
  }