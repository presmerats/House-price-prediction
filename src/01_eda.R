####################################################################
# Machine Learning Project
# Pau Rodriguez

# Data pre-processing and Exploratory Data analysis
# 07/05/2018
####################################################################

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

basic.eda <- function(data)
{
  # 1. inspection--------------------------------------------------
  dim(data)
  names(data)
  # target data$price
  
  summary(data)
  
  plot(data[1:1000,c(3:17)])
  
  # continuous vars
  price
  sqft_living
  sqft_lot
  sqft_basement
  sqft_living15
  sqft_lot15
  bathrooms
  lat
  long
  yr_built
  yr_renovated
  # continuous-integer:
  bedrooms
  floors
  # categorical-ordinal -> consider continuous
  condition
  table(condition)
  is.factor(condition)
  grade
  table(grade)
  is.factor(grade)
  
  # categorical vars
  # categorical-binary
  waterfront            
  summary(waterfront)
  table(waterfront==1)
  is.factor(waterfront)
  # we use it as numeric
  
  view
  summary(view)
  table(view==0)
  is.factor(view)
  # we use it as numeric
  
  # categorical: 
  # should be categorical? 
  #    -> order has no sense
  #    -> but linear regression cannot deal with it..
  zipcode
  table(zipcode)
  levels(zipcode)
  is.factor(zipcode)
  zipcode <- as.factor(zipcode)
  is.factor(zipcode)
  
  
  
  
  #date -> to numeric? does not maintain order
  date
  is.factor(date)
  date[c(1,100,200,300)]
  as.numeric(date)[c(1,100,200,300)]
  # we should skip this var for now, we are not doing time-series 
  # we could transform into quarters
  
  
  # set id as row.name
  #row.names(data) <- data$id # duplicated names!
  data$rownames <- data$id
  for (j in 1:nrow(data)){
    data$rownames[j] = paste(data$rownames[j],substr(as.character(data$date[j]), 1,6),sep="_") 
  }
  head(data$rownames)
  row.names(data) <- data$rownames
  head(data)
  
  
  
  # 2.missing data--------------------------------------------------
  
  summary(data)
  # NA's: not found
  # any Mean >> 3rd Qu.? or Mean << 1st Qu.?
  data$yr_renovated # a lot of houses are not renovated, but this is not missing data
  
  
  # 3. outliers--------------------------------------------------
  # Visual: summary, histogram
  summary(price)
  hist(data$price)
  hist(price[price < 4000000 ], breaks=15)
  hist(price[price > 5000000 ], breaks=15)
  hist(price[price > 3000000 ], breaks=15)
  # not sure they are outliers. 
  # long tail for sure
  
  hist(data$bathrooms)
  table(bathrooms)
  hist(bathrooms[])
  # long tail again, but maybe this time it is easier to remove 8
  
  
  hist(data$bedrooms)
  table(bedrooms)
  # 33 is a clear outlier
  hist(data$bedrooms[bedrooms!=33])
  
  
  hist(data$sqft_living)
  hist(sqft_living[sqft_living<10000])
  hist(sqft_living[sqft_living<5000])
  summary(sqft_living)
  # long tail again
  
  hist(data$sqft_lot)
  hist(data$sqft_lot[sqft_lot<100000])
  summary(sqft_lot)
  # long tail...
  
  hist(data$sqft_above)
  hist(data$sqft_basement)  # long tail or outlier
  hist(data$sqft_living15)
  hist(data$sqft_lot15)     # long tail or outlier
  
  
  # one individual with bedrooms=33 -> clear outlier
  data[data$price==640000,1:4] # clear outlier!
  data <- data[data$bedrooms!=33,]
  #data.new <- data.new[data.new$bedrooms!=33,]
  
  # mahalanobis distance 
  load_install_packages("chemometrics")
  names(data)
  # columns to consider: continuous(price, bedrooms, bathrooms, sqft_*, floors,condition, grade), binary(waterfront, view)
  names(data)
  dim(data[,c(3:8,11:14)])
  data.outliers = Moutlier(data[,c(3:8,11:14)],quantile = 0.975, plot = TRUE) # singular
  data.outliers = Moutlier(data[,c(3:12,18,19)],quantile = 0.975, plot = TRUE)
  data.outliers = Moutlier(data[,c(3:12)],quantile = 0.975, plot = TRUE)
  data.outliers = Moutlier(data[,c(3:9)],quantile = 0.975, plot = TRUE)
  data.outliers = Moutlier(data[,c(3:7,11)],quantile = 0.975, plot = TRUE)
  data.outliers = Moutlier(data[,c(3:8,20,21)],quantile = 0.975, plot = TRUE)
  # maybe sqft_above and sqft_living? or any combination of the sqft
  # cols that make it singular:
  #    9,10,11,13,14,16,
  # non sense to use: id, date, zipcode, 
  cov(data[,c(3:14,15,16,18:21)])
  cr <- cor(data[,c(3:14,15,16,18:21)])
  as.numeric(cr > 0.69)
  
  
  data.outliers = Moutlier(data[,c(3:8,12,15,18:21)],quantile = 0.975, plot = TRUE)
  data.outliers$cutoff
  head(data.outliers$rd)
  data.outliers.sorted <- sort(data.outliers$rd, decreasing=TRUE)
  head(data.outliers.sorted)
  length(data.outliers.sorted[data.outliers.sorted>data.outliers$cutoff]) #5585! by the cutoff and 0.975 quantile
  length(data.outliers.sorted[data.outliers.sorted>data.outliers$cutoff]) #4682! by the cutoff and 0.997 quantile
  # we can remove the first 8 values in the data.outliers.rd
  data.outliers.sorted[1:8]
  
  #cleaning outliers
  row.names(data)
  colnames(data)
  data.orig <- data
  data <- data.orig
  data <- data[data.outliers$rd<566,2:21]
  detach(data)
  attach(data)
  
  
  
  # 4. features--------------------------------------------------
  
  # ranges
  
  
  date
  as.numeric(date)
  
  # transform to months
  datelevels <- levels(date)
  for(i in 1:length(datelevels)){
    datelevels[i] <- substr(strsplit(as.character(datelevels[i]),"T"), 8,9)
  }
  datelevels
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
  datelevels
  buyquarter <- data$date
  levels(buyquarter) <- datelevels
  buyquarter <- as.numeric(buyquarter)
  
  
  
  
  # bathrooms manually generated ranges: <1, <2,<3,<4,>=4
  table(bathrooms)
  hist(bathrooms)
  bathrooms.cat <- cut(bathrooms, breaks = c(seq(-0.254,0.25),8) )  # makes no sense
  bathrooms.cat <- cut(bathrooms, breaks = seq(-0.25,8,0.25) )  # better, but it's the same as the bathrooms var
  head(bathrooms.cat)
  table(bathrooms.cat)
  head(bathrooms)
  length(bathrooms.cat[is.na(bathrooms.cat)])
  hist(as.numeric(bathrooms.cat))
  bathrooms.range <- as.numeric(bathrooms.cat)
  
  
  # bedrooms manually generated ranges: 
  table(bedrooms)
  hist(bedrooms)
  bedrooms.cat <- cut(bedrooms, breaks = c(-1,0:10,33) ) 
  head(bedrooms.cat)
  head(bedrooms)
  table(bedrooms.cat)
  length(bedrooms.cat[is.na(bedrooms.cat)])
  hist(as.numeric(bedrooms.cat))
  bedrooms.range <- as.numeric(bedrooms.cat)
  
  # sqft_living
  hist(sqft_living)
  hist(sqft_living, breaks=100)
  max(sqft_living)
  sqft_living.cat <- cut(sqft_living, breaks = c(seq(0,7000,10),13540 ) )
  sqft_living.range <- as.numeric(sqft_living.cat)
  hist(sqft_living.range)
  
  # sqft_lot
  hist(sqft_lot)
  hist(sqft_lot, breaks=100)
  summary(sqft_lot)
  sqft_lot.cat <- cut(sqft_lot, breaks = c(seq(519,20000,50),1651359 ) )
  hist(as.numeric(sqft_lot.cat)) # really long tail!
  sqft_lot.cat <- cut(sqft_lot, breaks = c(seq(519,100000,50),1651359 ) )
  hist(as.numeric(sqft_lot.cat)) # really long tail!
  sqft_lot.cat <- cut(sqft_lot, breaks = c(seq(519,1000000,50),1651359 ) )
  hist(as.numeric(sqft_lot.cat)) 
  length(sqft_lot[sqft_lot>50000])
  length(sqft_lot[sqft_lot>100000])
  length(sqft_lot[sqft_lot>1000000])
  length(sqft_lot[sqft_lot>1500000])
  
  
  
  
  # binary vars
  
  is.renovated <- as.numeric(yr_renovated > 0 )
  is.renovated
  
  # ratios
  
  bathrooms.bedrooms.ratio <- bathrooms/bedrooms
  # infinite values appear
  table(bathrooms)
  table(bedrooms)
  # change inf to 0
  bathrooms.bedrooms.ratio[bedrooms==0] <- 0
  summary(bathrooms.bedrooms.ratio)
  
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
  
  names(data.new)
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
  
  
  for(i in 1:ncol(data.new)){
    print(length(data.new[,i]))
  }
  
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
  
  
  # plots
  par(mfrow=c(1,1))
  
  boxplot(price)
  names(data.new)
  boxplot(data.new[,1:7])
  boxplot(data.new[,8:14])
  boxplot(data.new[,15:21])
  boxplot(data.new[,22:28])
  # sqft_* 
  boxplot(data.new[,c(4,5,11,12,18,19)]) # sqft_lot has the most variability
  # sqft-living, sqft_above, sqft_basement
  boxplot(data.new[,c(4,11,12,18)]) 
  # bathrooms, bedrooms
  boxplot(data.new[,c(2,3,6,7,8,9)])
  # ratios
  boxplot(data.new[,c(25,26)])  # bathrooms bedrooms hihg variance
  boxplot(data.new[,c(25,26,27)])
  boxplot(data.new[,c(26,27)])
  boxplot(data.new[,c(25,26,27,28)]) # sqft.ratio biggest variance
  
  # pairs
  length(price)
  dim(data.new)
  plot(sqft.ratio,price)
  plot(sqft_living,price)
  plot(sqft_lot,price)
  plot(bedrooms.sqft.living.ratio,price)
  plot(bathroom.sqft.living.ratio,price)
  pairs(price ~ bedrooms.sqft.living.ratio + bathroom.sqft.living.ratio)
  
  pairs(price ~ bedrooms.sqft.living.ratio + bathroom.sqft.living.ratio + sqft.ratio + sqft_living+sqft_lot) # nice one
  
  pairs(price ~ floor.sqft.living.ratio  + floor.sqft.lot.ratio + floor.bedrooms.ratio +  floor.bathrooms.ratio + sqft.living.floors.ratio + bedrooms.floors.ratio) # nice one
  
  
  plot(bedrooms.range,price) # better than the normal bedrooms
  plot(bedrooms,price) #
  plot(bathrooms.range,price) # worse than the normal bathrooms
  plot(bathrooms,price)
  pairs(price ~ bathrooms + bedrooms + bedrooms.range + bathrooms.range + floors) 
  pairs(price ~ bathrooms + bedrooms.range + floors) # nice one
  
  # log plots
  par(mfrow=c(1,2))
  plot(bedrooms,price)
  plot(log.bedrooms,price)
  
  
  plot(bathrooms,price)
  plot(log.bathrooms,price)
  
  plot(sqft_living ,price)
  plot(log.sqft_living,price)
  
  plot( sqft_lot,price)
  plot(log.sqft_lot,price)
  
  plot( floors,price)
  plot(log.floors,price)
  
  plot( condition,price)
  plot(log.condition,price)
  
  plot( grade,price)
  plot(log.grade,price)
  
  plot(sqft_above ,price)
  plot(log.sqft_above,price)
  
  plot(sqft_basement ,price)
  plot(log.sqft_basement,price)
  
  plot(lat ,price)
  #plot(log.lat,price) # makes no sense either
  
  plot( long,price)
  #plot(log.long,price) # impossible
  
  plot(sqft_living15 ,price)
  plot(log.sqft_living15,price)
  
  plot(sqft_lot15 ,price)
  plot(log.sqft_lot15,price)
  
  plot(bathrooms.range ,price)
  plot(log.bathrooms.range,price)
  
  plot( bedrooms.range,price)
  plot(log.bedrooms.range,price)
  
  plot(bathrooms.bedrooms.ratio ,price)
  plot(log.bathrooms.bedrooms.ratio,price)
  
  plot(bedrooms.sqft.living.ratio ,price)
  plot(log.bedrooms.sqft.living.ratio,price)
  
  plot( bathroom.sqft.living.ratio,price)
  plot(log.bathroom.sqft.living.ratio,price)
  
  plot( sqft.ratio,price)
  plot(log.sqft.ratio,price)
  
  plot( floor.sqft.living.ratio,price)
  plot(log.floor.sqft.living.ratio,price)
  
  plot(floor.sqft.lot.ratio ,price)
  plot(log.floor.sqft.lot.ratio,price)
  
  plot(floor.bedrooms.ratio ,price)
  plot(log.floor.bedrooms.ratio,price)
  
  plot( floor.bathrooms.ratio,price)
  plot(log.floor.bathrooms.ratio,price)
  
  plot( sqft.living.floors.ratio,price)
  plot(log.sqft.living.floors.ratio,price)
  
  plot( bedrooms.floors.ratio,price)
  plot(log.bedrooms.floors.ratio,price)
  
  plot(data.new$zipcode,price) # interesting information , but need to reorder maybe in terms of price? regression cannot work with that!
  
  plot(buymonth,price)
  plot(buyquarter,price)
  plot(is.renovated,price)
  
  
  table(price,buyquarter)
  table(price,buymonth)  
  
  
  # final plots
  ncol(data.new)
  names(data.new)
  par(mfrow=c(4,4))
  for (k in 2:17){
    thelable=paste(colnames(data.new)[k])
    plot(data.new[,k],price,xlab=thelable)
  }
  # save as image?
  
  par(mfrow=c(4,4))
  for (k in 18:32){
    thelable=paste(colnames(data.new)[k])
    plot(data.new[,k],price,xlab=thelable)
  }
  
  par(mfrow=c(4,4))
  for (k in 33:47){
    thelable=paste(colnames(data.new)[k])
    plot(data.new[,k],price,xlab=thelable)
  }
  
  par(mfrow=c(4,4))
  for (k in 48:59){
    thelable=paste(colnames(data.new)[k])
    plot(data.new[,k],price,xlab=thelable)
  }
  
  
  # 5. statistical analysis--------------------------------------------------
  
  # Gaussianity
  
  #     histograms with normal overl
  
  hist.with.normal <- function (x, main, xlabel=deparse(substitute(x)), ...)
  {
    h <- hist(x,plot=F, ...)
    s <- sd(x)
    m <- mean(x)
    ylim <- range(0,h$density,dnorm(0,sd=s))
    hist(x,freq=FALSE,ylim=ylim,xlab=xlabel, main=main, ...)
    curve(dnorm(x,m,s),add=T,col="red")
  }
  
  
  par(mfrow=c(3,2))
  for (k in c(2:24,26:30,33:34,37:42,44:48) ){
    thelabel=paste(colnames(data.new)[k])
    print(paste(k,thelabel,sep=" "))
    hist.with.normal (data.new[,k], thelabel )
  }
  # selected for Gaussianization
  log.sqft_lot15
  log.grade
  log.sqft_lot
  bedrooms.floors.ratio
  bedrooms.ssqft.living.ratio
  badrooms.sqft.living.ratio
  floot.sqft.living.ratio
  sqft.living.floors.ratio
  sqft_living15
  grade
  sqft_lot
  bathrooms
  
  
  
  #     Gaussianization
  load_install_packages("MASS")
  
  gaussianize <- function(x,y,df) {
  
    
    par(mfrow=c(1,3))
    hist(x, main="Look at that ...")
    
    bx <- boxcox(I(x+1) ~ . - y, data = df,
                 lambda = seq(-0.25, 0.25, length = 10))
    
    lambda <- bx$x[which.max(bx$y)]
    
    x.BC <- (x^lambda - 1)/lambda
    
    hist(x.BC, main="Look at that now!")
    
    par (mfrow=c(1,1))
    return(x.BC)
  }
  
  data.new2 <- data.new[,c(3:8,12,15,18:21)]
  gaussianize(bathrooms,price,data.new2)
  log.sqft_lot15.BC <- gaussianize(log.sqft_lot15,price,data.new2)  # maybe
  gaussianize(log.grade,price,data.new2)
  log.sqft_lot.BC <- gaussianize(log.sqft_lot,price,data.new2)   # maybe
  gaussianize(bedrooms.floors.ratio,price,data.new2)
  gaussianize(bedrooms.sqft.living.ratio,price,data.new2)
  bathroom.sqft.living.ratio.BC <- gaussianize(bathroom.sqft.living.ratio,price,data.new2) # maybe
  gaussianize(floor.sqft.living.ratio,price,data.new2)
  sqft.living.floors.ratio.BC <- gaussianize(sqft.living.floors.ratio,price,data.new2)  # maybe
  sqft_living15.BC <- gaussianize(sqft_living15,price,data.new2)  # maybe
  gaussianize(grade,price,data.new2)
  sqft_lot.BC <- gaussianize(sqft_lot,price,data.new2)  # better, but not gaussian
  gaussianize(bathrooms,price,data.new2)
  
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
  
  
  # PCA
  #library(FactoMineR)
  #pca <- PCA(data.new)
  

  
  
}
