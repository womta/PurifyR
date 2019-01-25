#####################
# General Functions #
#####################

# Strip special characters + spaces out of a vector
stripSpecChar = function (x) {
  x = gsub("[^[:alnum:] ]", "", x)
  x = gsub(" ", "", x)
  return(x)
}

# check data, warn user assumptions
CheckData <- function(x) {
  N=nrow(x)
  M=ncol(x)
  
  Warning = 'Warning '
  messages = 0
  
  if (M>N) {
    cat(Warning, 'More Columns Than Records\n')
    messages = messages+1
  }
  
  if (N<30) {
    cat('Very Small Data Set\n')  
    messages = messages+1
  }
  
  if (messages==0) {
    cat('No Warnings\n')  
  }
  
}

# Calculate Mode
mode = function(x) {
  ux = unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

# Standard Error
SE = function(x) {
  x = x[!is.na(x)]
  COUNT = length(x)
  SD = sd(x)
  SE = SD/sqrt(COUNT)
}

# Coefficient Of Variation
cv = function(x){
  sd = sd(x, na.rm=TRUE)
  mean = mean(x, na.rm=TRUE)
  cv = 100*(sd/mean)
  return(cv)
}

# remove NAs, NaN, Inf, -Inf
removeNANaNInf = function (x) {
  #x=x[!is.na(x)]
  #x=x[!is.nan(x)]
  #x=x[!is.infinite(x)]
  x=x[is.finite(x)]
  x = as.numeric(x)
  return(x)
}

# Mean Absolute Deviation
meanAD=function (x) {
  mean=mean(x, na.rm=TRUE)
  y=mean(abs((x-mean)), na.rm=TRUE)
  return(y)
}

# Calculate Missing Data Percentage Per Record
missingDataPerRecord = function(DT) {
  nCOL = ncol(DT)
  percentageMissing = unlist(DT[,100*(Reduce(`+`, lapply(.SD,is.na))/nCOL)])
  return(percentageMissing)
}

# Get the numeric column names
detectNumericClass = function(x) {
  CLASS = class(x)
  if(CLASS=='numeric' | CLASS=='integer' | CLASS=='numeric') {
    return(CLASS)
  }
}

# SCALING
featureScaling = function(y, scalingMethod) {
  
  LENGTH = length(y)
  SD = sd(y, na.rm=TRUE)
  MEAN = mean(y, na.rm=TRUE)
  MISSING = sum(is.na(y))
  
  if (is.na(SD)) {
    SD = 0
  }
  
  if (is.na(MEAN)) {
    MEAN = 0
  }
  
  # Fix for SD of zero in one or more plates
  if (LENGTH==0 | SD==0 | MEAN==0 | MISSING==LENGTH) {
    cat("Skipping variable \n")
  } else {
    # Feature scaling
    if (is.na(scalingMethod) | scalingMethod=="none") {
      # nothing
    } else if (scalingMethod=="zscore") {
      y = plusOne(y)
      y=scale(y)
    } else if (scalingMethod=="robustZscore") {
      y = plusOne(y)
      y=robustZscore(y)
    } else if (scalingMethod=="minMax") {
      y = plusOne(y)
      y=range01(y)
    }
  }
  return(y)
}


#################
# Preprocessing #
#################

# Probability Uniformal
ProbUniform = function(x) {
  x = removeNANaNInf(x)
  
  meanV = mean(x)
  sdV = sd(x)
  minV = min(x)
  maxV = max(x)
  
  pnormV = try(dnorm(x, meanV, sdV))
  pnormV<-try(sum(log(pnormV)))
  
  # probability uniform
  punifV = try(dunif(x, minV, maxV))
  punifV = try(sum(log(punifV)))
  
  uniFormDistributionV = try(punifV-pnormV)
  return(uniFormDistributionV)
}


##################
# Transformation #
##################

applyTransformation = function(data, skewness) {
  if(!is.na(skewness)) {
    if (skewness>0) {
      data = plusOne(data)  
      data = log(data)      
    } else {
      data = sqrt(data)
    }
  }
  return(data)
}  

###########
# Scaling #
###########

range01 = function(x){
  min = min(x, na.rm=TRUE)
  max = max(x, na.rm=TRUE)
  rs = (x-min)/(max-min)
  return(rs)
}

plusOne = function(x) { 
  x <- 1 + x - min(x,na.rm=TRUE)
  return(x)
}

# robust z-score
robustZscore=function(x) {
  meanAD <<- meanAD
  
  med <- median(x,na.rm=TRUE)
  nas<-sum(!is.na(x))
  if (nas !=0){
    mad = mad(x, na.rm=TRUE)
  }
  else {
    mad=0
  }
  
  meanADscore <- meanAD(x)
  
  if (mad==0) {
    y <- (x-med)/(1.253314*meanADscore)
  } else {
    y <- (x-med)/(1.4826*mad)
  }
  return(y)
}


######################
# Imputation Methods #
######################

# Impute median per column
impute.median=function(x) replace(x, is.na(x), median(x, na.rm = TRUE))	# median

# impute mean per column
impute.mean=function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))	# mean

# impute max per column
impute.mode=function(x) replace(x, is.na(x), mode(x))	# mode

# impute max per column
impute.max=function(x) replace(x, is.na(x), max(x, na.rm = TRUE))	# max

# calculate Estimators per well
calculateEstimator = function(x) {
  NAs = sum(is.na(x))
  COUNTRAW = length(x)
  x = x[!is.na(x)]
  COUNT = length(x)
  if (COUNT>1) {
    SUMMARY = summary(x)
    MIN = SUMMARY[1]
    Q1 = SUMMARY[2]
    MEDIAN = SUMMARY[3]
    MEAN = SUMMARY[4]
    Q3 = SUMMARY[5]
    IQR = IQR(x, na.rm=T)
    MAX = SUMMARY[6]
    SD = sd(x)
    MAD = mad(x)
    CV = as.numeric(try(100*(SD/MEAN)))
    SE = SD/sqrt(COUNT)
    UNIQUE = unique(x)
    PercentUnique = 100*(length(UNIQUE)/COUNTRAW)  
  } else {
    MIN = NA
    Q1 = NA
    MEDIAN = NA
    MEAN = NA
    Q3 = NA
    IQR = NA
    MAX = NA
    SD = NA
    MAD = NA
    CV = NA
    SE = NA
    UNIQUE = 1
    PercentUnique = 0
  }
  return(c(MIN, Q1, MEDIAN, MEAN, IQR, Q3, MAX, SD, MAD, CV, SE, NAs, COUNT, PercentUnique))
}

# Eliminate outliers
eliminateOutliers = function(x, SDs) {
  SD = sd(x)
  MEAN = mean(x)
  threshold1 = MEAN + SDs*SD
  threshold2 = MEAN - SDs*SD
  
  x[x>threshold1 | x<threshold2]=NA
  return(x)
}

# initializing multithreading
# initialize cpus for multithreading
initializeCPUS = function(condition) {
  maxNumberOfCores <<- parallel::detectCores()
  
  if(is.na(maxNumberOfCores)) {
    maxNumberOfCores = 1
  }
  
  parallel=TRUE
  cpus=condition
  
  if (cpus >= maxNumberOfCores) {
    cpus = maxNumberOfCores
  }
  
  if (cpus<2) {
    parallel = FALSE
  }
  
  assign("parallel", parallel, envir = .GlobalEnv)
  assign("cpus", cpus, envir = .GlobalEnv)
}


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Detect Garbage function
detectGarbage=function(i) {
  numberOfDistinctValuesCurrentParameter=length(unique(i))
  NumberOfRecords = length(i)
  if (numberOfDistinctValuesCurrentParameter<=numberOfValuesDiscreteVariable) {
    previewUniqueValues=numberOfDistinctValuesCurrentParameter
  } else {
    previewUniqueValues=numberOfValuesDiscreteVariable
  }
  
  currentClass=class(i)[1]
  
  if (currentClass!="numeric" & currentClass!="integer" & currentClass!="logical") {
    dataCategory=currentClass
    
    # statistics
    stdevCurrentParameter=NA
    minCurrentParameter=NA
    q1CurrentParameter=NA
    medianCurrentParameter=NA
    meanCurrentParameter=NA
    q3CurrentParameter=NA
    maxCurrentParameter=NA
    iqrCurrentParameter=NA
    cvCurrentParameter=NA
    COUNT = length(!is.na(i))
    SE = NA
    MAD = NA
    NAs = sum(is.na(i))
    skewness = NA
    
    missingCurrentParameter=sum(is.na(i))
    percentageMissing=100*(missingCurrentParameter/NumberOfRecords)
    percentageMissing=round(percentageMissing, 2)
    status=as.character("noData")
    distinctValuesCurrentParameter=paste(as.character(unique(i))[1:previewUniqueValues], collapse=', ', sep='')
  } else {
    i = as.double(i)
    distinctValuesCurrentParameter=paste(as.character(round(unique(i), 3)[1:previewUniqueValues]), collapse=', ', sep='')
    
    # statistics
    missingCurrentParameter=sum(is.na(i))
    SUMMARY = summary(i, na.rm=T)
    stdevCurrentParameter=sd(i, na.rm=T)
    
    minCurrentParameter=SUMMARY[1]
    q1CurrentParameter=SUMMARY[2]
    medianCurrentParameter=SUMMARY[3]
    meanCurrentParameter=SUMMARY[4]
    q3CurrentParameter=SUMMARY[5]
    maxCurrentParameter=SUMMARY[6]
    iqrCurrentParameter=IQR(i,na.rm=T)
    COUNT = length(!is.na(i))
    MAD = mad(i,na.rm=T)
    NAs = sum(is.na(i))
    skewness = e1071::skewness(i)
    
    if (is.na(stdevCurrentParameter)) {
      stdevCurrentParameter=0
      cvCurrentParameter=0
      SE = 0
      
    } else {
      cvCurrentParameter=100*stdevCurrentParameter/meanCurrentParameter
      SE = stdevCurrentParameter/sqrt(COUNT)
    }	
    
    if (is.na(numberOfDistinctValuesCurrentParameter)) {
      numberOfDistinctValuesCurrentParameter=0
    }
    
    percentageMissing=100*(missingCurrentParameter/NumberOfRecords)
    percentageMissing=round(percentageMissing, 2)
    
    if (numberOfDistinctValuesCurrentParameter<3) {
      dataCategory="Binary"
    } else if (numberOfDistinctValuesCurrentParameter<5) {
      dataCategory="Categorial"
    } else {
      dataCategory="Continuous"
    }
    
    if(percentageMissing==100) {
      status<-"Empty"
    } else if (stdevCurrentParameter==0) {
      status="No variation"
    } else if (percentageMissing>=95) {
      status="Missing"
    } else if (dataCategory=="Binary" || dataCategory=="Categorial") {
      status="Discriminant"
    }	else {
      status="Healthy"
    }
  }
  
  
  result= c(dataCategory, currentClass, numberOfDistinctValuesCurrentParameter, distinctValuesCurrentParameter, stdevCurrentParameter, percentageMissing, NAs, skewness, cvCurrentParameter, minCurrentParameter, q1CurrentParameter, meanCurrentParameter, medianCurrentParameter, iqrCurrentParameter, q3CurrentParameter, maxCurrentParameter, SE, MAD, status)
  return(result)
}

#' LoadR'
#' 
#' This function 'loads' the data in the right format (data.table)
#' @param DT This is a data.table object
#' @return a data.table with all the data
#' @export
#' @examples
#' DT = data.table::data.table(plyr::baseball)
#' DT = LoadR(DT)

#########
# LoadR #
#########
LoadR = function(DT) {
  if(class(DT)[1]=='character') {
    cat('assuming a link to a flat file')
    DT = data.table::fread(DT, data.table=TRUE, check.names=TRUE, integer64="numeric")
  }
  
  if(class(DT)[1]!='data.table') {
    data.table::setDT(DT)
  } 
  
  return(DT)
}


#########
# ScanR #
#########

ScanR = function(DT, correlationCutoff, SamplingPercentage, percentUniqueCutoff) {
  
  # Non-mandatory arguments
  if(missing(correlationCutoff)) {
    print("Setting Correlation Cutoff to 0.99 (Default)")
    correlationCutoff = .99
  } 
  
  if(missing(SamplingPercentage)) {
    print("Setting Sampling to 15% (Default)")
    
    N = nrow(DT)
    checkBoundary = ceiling(N * 0.15)
    
    if (checkBoundary<minSamplingSize) {
      SamplingPercentage = 100
    } else if (checkBoundary>maxSamplingSize) {
      SamplingPercentage = N/100
      SamplingPercentage = round(maxSamplingSize/SamplingPercentage)
    } else {
      SamplingPercentage = 15      
    }
  } 
  
  # Non-mandatory arguments
  if(missing(percentUniqueCutoff)) {
    print("Setting Percent Unique Cutoff to 20 percent (Default)")
    percentUniqueCutoff = 5
  } 
  
  # Log 
  logDT = c()
  logDT = c(logDT, 'All Messages Down Here')
  
  # Clean up variables names
  newNames = sapply(names(DT), function(x) stripSpecChar(x))
  lengthNames = length(newNames)
  lengthUniqueNames = length(unique(newNames))
  if (lengthNames!=lengthUniqueNames) {
    newNames = sapply(1:length(newNames), function(x) paste0(newNames[x],'_',x))
  }
  
  if (length(names(DT)) == length(unique(names(DT)))) {
    data.table::setnames(DT, names(DT), newNames)
  } else {
    names(DT) = newNames
  }
  
  # Clean up predictor names
  predictors = newNames
  
  # sampling
  N = nrow(DT)
  percentage = SamplingPercentage # Argument in Main (in percentages)
  percentage = percentage/100
  percentage = ceiling(percentage*N)
  sample = sample.int(N, percentage)
  
  # train & test set & sample
  DT = DT[sample]
  
  ##################
  # Preparing Data #
  ##################
  
  #nzv = caret::nearZeroVar(DT, names = T, allowParallel = T, saveMetrics = F)
  
  currentOS = as.character(get_os())
  
  if(currentOS=='linux') {
    initializeCPUS(ncol(DT))
    doMC::registerDoMC(cores=cpus)
    # filter of predictors# preparing/preprocessing data
    VariableTable=data.table::data.table(t(DT[, parallel::mclapply(.SD, detectGarbage, mc.cores=cpus), .SDcols = predictors]))
    #VariableTable=data.table::data.table(t(DT[, lapply(.SD, detectGarbage), .SDcols = predictors]))
    
  } else {
    VariableTable=data.table::data.table(t(DT[, lapply(.SD, detectGarbage), .SDcols = predictors]))
  }
  
  # set column names
  data.table::setnames(VariableTable, names(VariableTable), c('DataCategory', 'DataType', 'NumberUniqueValues', 'UniqueValues', 'Stdev', 'PercentMissing','NAs', 'Skewness', 'CV', 'MIN', 'Q1', 'MEAN', 'MEDIAN', 'IQR', 'Q3', 'MAX', 'SE', 'MAD', 'Status'))
  
  # setting data type for output
  class(VariableTable$Stdev) = 'numeric'
  class(VariableTable$PercentMissing) = 'numeric'
  class(VariableTable$NumberUniqueValues) = 'numeric'
  class(VariableTable$Skewness) = 'numeric'
  VariableTable$Predictors = predictors
  
  # Filter predictors (ignoring the garbage variables)
  predictors = VariableTable[Status=='Healthy',Predictors]
  #predictors = filter(VariableTable, Status=='Healthy')$Predictors
  
  # Warning user about assumptions/checks
  CheckData(DT[,predictors, with=FALSE])
  
  # Initialize Max Correlation
  VariableTable$Correlation=0
  
  # Multivariate Data Set, check/prepare for singularity 
  if (length(predictors)>2) {
    
    # correlation matrix data
    CorData = DT[,predictors, with=FALSE]
    
    # Preparing correlation matrix data
    if (nrow(na.omit(DT[,predictors, with=FALSE]))<500) {
      logDT = c(logDT, 'Too many missing data points')
      Sample = sample.int(nrow(DT[,predictors, with=FALSE]), 500, replace = TRUE)
      logDT = c(logDT, 'sample for imputation')
      CorData = DT[Sample,predictors, with=FALSE]
      cat('sample created\n')
      
      mice.imp = try(mice::mice(CorData[,predictors, with=F],m=1,maxit=1,meth='sample'))
      if (class(mice.imp)!='try-error') { # mice results a list
        try(CorData[, (predictors) := mice::complete(mice.imp,action = 1, include = FALSE)]) #complete(plate.imp,action = 1, include = FALSE))
      }
      CorData = data.table::data.table(apply(CorData[,predictors, with=FALSE], 2, function(x) return(impute.median(x))))
      cat('imputation done\n')
    }
    
    # Create correlation matrix
    #CorData = apply(CorData[,predictors, with=FALSE], 2, function(x) return(impute.median(x)))
    #CorData = data.table(apply(CorData[,predictors, with=FALSE], 2, function(x) return(impute.median(x))))
    CorData = cor(CorData[,predictors, with=FALSE], use='complete.obs', method='pearson')
    diag(CorData) = NA
    row.names(CorData) = colnames(CorData)
    CorData = data.frame(CorData)
    VariableTable[VariableTable$Predictors %in% predictors,]$Correlation = c(apply(CorData[,predictors], 1, function(x) max(x[is.finite(x)], na.rm=TRUE))) # seting maximum correlation per variable
    
    # initialize 
    #correlationCutoff = .99
    mahalanobis = NA
    class(mahalanobis) = "try-error"
    
    # Detecting Variables with a correlation cut-off of .99 or higher for non-singular matrix assumption 
    #if (length(highCovariationVariables)>1) {
    while (class(mahalanobis)[1]=="try-error") {
      correlationCutoff = correlationCutoff-0.01
      print(correlationCutoff)
      #highCovariationVariables = data.table::data.table(VariableTable %>% filter(abs(Correlation)>=correlationCutoff))$Predictors # Variables that covary > cut-off
      #highCovariationVariables = VariableTable %>% filter(abs(Correlation)>=correlationCutoff))$Predictors # Variables that covary > cut-off
      highCovariationVariables = VariableTable[abs(Correlation)>=correlationCutoff,Predictors] # Variables that covary > cut-off
      highCovariationVariablesTemp = highCovariationVariables
      
      if (length(highCovariationVariables)>1) {
        for (i in 1:length(highCovariationVariables)) {
          currentHighCovariationVariable = highCovariationVariables[i] # select a variable from the high covariates
          if (currentHighCovariationVariable%in%highCovariationVariablesTemp) {
            currentHighCovariates = row.names(CorData[abs(CorData[,currentHighCovariationVariable])>=correlationCutoff,]) # what variables do covary high with the selected variable
            currentHighCovariates = c(currentHighCovariationVariable, currentHighCovariates) # make vector group of variables that covary together
            #lowestMissingDataValue = min(data.table::data.table(filter(VariableTable, Predictors %in% currentHighCovariates))$PercentMissing, na.rm=TRUE) # the variable from that group with the lowest missing percentage data
            lowestMissingDataValue = min(VariableTable[Predictors %in% currentHighCovariates,PercentMissing], na.rm=TRUE) # the variable from that group with the lowest missing percentage data
            
            
            #lowestMissingDataVariable = data.table(filter(VariableTable, Predictors %in% currentHighCovariates & PercentMissing==lowestMissingDataValue))$Predictors[1] # select the first from that vector if there are more than 1
            lowestMissingDataVariable = VariableTable[Predictors %in% currentHighCovariates & PercentMissing==lowestMissingDataValue,Predictors][1] # select the first from that vector if there are more than 1
            
            
            
            currentHighCovariates = currentHighCovariates[currentHighCovariates!=lowestMissingDataVariable] # select the variables that need to be ignored
            VariableTable[VariableTable$Predictors %in% currentHighCovariates]$Status = 'Covariance' # set to code Covariance where they will be ignored
            highCovariationVariablesTemp = highCovariationVariablesTemp[!highCovariationVariablesTemp%in%currentHighCovariates]
            cat('current covariates ', length(currentHighCovariates), '\n')
            cat('high covariates ', length(highCovariationVariables), '\n')		
          }
        }
      }
      
      
      # Filter Predictors again after singular matrix analysis (correlation cut-off of .99)
      #predictors = filter(VariableTable, Status=='Healthy')$Predictors
      predictors = VariableTable[Status=='Healthy',Predictors]
      
      
      # calculating the mahalanobis distance (sensitive for a singular matrix), trying until succeeds
      colMeans = colMeans(DT[,predictors, with=F])
      Sx = try(cov(DT[,predictors, with=F]))
      mahalanobis = try(mahalanobis(DT[,predictors, with=F], colMeans, Sx), silent = TRUE)
      
    }
  }
  
  # ignoring variables with no correlation coefficient
  VariableTable[VariableTable$Correlation==0]$Correlation = NA
  
  # Filter Predictors again after singular matrix analysis (correlation cut-off of .99)
  predictors = VariableTable[Status=='Healthy',Predictors]
  #predictors = filter(VariableTable, Status=='Healthy')$Predictors
  
  if (length(predictors)>2) {
    CorData = CorData[predictors,predictors]
  }
  
  VariableTable$PercentUnique = round(100*(VariableTable$NumberUniqueValues / nrow(DT)), 3)
  
  VariableTable[,'NormalD':=0]
  VariableTable[Status=='Healthy','NormalD':=as.numeric(unlist(c(DT[, lapply(.SD, function(x) MASS::fitdistr(removeNANaNInf(x), densfun="normal")[1]$estimate[1]), .SDcols = predictors])))]
  VariableTable[NormalD==0,'NormalD':=NA]
  
  VariableTable[,'UniformD':=0]
  VariableTable[Status=='Healthy','UniformD':= unlist(DT[, lapply(.SD, function(x) ProbUniform(x)), .SDcols = predictors])]
  VariableTable[UniformD==0,'UniformD':=NA]
  VariableTable[UniformD>0]$Status = 'Uniform'
  #VariableTable[PercentUnique<percentUniqueCutoff]$Status = 'LowUniqueness'
  
  
  # Most Correlated To
  diag(CorData)=0
  data.table::setDT(CorData)
  VariableTable[,'MostCorrelatedTo':='']
  VariableTable[Predictors%in%predictors,'MostCorrelatedTo':= CorData[,names(.SD)[max.col(.SD, ties.method="first")]]]
  VariableTable[MostCorrelatedTo=='','MostCorrelatedTo':=NA]
  
  VariableTable = VariableTable[,c('Predictors', 'Status', 'DataCategory', 'DataType', 'PercentUnique', 'MostCorrelatedTo', 'NumberUniqueValues', 'UniqueValues', 'NormalD', 'UniformD', 'Correlation', 'PercentMissing', 'NAs', 'Stdev', 'Skewness', 'CV', 'MIN','Q1', 'MEAN', 'MEDIAN', 'IQR', 'Q3', 'MAX', 'SE', 'MAD')]
  return(VariableTable)
}

#' ScrubR'
#' 
#' This function 'scrubs' the data, it massages the data in checking and fixing missing data,
#' transformations, scaling and outlier removal or imputation
#' @param DT This is a data.table object
#' @param predictors These are the clean predictors provided manually or from the output of ScanR
#' @param rowWiseMissingPercentage This the the allowed threshold of the percentage missing data 
#' per row
#' @param SDs This the the allowed threshold of standard deviations away from the mean
#' @param transformTable This is the table received from ScanR that provides information what 
#' features and how they need to be transformed
#' @param scalingMethod This is the way the data will be scaled
#' @param imputationMethod This is the way missing data will be treated
#' @return a data.table with all the preprocessed data
#' @export
#' @examples
#' DT = data.table::data.table(plyr::baseball)
#' DT = LoadR(DT)
#' rs = scanr(DT, correlationCutoff = 0.99, SamplingPercentage=15, percentUniqueCutoff=5)
#' if (length(names(DT)) == length(unique(names(DT)))) {
#'	data.table::setnames(DT, names(DT), rs$Predictors)
#' } else {
#' 	names(DT) = rs$Predictors
#' }
#' predictors = rs[Status=='Healthy' | Status=='Uniform' ,Predictors]
#' transformTable = rs[abs(Skewness)>abs(selectedTransformationBoundary),
#' c('Predictors', 'Skewness'), with=F]
#' results = scrubr(DT, predictors, rowWiseMissingPercentage=100, SDs=8,
#' transformTable=transformTable, scalingMethod='robustZscore', imputationMethod='CWD')


ScrubR = function(DT, predictors, rowWiseMissingPercentage, SDs, transformTable, scalingMethod, imputationMethod) {
  
  # Remove Data Based Case Wise Missing Percentage
  DT[['percentageMissing']] = missingDataPerRecord(DT[,predictors, with=F])
  DT = DT[percentageMissing<=rowWiseMissingPercentage]
  
  # Remove row-wise outliers (provide predictors as columns), SDs is the number of standard deviations considered as outliers
  DT[,(predictors):=lapply(.SD, function(x) eliminateOutliers(x, SDs)), .SDcols=predictors]
  
  # Transform data based on skewness
  #rs <<- rs # take results
  #transformTable = rs[abs(Skewness)>abs(selectedTransformationBoundary),c('Predictors', 'Skewness'), with=F] # filter based on boundary
  tranformVariables = transformTable[['Predictors']] # take predictors
  DT[,(tranformVariables):= lapply(1:length(tranformVariables), function(x) applyTransformation(data=DT[[tranformVariables[x]]], skewness=transformTable[Predictors==tranformVariables[x],]$Skewness))] # apply transformation(s)
  
  # Feature scaling for each column (provide predictors as columns), scalingMethod is the way to scale
  DT[,(predictors):=lapply(.SD, function(x) featureScaling(x, scalingMethod)), .SDcols=predictors]
  
  # Data Imputation (Taking care of missing data)
  if (imputationMethod=='CWD') {
    
    # Case Wise Deletion
    DT = DT[complete.cases(DT[,predictors, with=F])]
  } else if(imputationMethod=='median') {
    # median imputation
    DT[, (predictors) := lapply(.SD, function(x) impute.median(x)), .SDcols = predictors]
    
  } else if(imputationMethod=='mean') {
    # mean imputation
    DT[, (predictors) := lapply(.SD, function(x) impute.mean(x)), .SDcols = predictors]
    
  } else if(imputationMethod=='mode') {
    # mode imputation
    DT[, (predictors) := lapply(.SD, function(x) impute.mode(x)), .SDcols = predictors]
    
  } else if(imputationMethod=='max') {
    # max imputation
    DT[, (predictors) := lapply(.SD, function(x) impute.max(x)), .SDcols = predictors]
    
  } else if(imputationMethod=='sample') {
    # random imputation
    mice.imp = try(mice::mice(DT[,predictors, with=F],m=1,maxit=1,meth='sample'))
    if (class(mice.imp)!='try-error') { # mice results a list
      try(DT[, (predictors) := mice::complete(mice.imp,action = 1, include = FALSE)]) #complete(plate.imp,action = 1, include = FALSE))
    }
    
  } else if(imputationMethod=='pmm') {
    # regression
    mice.imp = try(mice::mice(DT[,predictors, with=F],m=1,maxit=1,meth='pmm'))
    if (class(mice.imp)!='try-error') { # mice results a list
      try(DT[, (predictors) := mice::complete(mice.imp,action = 1, include = FALSE)]) #complete(plate.imp,action = 1, include = FALSE))
    }
    
  } else if(imputationMethod=='rf') {
    # random forest
    mice.imp = try(mice::mice(DT[,predictors, with=F],m=1,maxit=1,meth='rf'))
    if (class(mice.imp)!='try-error') { # mice results a list
      try(DT[, (predictors) := mice::complete(mice.imp,action = 1, include = FALSE)]) #complete(plate.imp,action = 1, include = FALSE))
    }
  }
  return(DT)
}

##########
# SmashR #
##########

#' SmashR'
#' 
#' This function 'smashes' the data, it calculates various statistics per unit of analysis (UoA)
#' @param DT This is a data.table object
#' @param UoA This is the variable in the data.table describing the unit of analysis
#' @param predictors These are the clean predictors provided manually or from the output of ScanR
#' @return a data.table with all the statistics per UoA
#' @export
#' @examples
#' DT = data.table::data.table(plyr::baseball)
#' UoA = 'team'
#' predictors = c('g', 'ab', 'r', 'h')
#' rs = SmashR(DT, UoA, predictors)
SmashR = function(DT, UoA, predictors) { 
  # calculate statistics per unit of analysis
  DT = DT[complete.cases(DT[,UoA, with=F])] # removing all records with NAs in the column that defines the UoA
  
  DT[,'UoA':=tidyr::unite(DT[, UoA, with=F])]
  resultingVariables = c('UoA', predictors)
  DT = DT[,lapply(.SD, function(x) calculateEstimator(x)), .SDcols=predictors, by=UoA]
  
  UoAVector = unique(DT[['UoA']])
  DT = split(DT, DT$UoA)
  DT = lapply(DT, function(x) unlist(x[,predictors, with=F]))
  data.table::setDT(DT)
  DT = data.table::data.table(t(DT))
  estimators = c('MIN', 'Q1', 'MEDIAN', 'MEAN', 'IQR', 'Q3', 'MAX', 'SD', 'MAD', 'CV', 'SE', 'NAs', 'COUNT', 'PercentUnique')
  data.table::setnames(DT, names(DT), c(sapply(predictors, function(x) paste(x, estimators, sep='_'))))
  DT[['UoA']] = UoAVector
  return(DT)
}