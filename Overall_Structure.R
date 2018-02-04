
#Load Packages
library(randomForest)
library(randomForestSRC)
library(missForest)
library(CALIBERrfimpute)
library(mice)
library(MASS)

#function to generate data for simulation described in paper
Generate_Sim_Data <- function(rho, size=1000){
Sigma <- matrix(rho, nrow=6, ncol=6)
Sigma[1,1] <- Sigma[2,2] <- Sigma[3,3] <- Sigma[4,4] <- Sigma[5,5] <- Sigma[6,6] <- 1
X <- mvrnorm(n = size, mu=c(0,0,0,0,0,0), Sigma=Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
y <- rep(NA, size)
epsilon <- rnorm(size,mean=0, sd=1)
y <- .5*X[,1]+.4*X[,2]+.3*X[,3]+.2*X[,4]+.1*X[,5]+epsilon
DATA <- data.frame(cbind(X,y))
return(DATA)
}

#function to delete a percentage of values, p, for a variable x
DeleteMissing <- function(data, xvar, p){
  Miss <- rbinom(nrow(data),1,p)
  data[Miss==1 ,xvar] <- NA
  return(data)
}

#Function to impute missing values and compute variable importance using Shah's method
#This function does the imputation and VI once. Apply it nmult times for multiple imputation, as is done in Impute_VI
CaliberVI <- function(x, y, ntreesimp, ntrees, xvar){
  if(sum(is.na(x))>0){
    ImputedX <-x #first set imputed dataset equal to one with missing values then fill missing values
    if(is.factor(x[,xvar])){
    ImputedX[is.na(x[,xvar]),xvar] <- mice.impute.rfcat(x[,xvar],!is.na(x[,xvar]), x[,-xvar], iter=5, ntree=ntreesimp)
  } else{
    ImputedX[is.na(x[,xvar]),xvar] <- mice.impute.rfcont(x[,xvar],!is.na(x[,xvar]), x[,-xvar], iter=5, ntree=ntreesimp)
  }
  Imputed <- cbind(ImputedX, y)} else{
  Imputed <- cbind(x,y)}
  rfMiss=randomForest(y~., data=Imputed, ntree=ntrees,importance=TRUE)
  return(rfMiss$importance[,1])
}

#Function to impute missing values and compute variable importance using Doove's method
#This function does the imputation and VI once. Apply it nmult times for multiple imputation, as is done in Impute_VI
miceVI <- function(x,y,ntreesimp, ntrees, xvar){
  if(sum(is.na(x))>0){
  ImputedX <-x #first set imputed dataset equal to one with missing values then fill missing values
  ImputedX[is.na(x[,xvar]),xvar] <- mice.impute.rf(x[,xvar],!is.na(x[,xvar]), x[,-c(xvar, ncol(x))], iter=5, ntree=ntreesimp)
  Imputed <- cbind(ImputedX, y)} else{
  Imputed <- cbind(x,y)}   
  rfMiss=randomForest(y~., data <- Imputed, ntree=ntrees,importance=TRUE)
  return(rfMiss$importance[,1])  
}



Impute_and_VI <- function(data, ntreesimp=300, ntrees=500, nmult=5, ntechs=6, xvar){
 #separate predictor variables from response since some techniques require one or other
   x <- data[,-ncol(data)]
   y <- data[,ncol(data)]
  #setup dataframe to store variable importance results
    VI <- array(NA, dim=c(ncol(x), ntechs))  #rows correspond to variables, columns to imputation techniques
  
  #Impute using rfImpute
  if(sum(is.na(x))>0){
  Imputed <- rfImpute(x, y, iter=5, ntree=ntreesimp)[,c(2:(ncol(x)+1),1)]} else{  #reordered so y still last
  Imputed <- data
  }
  rfMiss <- randomForest(y~., data=Imputed, ntree=ntrees,importance=TRUE)
  VI[,1] <- rfMiss$importance[,1] #store variable importance resulting from RFimpute in first column  
  
  #Impute using missForest
  if(sum(is.na(x))>0){
  ImputedX <- missForest(x, maxiter=10, ntree=ntreesimp, verbose=F)$ximp
  Imputed <- cbind(ImputedX, y)} else{  
    Imputed <- data
  }
  rfMiss <- randomForest(y~., data=Imputed, ntree=ntrees,importance=TRUE)
  VI[,2] <- rfMiss$importance[,1] #store variable importance resulting from RFimpute in first column  
  
  #Impute using RFSRC-1 iteration
  rfMiss <- rfsrc(y~., data=data, ntree=ntrees,importance=c("permute"), na.action=c("na.impute"), nimpute=1)
  VI[,3] <- rfMiss$importance
  
  #Impute using RFSRC-5 iterations
  rfMiss <- rfsrc(y~., data=data, ntree=ntrees,importance=c("permute"), na.action=c("na.impute"), nimpute=5)
  VI[,4] <- rfMiss$importance
  
  #Impute using CALIBER
  #since this is a multiple imputation technique, perform nmult times then average VI
  VImat <- replicate(n=nmult, CaliberVI(x,y, ntreesimp = ntreesimp, ntrees=ntrees, xvar=xvar))
  VI[,5] <- rowMeans(VImat)
  
  #Impute using mice
  #since this is a multiple imputation technique, perform nmult times then average VI
  VImat <- replicate(n=nmult, miceVI(x,y,ntreesimp = ntreesimp, ntrees=ntrees, xvar=xvar))
  VI[,6] <- rowMeans(VImat)

  return(VI)  
}


#Function to do deletion and imputation. Apply this for different xvars after data have been generated
Del_Impute <- function(data, xvar, pvec, ntrees=500){
  Deleted_Data <- lapply(X=pvec, data=data, FUN=DeleteMissing, xvar=xvar)
  VI <- lapply(X=Deleted_Data, FUN=Impute_and_VI, xvar=xvar)
  return(VI)
}

#function to generated data, then delete and impute for all variables of interest.
Gen_Del_Impute <- function(rho, xvarvec, pvec, size=100, ntrees=500){
  DATA <- Generate_Sim_Data(rho, size)
  VI <- lapply(X=xvarvec, pvec=pvec, data=DATA, FUN=Del_Impute)
  return(VI)
}


##############################################################################################

# just generate a single dataset, and delete and impute x1 and x5 for proportions missing given in pvec
MVVI <- Gen_Del_Impute(rho=0.75, xvarvec=c(1,5),pvec=c(0, 0.1, 0.25, 0.5, 0.75), size=100, ntrees=500)
do.call(rbind, MVVI)[2,5]

#Do 2 repetitions for each p and xvar using lapply, no other parallelization
MVVIMP <- sapply( X=1:3, FUN=function(i){Gen_Del_Impute(rho=0.75, xvarvec=c(1,5),pvec=c(0, 0.1, 0.25, 0.5, 0.75), size=100, ntrees=500)})
MVVIMP #rows are variables, columns are iterations, fill is list of five  6x6 tables for VI with x1-x6 as rows and 6 techniques as columns, five tables range over prop. missing
#Ex MVVIMP[1,3][[1]][[2]] gives 6x6 VI table for deleting and imputing x1, 10% missing, in 3rd iteration.



####################################################################################################
#Apply iteratively using parSapply

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

clusterExport(cl, c("Impute_and_VI", "CaliberVI", "miceVI", "Gen_Del_Impute", "Generate_Sim_Data", "DeleteMissing", "Del_Impute"))
clusterEvalQ(cl, {
  library(randomForest)
  library(randomForestSRC)
  library(missForest)
  library(CALIBERrfimpute)
  library(mice)
  library(MASS)
})
MVVIMP <- parSapply(cl=cl, X=1:3, FUN=function(i){Gen_Del_Impute(rho=0.75, xvarvec=c(1,5),pvec=c(0, 0.1, 0.25, 0.5, 0.75), size=100, ntrees=500)})

stopCluster(cl)





############################################################################################
#run in parallel using foreach

library(foreach)
library(doParallel)

cl<-makeCluster(no_cores)
registerDoParallel(cl)

MVVIMP <- foreach(i=1:3, 
                  .packages = c("randomForest", "randomForestSRC", "missForest", "CALIBERrfimpute", "mice", "MASS")
                                        ) %dopar%
              (Gen_Del_Impute(rho=0.75, xvarvec=c(1,5),pvec=c(0, 0.1, 0.25, 0.5, 0.75), size=100, ntrees=500))

stopCluster(cl)

do.call(rbind,MVVIMP)

MVVIMP  #stored as [[repeition]][[xvar]][[prop.missing]]

MVVIMP[[3]][[2]][[4]] #gives 6x6 VI table 3rd rep, x5 (2nd variable) , 50% missing. Rows in 6x6 table are variables, columns are techniques
