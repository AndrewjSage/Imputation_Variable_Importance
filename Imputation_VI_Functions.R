#setwd("/work/STAT/ajsage")

#Load Packages
library(randomForest)
library(randomForestSRC)
library(CALIBERrfimpute)
library(mice)
library(MASS)

#function to generate data for simulation described in paper
Generate_Sim_Data <- function(rho, size=1000, simsetting=1){
  Sigma <- matrix(rho, nrow=6, ncol=6)
  Sigma[1,1] <- Sigma[2,2] <- Sigma[3,3] <- Sigma[4,4] <- Sigma[5,5] <- Sigma[6,6] <- 1
  X <- mvrnorm(n = size, mu=c(0,0,0,0,0,0), Sigma=Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
  y <- rep(NA, size)
  epsilon <- rnorm(size,mean=0, sd=1)
  if (simsetting==1){
  y <- .5*X[,1]+.4*X[,2]+.3*X[,3]+.2*X[,4]+.1*X[,5]+epsilon
  } else{
      y <- 0.3*exp(X[,1])-0.4*X[,2]^2 + 0.5*X[,3]*X[,4] +.2*X[,5]+epsilon
  }  
  DATA <- data.frame(cbind(X,y))
  return(DATA)
}

#function to delete a percentage of values, p, for a variable x
DeleteMissing <- function(data, xvar, p, missingness){
  if(missingness == "MCAR"){
  Miss <- sample(1:nrow(data),p*nrow(data))
  } else if(missingness == "MAR"){
    var <- sample(setdiff((1:(ncol(data)-1)), c(xvar)), 1)
    weights <- 1/(1+exp(-3*data[,var]))  #assign sampling weights according to another randomly chosen x variable
    if(rbinom(1,0,1)==1){ #randomly determine whether to sample high or low values more heavily
      weights==1-weights
    }
    Miss <- sample(1:nrow(data),p*nrow(data), prob=weights)
  } else if(missingness == "MNAR"){
    weights <- 1/(1+exp(-3*data[,xvar])) #assign sampling weights according to variable being deleted/imputed
    if(rbinom(1,0,1)==1){ #randomly determine whether to sample high or low values more heavily
    weights==1-weights
    }
    Miss <- sample(1:nrow(data),p*nrow(data), prob=1/(1+exp(-3*data[,xvar])))
  } else{ stop("missingness must be set to either 'MCAR', 'MAR', or 'MNAR'")
  }
  data[Miss, xvar] <- NA
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
  if(is.factor(y)){VIvec <- rfMiss$importance[,3]}else{VIvec <- rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first
  return(VIvec) 
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
  if(is.factor(y)){VIvec <- rfMiss$importance[,3]}else{VIvec <- rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first
  return(VIvec) 
  }


#Function to delete values and perform imputation using each technique in question. 
Impute_and_VI <- function(data, ntreesimp=300, ntrees=500, nmult=5, ntechs=9, xvar){
  #separate predictor variables from response since some techniques require one or other
  x <- data[,-ncol(data)]
  y <- data[,ncol(data)]
  #setup dataframe to store variable importance results
  VI <- array(NA, dim=c(ncol(x), ntechs))  #rows correspond to variables, columns to imputation techniques
  
  #Strawman-median imputation
  tech <- 1
  if(sum(is.na(x))>0){
    Imputed <- data
      Imputed[which(is.na(Imputed[,xvar])), xvar] <- median(Imputed[, xvar], na.rm=TRUE)} else{  #reordered so y still last
      Imputed <- data
      }
  rfMiss <- rfsrc(y~., data=Imputed, ntree=ntrees,importance=TRUE)
  if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector
  
  
  #Impute using rfImpute
  tech <- 2
    if(sum(is.na(x))>0){
      Imputed <- rfImpute(x, y, iter=5, ntree=ntreesimp)[,c(2:(ncol(x)+1),1)]} else{  #reordered so y still last
      Imputed <- data
    }
  rfMiss <- rfsrc(y~., data=Imputed, ntree=ntrees,importance=TRUE)
  if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector
  
  #Impute using missForest
  tech <- 3
  if(sum(is.na(x))>0){
    ImputedX <- impute(data = x, mf.q = 1/ncol(x))
    Imputed <- cbind(ImputedX, y)} else{  
      Imputed <- data
    }  
  rfMiss <- rfsrc(y~., data=Imputed, ntree=ntrees,importance=TRUE)
  if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector
  
  #Impute using RFSRC-1 iteration
  tech <- 4
  rfMiss <- rfsrc(y~., data=data, ntree=ntrees,importance=c("permute"), na.action=c("na.impute"), nimpute=1)
  if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector

  #Impute using RFSRC-5 iterations
  tech <- 5
  rfMiss <- rfsrc(y~., data=data, ntree=ntrees,importance=c("permute"), na.action=c("na.impute"), nimpute=5)
  if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector
  
  # RFSRC unsupervised - 1 iteration
  tech <- 6  
  if(sum(is.na(x))>0){
    ImputedX <- impute(data = x, nimpute = 1)
    Imputed <- cbind(ImputedX, y)} else{  
    Imputed <- data
    }
  rfMiss <- randomForest(y~., data=Imputed, ntree=ntrees,importance=TRUE)
  if(is.factor(y)){VI[,tech]<-rfMiss$importance[,3]}else{VI[,tech]<-rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first
  
  # RFSRC unsupervised - 5 iterations
  tech <- 7
  if(sum(is.na(x))>0){
    ImputedX <- impute(data = x, nimpute = 5)
    Imputed <- cbind(ImputedX, y)} else{  
    Imputed <- data
    }
  rfMiss <- randomForest(y~., data=Imputed, ntree=ntrees,importance=TRUE)
  if(is.factor(y)){VI[,tech]<-rfMiss$importance[,3]}else{VI[,tech]<-rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first
  
  #Impute using CALIBER
  #since this is a multiple imputation technique, perform nmult times then average VI
  tech <- 8
  VImat <- replicate(n=nmult, CaliberVI(x,y, ntreesimp = ntreesimp, ntrees=ntrees, xvar=xvar))
  VI[,tech] <- rowMeans(VImat)
  
  #Impute using mice
  #since this is a multiple imputation technique, perform nmult times then average VI
  tech <- 9
  VImat <- replicate(n=nmult, miceVI(x,y,ntreesimp = ntreesimp, ntrees=ntrees, xvar=xvar))
  VI[,tech] <- rowMeans(VImat)
  
  return(VI)  
}


#Function to do deletion and imputation. Apply this for different xvars after data have been generated
Del_Impute <- function(data, xvar, pvec, ntrees=500, missingness){
  Deleted_Data <- lapply(X=pvec, data=data, FUN=DeleteMissing, xvar=xvar, missingness=missingness)
  VI <- lapply(X=Deleted_Data, FUN=Impute_and_VI, xvar=xvar)
  return(VI)
}

#function to generated data, then delete and impute for all variables of interest and measure VI
Gen_Del_Impute <- function(rho, xvarvec, pvec, size=100, ntrees=500, missingness="MCAR", simsetting=1){
  DATA <- Generate_Sim_Data(rho=rho, size=size, simsetting=simsetting)
  VI <- lapply(X=xvarvec, pvec=pvec, data=DATA, FUN=Del_Impute, missingness=missingness)
  return(VI)
}

#function to delete and impute for all variables of interest for given dataset and measure VI
Del_Impute_wrapper <- function(data, xvarvec, pvec, ntrees=500, missingness="MCAR"){
  VI <- lapply(X=xvarvec, pvec=pvec, data=data, FUN=Del_Impute, missingness=missingness)
  return(VI)
}

Res <- Gen_Del_Impute(rho=0, xvarvec=4, pvec=c(0.25), size=1000, ntrees=500, missingness="MCAR", simsetting=1)
Res
