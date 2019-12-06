#Includes all students. Attrition could mean leaving STEM or leaving university. 

STEM <- readRDS("STEM.rds")
STEM1 <- readRDS("STEM1.rds")
names(STEM1)
summary(STEM1)

library(dplyr)
STEM1 <- filter(STEM1, Year==2016)
names(STEM1)

#This function does the imputation and VI once. Apply it nmult times for multiple imputation, as is done in Impute_VI
CaliberVImvt <- function(x, y){
  vecdata <- unlist(c(x))  
    ImputedX <-x #first set imputed dataset equal to one with missing values then fill missing values
    ImputeInfo <- mice(ImputedX, method = c('rfcont'), m = 1, maxit = 5)
    Imputedvals <- unlist(ImputeInfo$imp)
    vecdata[is.na(vecdata)] <- Imputedvals
    ImputedX <- data.frame(matrix(vecdata, nrow=nrow(x), ncol=ncol(x), byrow=FALSE))
    names(ImputedX) <- names(x)
    Imputed <- cbind(ImputedX, y)
    rfMiss=randomForest(y~., data <- Imputed, ntree=500,importance=TRUE)
    if(is.factor(y)){VIvec <- rfMiss$importance[,3]}else{VIvec <- rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first
  return(VIvec) 
}



#Function to impute missing values and compute variable importance using Doove's method
#This function does the imputation and VI once. Apply it nmult times for multiple imputation, as is done in Impute_VI
miceVImvt <- function(x,y){
  vecdata <- unlist(c(x))  
  ImputeInfo <- mice(data=x, m=1, method="rf")
  Imputedvals <- unlist(ImputeInfo$imp)
  vecdata[is.na(vecdata)] <- Imputedvals
  ImputedX <- data.frame(matrix(vecdata, nrow=nrow(x), ncol=ncol(x), byrow=FALSE))
  names(ImputedX) <- names(x)
  Imputed <- cbind(ImputedX, y)
  rfMiss=randomForest(y~., data <- Imputed, ntree=500,importance=TRUE)
  if(is.factor(y)){VIvec <- rfMiss$importance[,3]}else{VIvec <- rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first
  return(VIvec) 
}



Propmiss <- function(x){
sum(is.na(x)/length(x))  
}
apply(STEM1, 2, Propmiss)

STEM1 <- STEM1 %>% select(`ACT Composite Score`, `High School Rank`, `Gender`, `Greek Life Participation`, `ISU Athlete`, `Age`,
                          `Iowa Resident`, `ALEKS Math Placement Overall Score`, `Learning Community Participation`, `Selected STEM Major on ACT Interest Survey`, 
                          `Mapworks-ISU Rank in Choices of College`, `Mapworks-Aspired Level of Education`, `Mapworks-Hours Studying per Week`, `Mapworks-Academic Skills`, 
                          `Mapworks-Social Integration`, `Mapworks-Math and Science Self Efficacy`, `Class`)
STEM1$`Learning Community Participation` <- as.numeric(STEM1$`Learning Community Participation` != "0")
STEM1$`Iowa Resident` <- as.numeric(STEM1$`Iowa Resident`)
STEM1$Class <- factor(STEM1$Class)

ntechs=9
data <- STEM1
names(data) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9" ,"x10" ,"x11" ,"x12", "x13", "x14", "x15", "x16", "y")
x <- data[,-ncol(data)]
y <- data[,ncol(data)]
VI <- array(NA, dim=c(ncol(x), ntechs))  #rows correspond to variables, columns to imputation techniques



#Strawman-median imputation
tech <- 1
Imputed <- cbind(na.roughfix(x), y)
rfMiss <- rfsrc(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance}

#rfimpute
tech <- 2
Imputed <- rfImpute(x, y, iter=5)[,c(2:(ncol(x)+1),1)]
rfMiss <- rfsrc(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance}
                          
#Impute using missForest
tech <- 3
  ImputedX <- impute(data = x, mf.q = 1/ncol(x))
  Imputed <- cbind(ImputedX, y)
rfMiss <- rfsrc(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector

#Impute using RFSRC-1 iteration
tech <- 4
rfMiss <- rfsrc(y~., data=data, ntree=500,importance=c("permute"), na.action=c("na.impute"), nimpute=1)
if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector

#Impute using RFSRC-5 iterations
tech <- 5
rfMiss <- rfsrc(y~., data=data, ntree=500,importance=c("permute"), na.action=c("na.impute"), nimpute=5)
if(is.factor(y)){VI[,tech]<-rfMiss$importance[,1]}else{VI[,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector

# RFSRC unsupervised - 1 iteration
tech <- 6  
  ImputedX <- impute(data = x, nimpute = 1)
  Imputed <- cbind(ImputedX, y)
rfMiss <- randomForest(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[,tech]<-rfMiss$importance[,3]}else{VI[,tech]<-rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first

# RFSRC unsupervised - 5 iterations
tech <- 7
  ImputedX <- impute(data = x, nimpute = 5)
  Imputed <- cbind(ImputedX, y)
rfMiss <- randomForest(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[,tech]<-rfMiss$importance[,3]}else{VI[,tech]<-rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first

#Impute using CALIBER
#since this is a multiple imputation technique, perform nmult times then average VI
tech <- 8
VImat <- replicate(n=5, CaliberVImvt(x,y))
VImat1 <- apply(VImat, 2, function(x){x/sum(x)}) #scale so each rep counts equally
VI[,tech] <- rowMeans(VImat)

#Impute using mice
#since this is a multiple imputation technique, perform nmult times then average VI
tech <- 9
VImat <- replicate(n=5, miceVImvt(x,y))
VImat1 <- apply(VImat, 2, function(x){x/sum(x)}) #scale so each rep counts equally
VI[,tech] <- rowMeans(VImat)


VIsc <- apply(VI, 2, function(x){x/sum(x)}) #scale so each rep counts equally

STEMsubset <- STEM1[complete.cases(STEM1),]
randomForest(y~., data=STEMsubset, ntree=500,importance=TRUE)
VIcc <- rfMiss$importance[,3]/sum(rfMiss$importance[,3])

cbind(VIsc, VIcc)
