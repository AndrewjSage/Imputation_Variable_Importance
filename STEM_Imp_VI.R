source("Imputation_VI_Functions.R")

STEM <- readRDS("Sims/STEM1.rds")  #Include all students, not only those who stayed at ISU
STEM <- STEM[STEM$Year==2016, ]
STEM$`Learning Community Participation` <- as.numeric(STEM$`Learning Community Participation`!=0)
STEM <- STEM[, c(3, 5, 6, 26, 27, 30, 31, 32, 40, 47, 50, 54, 55, 56, 57, 62)]
varnames <- c("ACT",                            
              "GPA",                                
              "Gender",                                         
              "Athlete",                                    
              "Age",                                            
              "ALEKS",             
              "LC",               
               "STEM Interest",     
               "Remedial Math",            
              "Parent Education",                         
              "Hrs. Studying*",
              "Academic Skills*",                       
              "Social Integration*",                    
              "Peer Connections*",                      
              "Self Efficacy*",        
              "Class"  )
names(STEM) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "y")


Propmiss <- function(x){
  sum(is.na(x)/length(x))  
}
round(apply(STEM, 2, Propmiss),3)



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





ntechs <- 10
nreps <- 10
x <- STEM[,-ncol(STEM)]
y <- STEM[,ncol(STEM)]
VI <- array(NA, dim=c(nreps, ncol(x), ntechs))  #rows correspond to variables, columns to imputation techniques

set.seed(12062019)

for (it in 1:nreps){
#Strawman-median imputation
tech <- 1
Imputed <- cbind(na.roughfix(x), y)
rfMiss <- rfsrc(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[it,,tech]<-rfMiss$importance[,1]}else{VI[it,,tech]<-rfMiss$importance}

#rfimpute
tech <- 2
Imputed <- rfImpute(x, y, iter=5)[,c(2:(ncol(x)+1),1)]
rfMiss <- rfsrc(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[it,,tech]<-rfMiss$importance[,1]}else{VI[it,,tech]<-rfMiss$importance}

#Impute using missForest
tech <- 3
ImputedX <- impute(data = x, mf.q = 1/ncol(x))
Imputed <- cbind(ImputedX, y)
rfMiss <- rfsrc(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[it,,tech]<-rfMiss$importance[,1]}else{VI[it,,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector

#Impute using RFSRC-1 iteration
tech <- 4
rfMiss <- rfsrc(y~., data=STEM, ntree=500,importance=c("permute"), na.action=c("na.impute"), nimpute=1)
if(is.factor(y)){VI[it,,tech]<-rfMiss$importance[,1]}else{VI[it,,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector

#Impute using RFSRC-5 iterations
tech <- 5
rfMiss <- rfsrc(y~., data=STEM, ntree=500,importance=c("permute"), na.action=c("na.impute"), nimpute=5)
if(is.factor(y)){VI[it,,tech]<-rfMiss$importance[,1]}else{VI[it,,tech]<-rfMiss$importance} #If y is a factor, permutation importance is given in 1st col. Otherwise just a vector

# RFSRC unsupervised - 1 iteration
tech <- 6  
ImputedX <- impute(data = x, nimpute = 1)
Imputed <- cbind(ImputedX, y)
rfMiss <- randomForest(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[it,,tech]<-rfMiss$importance[,3]}else{VI[it,,tech]<-rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first

# RFSRC unsupervised - 5 iterations
tech <- 7
ImputedX <- impute(data = x, nimpute = 5)
Imputed <- cbind(ImputedX, y)
rfMiss <- randomForest(y~., data=Imputed, ntree=500,importance=TRUE)
if(is.factor(y)){VI[it,,tech]<-rfMiss$importance[,3]}else{VI[it,,tech]<-rfMiss$importance[,1]} #If y is a factor, permutation importance is given in 3rd col. Otherwise first

#Impute using CALIBER
#since this is a multiple imputation technique, perform nmult times then average VI
tech <- 8
VImat <- replicate(n=5, CaliberVImvt(x,y))
VImat1 <- apply(VImat, 2, function(x){x/sum(x)}) #scale so each rep counts equally
VI[it,,tech] <- rowMeans(VImat)

#Impute using mice
#since this is a multiple imputation technique, perform nmult times then average VI
tech <- 9
VImat <- replicate(n=5, miceVImvt(x,y))
VImat1 <- apply(VImat, 2, function(x){x/sum(x)}) #scale so each rep counts equally
VI[it,,tech] <- rowMeans(VImat)


#Complete Cases
tech <- 10
STEMsubset <- STEM[complete.cases(STEM),]
randomForest(y~., data=STEMsubset, ntree=500,importance=TRUE)
VI[it,,tech] <- rfMiss$importance[,1]/sum(rfMiss$importance[,1])

print(it)
}


VI[VI<0]<-0

VIsc <- apply(VI, 2, function(x){x/sum(x)}) #scale so each rep counts equally

round(VIcomb, 3)

###############################################

#Setup dataframe to show results
Technique <- rep(1:10, each=15)
Variable <- rep(1:15, 10)
Importance <- c(VIcomb)
IMPdf <- data.frame(Technique, Variable, Importance)

ggplot(data=IMPdf, aes(x=Technique, y=Importance)) + geom_col() + facet_grid(. ~ Variable)

ggplot(data=IMPdf, aes(x=Variable, y=Importance)) + geom_col() + facet_grid(. ~ Technique)

STEM1 <- STEM[complete.cases(STEM),]
library(corrplot)

STEM2 <- STEM1[, c(1, 2, 5, 6, 11:15) ]
M <- cor(STEM2)
corrplot(M)



