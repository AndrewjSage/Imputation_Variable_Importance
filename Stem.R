setwd("/work/STAT/ajsage")
source("Imputation_Functions.R")

#Load Packages
library(randomForest)
library(randomForestSRC)
library(missForest)
library(CALIBERrfimpute)
library(mice)
library(MASS)
load("STEMabv")

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

clusterExport(cl, c("Impute_and_VI", "CaliberVI", "miceVI", "Del_Impute_wrapper", "Generate_Sim_Data", "DeleteMissing", "Del_Impute", "xyabv"))
clusterEvalQ(cl, {
  library(randomForest)
  library(randomForestSRC)
  library(missForest)
  library(CALIBERrfimpute)
  library(mice)
  library(MASS)
})


clusterSetRNGStream(cl, 02012018)
RES <- parSapply(cl=cl, X=1:100,FUN=function(i){Del_Impute_wrapper(data=xyabv, xvarvec=c(1,6,10,11),pvec=c(0, 0.1, 0.25, 0.5, 0.75), ntrees=500)} )
stopCluster(cl)

save(RES, file="MVVIMP_STEM.Rdata")