setwd("/work/STAT/ajsage")
source("Imputation_VI_Functions.R")
library(parallel)


library(mlbench)
data("BostonHousing")

xyabv <- BostonHousing
xyabv$chas <- as.numeric(as.character(xyabv$chas))
names(xyabv)[14]="y"
#xabv=xyabv[,-14]
#yabv=xyabv$y

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

clusterExport(cl, c("Impute_and_VI", "CaliberVI", "miceVI", "Gen_Del_Impute", "Generate_Sim_Data", "DeleteMissing", "Del_Impute", "Del_Impute_wrapper", "xyabv"))
clusterEvalQ(cl, {
  library(randomForest)
  library(randomForestSRC)
  library(CALIBERrfimpute)
  library(mice)
  library(MASS)
})


clusterSetRNGStream(cl, 02012018)
MVVIMP <- parSapply(cl=cl, X=1:100,FUN=function(i){Del_Impute_wrapper(data=xyabv, xvarvec=c(1,4,6,7),pvec=c(0, 0.1, 0.25, 0.5, 0.75), ntrees=500, missingness="MAR")} )
stopCluster(cl)

save(MVVIMP, file="MVVIMP_BH_MAR.Rdata")

