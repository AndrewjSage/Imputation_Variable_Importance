setwd("/work/STAT/ajsage")
source("Imputation_VI_Functions.R")
library(parallel)

#Apply iteratively using parSapply

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

clusterExport(cl, c("Impute_and_VI", "CaliberVI", "miceVI", "Gen_Del_Impute", "Generate_Sim_Data", "DeleteMissing", "Del_Impute"))
clusterEvalQ(cl, {
  library(randomForest)
  library(randomForestSRC)
  library(CALIBERrfimpute)
  library(mice)
  library(MASS)
})

clusterSetRNGStream(cl, 02012018)
MVVIMP <- parSapply(cl=cl, X=1:100, FUN=function(i){Gen_Del_Impute(rho=0.5, xvarvec=c(1,3,5),pvec=c(0, 0.1, 0.25, 0.5, 0.75), size=1000, ntrees=500, missingness="MAR", simsetting=2)})
stopCluster(cl)

save(MVVIMP, file="sim2_rho_50_MAR.Rdata")
