setwd("/work/STAT/ajsage")
source("Imputation_VI_Functions.R")
library(parallel)


STEM <- readRDS("STEM1.rds")  #Include all students, not only those who stayed at ISU
STEM <- STEM[STEM$Year==2016, ]
STEM$`Learning Community Participation` <- as.numeric(STEM$`Learning Community Participation`!=0)
STEM <- STEM[, c(3, 5, 6, 26, 27, 30, 31, 32, 40, 47, 50, 54, 55, 56, 57, 62)]
STEM <- STEM[complete.cases(STEM), ]
#STEMnames <- names(STEM)
names(STEM) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "y")

#rf <- randomForest(data=STEM, y~., importance=TRUE)
#rf$importance*1000

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

clusterExport(cl, c("Impute_and_VI", "CaliberVI", "miceVI", "Gen_Del_Impute", "Generate_Sim_Data", "DeleteMissing", "Del_Impute", "Del_Impute_wrapper", "STEM"))
clusterEvalQ(cl, {
  library(randomForest)
  library(randomForestSRC)
  library(CALIBERrfimpute)
  library(mice)
  library(MASS)
})


clusterSetRNGStream(cl, 02012018)
MVVIMP <- parSapply(cl=cl, X=1:100,FUN=function(i){Del_Impute_wrapper(data=xyabv, xvarvec=c(1,2,6,14),pvec=c(0, 0.1, 0.25, 0.5, 0.75), ntrees=500, missingness="MCAR")} )
stopCluster(cl)


save(MVVIMP, file="MVVIMP_STEM_MCAR.Rdata")

