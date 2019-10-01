library(tidyverse)
library(gridExtra)

load("sim1_rho_75_MCAR.Rdata")
#dims of MVVIMP are 3x100, 3 different variables deleted/imputed, 100 different reps
#MVVIMP[a,b] is a list of 5 6x9 matrices. These correspond to:
        # 5 proportions of missingness (0, .1, .25, .5, .75)
        # 6 explanatory variables (x1, x2, x3, x4, x5, x6)
        # 9 imputation techniques ( 1-median, 2-rfImpute, 3-missForest, 4-RFSRC1sup, 5-RFSRC5sup, 6-RFSRC1unsp, 7-RFSRC5unsp, 8-CALIBER, 9-mice)

#function to divide importance for each variable by sum of all importances. If negative, set to 0.
scaleVI <- function(x){
  x[x<0]  <- 0
  return(x/sum(x))
}


#Function to take in object with results from a simulation (for a given rho) 
#Need to specify number of predictors deleted and imputed, number of values used for p,
#number of iterations, 
#and return 5-dimensional array
#first dim is pred-var deleted and imputed
#second is prop missing
#third is iteration
#4th is x-variable whose importance is being considered
#5th is technique
SummaryFunc <- function(Obj){
  np=5 #number of different proportions of missingness
  nvarsimp <-dim(Obj)[1]  # number of variables deleted and imputed
  niter <- dim(Obj)[2]
  nvarstot <- dim(Obj[1,1][[1]][[1]])[1]  #number of predictor variables total
  ntechs <- 9
  A <- array(NA, dim=c(nvarsimp, niter,np,nvarstot,ntechs))
  for (var in 1:nvarsimp){
    for (p in 1:np){
      for (i in 1:niter){
        A[var,i,p,,]<- apply(Obj[var,i][[1]][[p]], 2, scaleVI) #pull out table corresponding to variable, iteration, and prop of interest and scale columns to add to 1
      } 
    }
  }
  return(A)
}

#Given variable deleted, compute summary in dataframe format for graphing
#Array argument is an object from SummaryFunc
#here xvar indexing only includes those who were deleted and imputed. (1=x1, 2=x3, 3=x5 when deleted vars are x1, x3, x5)
Summarydf <- function(Array, xvar, pind){  #take in array with mean variable importance for each setting, and the xvar, prop of interest
  MEANIMP <- apply(Array, c(1,3,4,5), mean)
  SDIMP <- apply(Array, c(1,3,4,5), sd)
  n <- dim(Array)[2]
  ntechs=dim(Array)[5]
  VI <- as.vector(MEANIMP[xvar, pind, ,])
  SD <- as.vector(SDIMP[xvar, pind, ,])
  var <- rep(1:dim(Array)[4], ntechs)  #list pred. vars 1:dim(Array)[4], repeated for each of 6 techniques
  pvec <- c(0, 0.10, 0.25, 0.50, 0.75)
  p <- rep(pvec[pind], dim(Array)[4]*ntechs)
  Techs <- c("median", "rfImpute", "missForest", "rfsrc-1s", "rfsrc-5s", "rfsrc-1u", "rfsrc-5u","CALIBER", "rfmice")
  Method <- rep(Techs, each=dim(Array)[4])
  Lower <- VI-qnorm(0.975)*SD/sqrt(n)
  Upper <- VI+qnorm(0.975)*SD/sqrt(n)
  df <- data.frame(VI, var, p, Method, Lower, Upper)
  return(df)
}


#take in list of dataframes from Summarydf and create plot need to specify varible number 
#here xvar indexing includes all, and xvar is index of variable we want to plot
Createplot <- function(dfList, xvar){ 
  df <- do.call("rbind", dfList)
  df <- subset(df, var==xvar)
  p <- ggplot(df, aes(x=p, y=VI, colour = Method))+theme(text = element_text(size=16))
  dodge <- position_dodge(width=0.05)
  p1 <- p +geom_line(aes(group = Method)) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), position = dodge, width = 0.05)+ labs(x = "Proportion Missing", y="Scaled Importance")+ theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
  return(p1)
}


#Get legend at bottom of all plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

####################################################################################

#Function to create graphic for all variables with a given missingness level
Create_Combined_Graphic <- function(type, sim, xvar, xvarused, ylower, yupper){
  #type is "MAR, MNAR, MCAR"
  #sim 1, 2, "BH", or "STEM"
  #xvar is variable deleted and imputed
  #xvarused is the vector of variables deleted and imputed in the simulation should always be c(1,3,5) 
  #ylower and yupper are bounds for y-axis
  
  rho <- 0
  load(paste("sim",sim,"_rho_",rho,"_",type,".Rdata", sep = ""))
  A <- SummaryFunc(MVVIMP)
  dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=which(xvarused==xvar))
  p1 <- Createplot(dfList, xvar=xvar)+ theme(legend.position = "bottom")+ylim(c(ylower,yupper))+ggtitle(expression(paste(rho,"=0")))
  
  rho <- 25
  load(paste("sim",sim,"_rho_",rho,"_",type,".Rdata", sep = ""))
  A <- SummaryFunc(MVVIMP)
  dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=which(xvarused==xvar))
  p2 <- Createplot(dfList, xvar=xvar)+ theme(legend.position = "bottom")+ylim(c(ylower,yupper))+ggtitle(expression(paste(rho,"=0.25")))
  
  rho <- 50
  load(paste("sim",sim,"_rho_",rho,"_",type,".Rdata", sep = ""))
  A <- SummaryFunc(MVVIMP)
  dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=which(xvarused==xvar))
  p3 <- Createplot(dfList, xvar=xvar)+ theme(legend.position = "bottom")+ylim(c(ylower,yupper))+ggtitle(expression(paste(rho,"=0.5")))
  
  rho <- 75
  load(paste("sim",sim,"_rho_",rho,"_",type,".Rdata", sep = ""))
  A <- SummaryFunc(MVVIMP)
  dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=which(xvarused==xvar))
  p4 <- Createplot(dfList, xvar=xvar)+ theme(legend.position = "bottom")+ylim(c(ylower,yupper))+ggtitle(expression(paste(rho,"=0.75")))
  
  #Put plots together
  
  mylegend<-g_legend(p1)
  p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                p2 + theme(legend.position="none"),
                                p3 + theme(legend.position="none"),
                                p4 + theme(legend.position="none"),
                                nrow=2),
                    mylegend, nrow=2,heights=c(10, 1))
  return(p)
}

#####################################################################################

#Simulation 1 - x1 deleted/imputed
Create_Combined_Graphic(type="MCAR", sim=1, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
Create_Combined_Graphic(type="MAR", sim=1, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
Create_Combined_Graphic(type="MNAR", sim=1, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)

#Simulation 1 - x3 deleted/imputed
Create_Combined_Graphic(type="MCAR", sim=1, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
Create_Combined_Graphic(type="MAR", sim=1, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
Create_Combined_Graphic(type="MNAR", sim=1, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)

#Simulation 1 - x5 deleted/imputed
Create_Combined_Graphic(type="MCAR", sim=1, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
Create_Combined_Graphic(type="MAR", sim=1, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
Create_Combined_Graphic(type="MNAR", sim=1, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)

#Simulation 2 - x1 deleted/imputed
Create_Combined_Graphic(type="MCAR", sim=2, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
Create_Combined_Graphic(type="MAR", sim=2, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
Create_Combined_Graphic(type="MNAR", sim=2, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)

#Simulation 2 - x3 deleted/imputed
Create_Combined_Graphic(type="MCAR", sim=2, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
Create_Combined_Graphic(type="MAR", sim=2, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
Create_Combined_Graphic(type="MNAR", sim=2, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)

#Simulation 2 - x5 deleted/imputed
Create_Combined_Graphic(type="MCAR", sim=2, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
Create_Combined_Graphic(type="MAR", sim=2, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
Create_Combined_Graphic(type="MNAR", sim=2, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)

############################################################################

#Plot Showing all variables together

load("sim1_rho_75_MCAR.Rdata")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
df <- do.call("rbind", dfList)
#df <- filter(df, Method%in%c("missForest", "CALIBER", "rfmice"))
p <- ggplot(df, aes(x=p, y=VI, colour = factor(var, labels=c("X1", "X2", "X3", "X4", "X5", "X6"))))+theme(text = element_text(size=16))+ facet_wrap(~Method)
dodge <- position_dodge(width=0.05)
p1 <- p +geom_line(aes(group = var)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = dodge, width = 0.05)+ labs(x = "Proportion of X5 Missing", y="Scaled Importance")+ theme(plot.title = element_text(hjust = 0.5))+ylim(c(0,0.4))+ labs(colour='Variable') 
p1


#################################################################################
#Delete/Impute Simulation Using Boston Housing
load("MVVIMP_BH_MCAR.Rdata")
xvarused <- c(1,4,6,7)

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
p1 <- Createplot(dfList, xvar=xvarused[1])+ theme(legend.position = "bottom")+ylim(c(0, .1))+ggtitle("Crime Rate")
p1

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
p2 <- Createplot(dfList, xvar=xvarused[2])+ theme(legend.position = "bottom")+ylim(c(0, .1))+ggtitle("Borders Charles River")
p2

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=3)
p3 <- Createplot(dfList, xvar=xvarused[3])+ theme(legend.position = "bottom")+ylim(c(0, .3))+ggtitle("Avg. Number of Rooms")
p3

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=4)
p4 <- Createplot(dfList, xvar=xvarused[4])+ theme(legend.position = "bottom")+ylim(c(0, .1))+ggtitle("Age")
p4

mylegend<-g_legend(p1)
p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              p4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))
p


#################################################################################
#Delete/Impute Simulation Using STEM
load("MVVIMP_STEM_MCAR.Rdata")
xvarused <- c(1,2,6,14)

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
p1 <- Createplot(dfList, xvar=xvarused[1])+ theme(legend.position = "bottom")+ylim(c(0, .25))+ggtitle("x1")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
p2 <- Createplot(dfList, xvar=xvarused[2])+ theme(legend.position = "bottom")+ylim(c(0, .35))+ggtitle("x2")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=3)
p3 <- Createplot(dfList, xvar=xvarused[3])+ theme(legend.position = "bottom")+ylim(c(0, .35))+ggtitle("x3")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=4)
p4 <- Createplot(dfList, xvar=xvarused[4])+ theme(legend.position = "bottom")+ylim(c(0, .2))+ggtitle("x4")

mylegend<-g_legend(p1)
p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              p4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))
p