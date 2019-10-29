library(tidyverse)
library(gridExtra)
library(tidyr)
setwd("~/OneDrive - Lawrence University/Research/Imputation and Variable Importance")

#load("sim1_rho_75_MCAR.Rdata")
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


#Function to create graphic for all variables with a given missingness level
Combined_Graphic_Miss_Types <- function(rho, sim, xvar, xvarused, ylower, yupper){
  #rho is 0, 25, 50, 75
  #sim 1, 2, "BH", or "STEM"
  #xvar is variable deleted and imputed
  #xvarused is the vector of variables deleted and imputed in the simulation should always be c(1,3,5) 
  #ylower and yupper are bounds for y-axis
  
  load(paste("sim",sim,"_rho_",rho,"_","MCAR",".Rdata", sep = ""))
  A <- SummaryFunc(MVVIMP)
  dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=which(xvarused==xvar))
  p1 <- Createplot(dfList, xvar=xvar)+ theme(legend.position = "bottom")+ylim(c(ylower,yupper))+ggtitle("MCAR")

  load(paste("sim",sim,"_rho_",rho,"_","MAR",".Rdata", sep = ""))
  A <- SummaryFunc(MVVIMP)
  dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=which(xvarused==xvar))
  p2 <- Createplot(dfList, xvar=xvar)+ theme(legend.position = "bottom")+ylim(c(ylower,yupper))+ggtitle("MAR")

  load(paste("sim",sim,"_rho_",rho,"_","MNAR",".Rdata", sep = ""))
  A <- SummaryFunc(MVVIMP)
  dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=which(xvarused==xvar))
  p3 <- Createplot(dfList, xvar=xvar)+ theme(legend.position = "bottom")+ylim(c(ylower,yupper))+ggtitle("MNAR")
  
    #Put plots together
  
  mylegend<-g_legend(p1)
  p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                p2 + theme(legend.position="none"),
                                p3 + theme(legend.position="none"),
                                nrow=1),
                    mylegend, nrow=2,heights=c(10, 1))
  return(p)
}


#function to create table giving importance values for a single variable that was deleted and imputed
CreateTable <- function(type="MCAR", sim=1, xvar=1, rho=0, xvarused=c(1,3,5)){
  load(paste("sim",sim,"_rho_",rho,"_",type,".Rdata", sep = ""))
  A <- SummaryFunc(MVVIMP)
  dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=which(xvarused==xvar))
  df <- rbind(dfList[[1]], dfList[[2]], dfList[[3]], dfList[[4]], dfList[[5]])
  df <- df[df$var==xvar, c(1,3,4)]
  data_wide <- spread(df, key=Method, value=VI)
  data_wide
  return(data_wide)
}

#####################################################################################
