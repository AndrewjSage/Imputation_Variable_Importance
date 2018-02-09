library(gridExtra)
library(tidyverse)

#Get legend at bottom of all plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


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
np=5
nvarsimp <-dim(Obj)[1]  # number of variables deleted and imputed
niter <- dim(Obj)[2]
nvarstot <- dim(Obj[1,1][[1]][[1]])[1]  #number of predictor variables total
ntechs <- 6
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
#here xvar indexing only includes those who were deleted and imputed. (1=x1, 2=x5)
Summarydf <- function(Array, xvar, pind){  #take in array with mean variable importance for each setting, and the xvar, prop of interest
MEANIMP <- apply(Array, c(1,3,4,5), mean)
SDIMP <- apply(Array, c(1,3,4,5), sd)
n <- dim(Array)[2]
VI <- as.vector(MEANIMP[xvar, pind, ,])
SD <- as.vector(SDIMP[xvar, pind, ,])
var <- rep(1:dim(Array)[4], 6)  #list pred. vars 1:dim(Array)[4], repeated for each of 6 techniques
pvec <- c(0, 0.10, 0.25, 0.50, 0.75)
p <- rep(pvec[pind], dim(Array)[4]*6)
Techs <- c("rfImpute", "missForest", "rfsrc-1", "rfsrc-5", "CALIBER", "rfmice")
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

#####################################################################
#Plots for X1

load("MVVIMP_sim0.Rdata")
A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
p1 <- Createplot(dfList, xvar=1)+ theme(legend.position = "bottom")+ylim(c(0,0.5))+ggtitle(expression(paste(rho,"=0")))

load("MVVIMP_sim25.Rdata")
A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
p2 <- Createplot(dfList, xvar=1)+ylim(c(0,0.5))+ggtitle(expression(paste(rho,"=0.25")))

load("MVVIMP_sim50.Rdata")
A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
p3 <- Createplot(dfList, xvar=1)+ylim(c(0,0.5))+ggtitle(expression(paste(rho,"=0.50")))

load("MVVIMP_sim75.Rdata")
A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
p4 <- Createplot(dfList, xvar=1)+ylim(c(0,0.5))+ggtitle(expression(paste(rho,"=0.75")))

#Put plots together

mylegend<-g_legend(p1)
p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              p4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))
p

#####################################################################
#Plots for X5

load("MVVIMP_sim0.Rdata")
A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
p1 <- Createplot(dfList, xvar=5)+ theme(legend.position = "bottom")+ylim(c(0,0.225))+ggtitle(expression(paste(rho,"=0")))

load("MVVIMP_sim25.Rdata")
A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
p2 <- Createplot(dfList, xvar=5)+ylim(c(0,0.225))+ggtitle(expression(paste(rho,"=0.25")))

load("MVVIMP_sim50.Rdata")
A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
p3 <- Createplot(dfList, xvar=5)+ylim(c(0,0.225))+ggtitle(expression(paste(rho,"=0.50")))

load("MVVIMP_sim75.Rdata")
A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
p4 <- Createplot(dfList, xvar=5)+ylim(c(0,0.225))+ggtitle(expression(paste(rho,"=0.75")))

#Put plots together

mylegend<-g_legend(p1)
p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              p4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))
p

#######################################################################################

#Example Plots

load("MVVIMP_sim75.Rdata")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
df <- do.call("rbind", dfList)
#df <- filter(df, Method%in%c("missForest", "CALIBER", "rfmice"))
p <- ggplot(df, aes(x=p, y=VI, colour = factor(var, labels=c("X1", "X2", "X3", "X4", "X5", "X6"))))+theme(text = element_text(size=16))+ facet_wrap(~Method)
dodge <- position_dodge(width=0.05)
p1 <- p +geom_line(aes(group = var)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = dodge, width = 0.05)+ labs(x = "Proportion of X5 Missing", y="Scaled Importance")+ theme(plot.title = element_text(hjust = 0.5))+ylim(c(0,0.35))+ labs(colour='Variable') 
p1

#######################################################################################
#STEM Housing Dataset

load("MVVIMP_STEM.Rdata")
A <- SummaryFunc(RES)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
Sp1 <- Createplot(dfList, xvar=1)+ theme(legend.position = "bottom")+ggtitle("ACT Score")

load("MVVIMP_STEM.Rdata")
A <- SummaryFunc(RES)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
Sp2 <- Createplot(dfList, xvar=6)+ggtitle("RAI")

load("MVVIMP_STEM.Rdata")
A <- SummaryFunc(RES)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=3)
Sp3 <- Createplot(dfList, xvar=10)+ggtitle("Science Units")

load("MVVIMP_STEM.Rdata")
A <- SummaryFunc(RES)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=4)
Sp4 <- Createplot(dfList, xvar=11)+ggtitle("Learning Community")

mylegend<-g_legend(Sp1)
p <- grid.arrange(arrangeGrob(Sp1 + theme(legend.position="none"),
                              Sp2 + theme(legend.position="none"),
                              Sp3 + theme(legend.position="none"),
                              Sp4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))
p


#######################################################################################
#Boston Housing Dataset

load("MVVIMP_BH.Rdata")
A <- SummaryFunc(RES)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
BHp1 <- Createplot(dfList, xvar=1)+ theme(legend.position = "bottom")+ggtitle("Crime Rate")

load("MVVIMP_BH.Rdata")
A <- SummaryFunc(RES)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
BHp2 <- Createplot(dfList, xvar=4)+ggtitle("Charles River")

load("MVVIMP_BH.Rdata")
A <- SummaryFunc(RES)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=3)
BHp3 <- Createplot(dfList, xvar=6)+ggtitle("Number of Rooms")


load("MVVIMP_BH.Rdata")
A <- SummaryFunc(RES)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=4)
BHp4 <- Createplot(dfList, xvar=7)+ggtitle("Age")


mylegend<-g_legend(BHp1)

p <- grid.arrange(arrangeGrob(BHp1 + theme(legend.position="none"),
                              BHp2 + theme(legend.position="none"),
                              BHp3 + theme(legend.position="none"),
                              BHp4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))
p