setwd("~/Documents/Github Repos/RF Imputation and Variable Importance")
source("Process_Results.R")
library(gridExtra)
library(tidyverse)

####################################################################################
#Create plots with one variable deleted and imputed under 4 different rho's 

setwd("~/OneDrive - Lawrence University/Research/Imputation and Variable Importance/Sims/ajsage")

#Simulation 1 - x1 deleted/imputed
p <- Create_Combined_Graphic(type="MCAR", sim=1, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
ggsave(filename="Sim1_x1_MCAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MAR", sim=1, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
ggsave(filename="Sim1_x1_MAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MNAR", sim=1, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
ggsave(filename="Sim1_x1_MNAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

#Simulation 1 - x3 deleted/imputed
p <- Create_Combined_Graphic(type="MCAR", sim=1, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
ggsave(filename="Sim1_x3_MCAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MAR", sim=1, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
ggsave(filename="Sim1_x3_MAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MNAR", sim=1, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
ggsave(filename="Sim1_x3_MNAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

#Simulation 1 - x5 deleted/imputed
p <- Create_Combined_Graphic(type="MCAR", sim=1, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
ggsave(filename="Sim1_x5_MCAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MAR", sim=1, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
ggsave(filename="Sim1_x5_MAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MNAR", sim=1, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
ggsave(filename="Sim1_x5_MNAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

#Simulation 2 - x1 deleted/imputed
p <- Create_Combined_Graphic(type="MCAR", sim=2, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
ggsave(filename="Sim2_x1_MCAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MAR", sim=2, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
ggsave(filename="Sim2_x1_MAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MNAR", sim=2, xvar=1, xvarused=c(1,3,5), ylower=0, yupper=0.5)
ggsave(filename="Sim2_x1_MNAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

#Simulation 2 - x3 deleted/imputed
p <- Create_Combined_Graphic(type="MCAR", sim=2, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
ggsave(filename="Sim2_x3_MCAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MAR", sim=2, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
ggsave(filename="Sim2_x3_MAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MNAR", sim=2, xvar=3, xvarused=c(1,3,5), ylower=0, yupper=0.3)
ggsave(filename="Sim2_x3_MNAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

#Simulation 2 - x5 deleted/imputed
p <- Create_Combined_Graphic(type="MCAR", sim=2, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
ggsave(filename="Sim2_x5_MCAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MAR", sim=2, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
ggsave(filename="Sim2_x5_MAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

p <- Create_Combined_Graphic(type="MNAR", sim=2, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
ggsave(filename="Sim2_x5_MNAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

###########################################################################
#Results for different types of missingness

Combined_Graphic_Miss_Types(rho=25, sim=1, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
ggsave(filename="Sim1_x5_rho25.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)


Combined_Graphic_Miss_Types(rho=75, sim=2, xvar=5, xvarused=c(1,3,5), ylower=0, yupper=0.25)
ggsave(filename="Sim2_x5_rho75.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)



############################################################################
#Plot Showing all variables together from a single simulation, fixed rho

#setwd("~/OneDrive - Lawrence University/Research/Imputation and Variable Importance")

load("sim1_rho_75_MCAR.Rdata")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=3) #delete and impute x5(3rd variable deleted)
df <- do.call("rbind", dfList)
#df <- filter(df, Method%in%c("missForest", "CALIBER", "rfmice"))
p <- ggplot(df, aes(x=p, y=VI, colour = factor(var, labels=c("X1", "X2", "X3", "X4", "X5", "X6"))))+theme(text = element_text(size=16))+ facet_wrap(~Method)
dodge <- position_dodge(width=0.05)
p1 <- p +geom_line(aes(group = var)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = dodge, width = 0.05)+ labs(x = "Proportion of X5 Missing", y="Scaled Importance")+ theme(plot.title = element_text(hjust = 0.5))+ylim(c(0,0.4))+ labs(colour='Variable') 
p1

ggsave(filename="AllVars_Sim1_x5_rho75_MCAR.eps", plot = last_plot(), device = "eps", path = NULL,
       scale = 1, width = 8, height = 6, units = "in",
       dpi = 300, limitsize = TRUE)

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
p3 <- Createplot(dfList, xvar=xvarused[3])+ theme(legend.position = "bottom")+ylim(c(0, .4))+ggtitle("Avg. Number of Rooms")
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
ggsave(filename="BH_Sim_MCAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)


#################################################################################
#Delete/Impute Simulation Using STEM
load("MVVIMP_STEM_MCAR.Rdata")
xvarused <- c(1,2,6,14)
#1 - ACT composite
#2 - High School GPA
#6 - ALEKS
#14 - Mapworks Peer Connections


A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=1)
p1 <- Createplot(dfList, xvar=xvarused[1])+ theme(legend.position = "bottom")+ylim(c(0, .25))+ggtitle("ACT Score")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=2)
p2 <- Createplot(dfList, xvar=xvarused[2])+ theme(legend.position = "bottom")+ylim(c(0, .35))+ggtitle("HS GPA")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=3)
p3 <- Createplot(dfList, xvar=xvarused[3])+ theme(legend.position = "bottom")+ylim(c(0, .35))+ggtitle("ALEKS")

A <- SummaryFunc(MVVIMP)
dfList <- lapply(X=1:5, FUN=Summarydf, Array=A, xvar=4)
p4 <- Createplot(dfList, xvar=xvarused[4])+ theme(legend.position = "bottom")+ylim(c(0, .2))+ggtitle("MW Connection")

mylegend<-g_legend(p1)
p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              p4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))
p
ggsave(filename="STEM_Sim_MCAR.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 7, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)

############################################################################################

CreateTable(type="MCAR", sim=1, xvar=1, rho=0, xvarused=c(1,3,5))
CreateTable(type="MCAR", sim=1, xvar=1, rho=25, xvarused=c(1,3,5))
CreateTable(type="MCAR", sim=1, xvar=1, rho=50, xvarused=c(1,3,5))
CreateTable(type="MCAR", sim=1, xvar=1, rho=75, xvarused=c(1,3,5))

CreateTable(type="MCAR", sim=1, xvar=3, rho=0, xvarused=c(1,3,5))
CreateTable(type="MCAR", sim=1, xvar=3, rho=25, xvarused=c(1,3,5))
CreateTable(type="MCAR", sim=1, xvar=3, rho=50, xvarused=c(1,3,5))
CreateTable(type="MCAR", sim=1, xvar=3, rho=75, xvarused=c(1,3,5))

round(CreateTable(type="MNAR", sim=1, xvar=5, rho=0, xvarused=c(1,3,5)),3)
round(CreateTable(type="MCAR", sim=1, xvar=5, rho=25, xvarused=c(1,3,5)),3)
round(CreateTable(type="MCAR", sim=1, xvar=5, rho=50, xvarused=c(1,3,5)),3)
round(CreateTable(type="MNAR", sim=1, xvar=5, rho=75, xvarused=c(1,3,5)),3)

