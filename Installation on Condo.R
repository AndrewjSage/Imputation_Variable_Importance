
# R versions
#r/3.4.3-py2-kaltwmm  #Used this one
#r/3.5.0-py2-hqpehv5
#r/3.5.0-py2-ne5hgqo
#r/3.5.0-py2-qqwf6c6
#r/3.5.0-py2-ufvuwmm
#r/3.5.0-py2-x335hrh
#r/3.6.0-py2-fupx2uq

Steps to install appropriate packages on condo 
module load r/3.4.3-py2-kaltwmm
R

withr::with_makevars(c(PKG_LIBS = "-liconv"), install.packages("readxl"), assignment = "+=")
install.packages("mice")
install.packages("https://cran.r-project.org/src/contrib/Archive/mvtnorm/mvtnorm_1.0-8.tar.gz", repos=NULL)
install.packages("CALIBERrfimpute")