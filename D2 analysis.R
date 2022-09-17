
########## D2 analysis with R ######################
#####version 10.08.2022  ###########################
########### biotools ########################
########### install biotools, readxl ###############
rm(list=ls())
library(biotools)
library(readxl)
data <- read_excel("D2forR.xlsx", sheet = "data")
View(D2forR)

library(readxl)
mean <- read_excel("D2forR.xlsx", sheet = "mean")
View(D2forR)

########seperating dependant variable ###############
dv <- as.matrix(data[,3:6])
############# MANOVA ################################
mod <-manova(dv~as.factor(Geno)+as.factor(Repl),data=data)
summary(mod)
ss<-SSD(mod)
covar <- ss$SSD/ss$df

###############contribution ########################
imp <-singh(dv,covar)

plot(imp)
############distance ###############################
d<-D2.dist(mean[,-1],covar)
#########tocher####################################
toc <-tocher(d)
#######modified toc ###############################
tocm<-tocher(d,algorithm ="sequential")

#################### print statement ###############
sink(file="output.txt",append=FALSE,split=FALSE)
cat('MANOVA',"\n")
summary(mod)
cat("\n")
cat('Mean of genotypes',"\n")
mean
cat("\n")
cat('Contribution of characters',"\n")
imp
cat("\n")
cat('Clusters',"\n")
toc$clusters
cat("\n")
cat('Cluster Distances',"\n")
toc$distClust
cat("\n")
cat('Modified Clusters',"\n")
tocm
cat("\n")
cat('GOOD LUCK',"\n")
sink()

################# end ##############################

