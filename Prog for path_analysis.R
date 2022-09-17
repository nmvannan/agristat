library(agricolae)
library(variability)
library(openxlsx)
rm(list=ls())
data <- read.xlsx(file.choose()) 
data$Treat<-as.factor(data$Treat)
data$Repl<-as.factor(data$Repl)
nch<-length(data)
sink(file="output.txt",append=FALSE,split=FALSE)
print("---------- Genotypic Correlation analysis")
a<- geno.corr(data[3:nch],data$Treat,data$Repl)
print(a)

print("---------- Genotypic path analysis")
a<-geno.path(data[,nch],data[3:(nch-1)],data$Treat,data$Repl)
print(a)

print("---------- Phenotypic Correlation analysis")
a<-  pheno.corr(data[3:nch],data$Treat,data$Repl)
print(a)

print("---------- Phenotypic path analysis")
a<-pheno.path(data[,nch],data[3:(nch-1)],data$Treat,data$Repl)
print(a)



x<-data[3:(nch-1)]
y<-data[,nch]
cor.y<-correlation(y,x)$correlation
cor.x<-correlation(x)$correlation

print("----------- Simple correlation analysis")
a<-correlation(data[3:nch])
print(a)
print("----------- Simple correlation based path analysis")
a<-path.analysis(cor.x, cor.y)
print(a)
sink()

