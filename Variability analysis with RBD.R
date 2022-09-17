# Variability with RBD Multiple traits using for loop
# ver 01.09.2022
library(openxlsx)
rm(list=ls())
mydata <- read.xlsx(file.choose()) 
mydata$Treat<-as.factor(mydata$Treat)
mydata$Repl<-as.factor(mydata$Repl)
nch<-length(mydata)
ch<-names(mydata[,3:nch]) 
tn<-length(unique(mydata$Treat))
rn<-length(unique(mydata$Repl))
attach(mydata)
resfile<-data.frame(dim=c(tn+10,nch-1))
str(resfile)
colnames(resfile[1])<-"Treat"

for (h in 1:length(ch)) 
{
  resfile[1,h+1]<-ch[h]
}
for (t in 1:tn)
{
  resfile[t+1,1]<-t
}
# Headers and row names for result file
resfile[tn+1+1: 9,1]<-c("Treat Ms","Error Ms",
                        "Prob","Mean","CV(%)","PCV(%)",
                        "GCV(%)","h2(%)","GAM")


for (h in 1:length(ch)) 
{
  an1<-as.formula(paste(ch[h],"~Repl","+Treat",sep=""))  
a<-aov(an1)
print(summary(a))
a1<- aggregate(mydata[,2+h], by=list(Treat), mean)
  for (t in 1:tn)
  {
  resfile[t+1,h+1]<-format(a1[t,2])
  }
a2<-unlist(summary(a))

# extract from anova
resfile[tn+1+1,h+1]<-format(a2[8])
resfile[tn+1+2,h+1]<-format(a2[9])
resfile[tn+1+3,h+1]<-format(a2[14])

# calculation of variability parameters
se<-sqrt(a2[9]/rn)
sed<-se*sqrt(2)
gmean<-mean(mydata[,2+h])
cv<-sqrt(a2[9])/gmean*100
ev<-a2[9]
gv<-(a2[8]-ev)/rn
pv<-gv+ev
pcv<-(pv^0.5/gmean)*100
gcv<-((abs(gv))^0.5*sign(gv)/gmean)*100
h2<-(gv/pv)*100
ga<-(gv/pv)*pv^0.5*2.06
gam<-ga/gmean*100

resfile[tn+1+4,h+1]<-format(gmean)
resfile[tn+1+5,h+1]<-format(cv)
resfile[tn+1+6,h+1]<-format(pcv)
resfile[tn+1+7,h+1]<-format(gcv)
resfile[tn+1+8,h+1]<-format(h2)
resfile[tn+1+9,h+1]<-format(gam)
}
# resfile
# str(resfile)
#write.xlsx(resfile,output.xlsx,asTable=T)

sink(file="output.txt",append=FALSE,split=FALSE)
print(resfile,quote=F)
sink()




