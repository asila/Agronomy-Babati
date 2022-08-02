#*****************************************************************#
# Job Kihara - CIAT             #
# Project: Yield GAP Ethiopia                      #
# Program Name: Yield Analysis.R                                  #
# Programmed By: Job Kihara                                      #
# Date: 2019                                                 #
#                                                                 #
#*****************************************************************#
library(doBy)
library(lme4)
library(Hmisc)
library(lme4)
#library(hwriter)
library(lsmeans)
library(RODBC)
library(pbkrtest)
library(xlsx)
yield<-read.xlsx("~/Alliance/Job/Data/Bounday_Analysis/WKPResp_Maize.xls",sheetName = "Sheet1",header=T)
#yield<-subset(yield,yield$Abs_Control>100)#Drop Tamasa
yield<-subset(yield, as.numeric(yield$AbControl) > 3)#Drop Tamasa; modified from above to pick the correct variable name.
yield<-subset(yield,yield$Crop_Type=="Maize")#Drop Tamasa
#######CONTROL VS P Treatment YIELD (FIGURE 2a)
plot(yield$Abs_Control, yield$NPK_RR, ylim=c(0,40),xlim=c(0,5000),xlab="Grain yield (t\\ha) of the control \n treatment with no N and P added",ylab="Response ratio",main="a")
cont<-unique(na.omit(yield$Abs_Class))
av.all<-c()
for ( i in 1:length (cont)){
    data.i<-subset(yield, yield$Abs_Class==cont[i])    
    data.o<-data.i[order(-data.i$NPK_RR),] 
    cpa<-as.data.frame(cbind(data.o$NPK_RR[1:3],data.o$Abs_Class[1:3])) 
    colnames(cpa)<-c("NPK_RR","Abs_Class") 
    y<-sapply(na.omit(cpa$NPK_RR),mean) 
    z<-mean(y)
    av<-c(cont[i],z)
    av.all<-rbind(av.all,av)
}
colnames(av.all)<-c("ContClass","PAE")
av.all<-as.data.frame(av.all)
av.all[order(av.all$ContClass),]
data3<-av.all[order(av.all$ContClass),]
require(drc)#for drm outputs, c=lower limit, d=upper limit, e=slope steepness, b=ed50 value i.e. value of x when y is halfway. 
curve<-drm(av.all$PAE~av.all$ContClass,fct=LL.3(),data=av.all)
axis(2,ylim=c(0,40),seq(0,40,10));axis(1,seq(0,5000,1000))
plot(curve,add=T,axes=FALSE)
coef(curve)
