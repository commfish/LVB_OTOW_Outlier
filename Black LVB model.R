########LVB Simulated Prediction Interval Error Check##############
###Based on LVB modeling from Ogle, D.H. 2015. FSA: Fisheries Stock Analysis. R package version 0.7.7.###
###and Monte Carlo Simulations from package {predict}

###Read Me####
#Columns used are age, sex, length, otoW, species
#Starting values are based on length in mm
#alpha for prediction intervals is set to 0.01 (99% PIs), model can take values less then 0.001 for wider intervals
#plots are jittered by adding decimal values to age, consider for visual comparisons

#####Help installing packages######

#install.packages("propagate")
#source("http://www.rforge.net/FSATeach/InstallFSATeach.R")

#####Load packages######
library(FSA)
library(nlstools)
library(ggplot2)
library(tcltk2)
library(propagate)
library(tidyverse)
library(readxl)

##### Load data#########

dataBDRF<-read_excel(file.choose()) 
dataBDRF<-read_csv(file=file.choose())


#Rename columns


CN_age<-'AGE'
CN_length<-'SPECIMEN.LENGTH'
CN_sex<-'GENDER_CODE'
CN_species<-'FIELD_SPECIES_CODE'
CN_otoW<-'AGE_STRUCTURE.WEIGHT'
dataBDRF<- dataBDRF %>% rename(age=CN_age,length=CN_length,sex=CN_sex,species=CN_species,otoW=CN_otoW)


if(max(dataBDRF$length,na.rm = TRUE)<200){
  dataBDRF$length<-dataBDRF$length*10 #convert cm to mm length
}

dataBRF<-subset(dataBDRF,dataBDRF$species==142)

#####Plot data#######

ggplot(dataBDRF,aes(age,length,col=as.factor(species)),)+
  geom_point(size=2,position=position_dodge(width = 1))+
  theme_classic()

#####Model######

svTypical<-vbStarts(length~age,data=dataBRF,fixed = list (t0=0),plot=TRUE)
#svTypical <- list(Linf=530,K=0.09,t0=-1)# black rockfish starting values 
#svTypical4 <- list(Linf=515,K=0.2,t0=.4)# alternative black rockfish starting values
vbTypical <- vbFuns() # get RHS of typical function
fitTypical <- nls(length~vbTypical(age,Linf,K,t0),data=dataBRF,start=svTypical)
fitTypicalM <- nls(length~vbTypical(age,Linf,K,t0),data=dataBRF,subset=sex== c("M","1"), start=svTypical)
fitTypicalF <- nls(length~vbTypical(age,Linf,K,t0),data=dataBRF,subset=sex==c("F","2"),start=svTypical)

#Model output for combined and sex specific
overview(fitTypical)
summary(fitTypical)
fitPlot(fitTypical,xlab="Age",ylab="Total Length (mm)",main="")
residPlot(fitTypical)
hist(residuals(fitTypical),main="")

overview(fitTypicalM)
summary(fitTypicalM)
fitPlot(fitTypicalM,xlab="Age",ylab="Total Length (mm)",main="")
residPlot(fitTypicalM)
hist(residuals(fitTypicalM),main="")

overview(fitTypicalF)
summary(fitTypicalF)
fitPlot(fitTypicalF,xlab="Age",ylab="Total Length (mm)",main="")
residPlot(fitTypicalF)
hist(residuals(fitTypicalF),main="")

#########################Monte Carlo CI and PI##################################

alphaLVB=0.01
pred.pred<-predictNLS(fitTypical, newdata=data.frame(age=seq(0,60,by=1)),interval="pred",nsim = 100000,alpha=alphaLVB)
pred.predM<-predictNLS(fitTypicalM, newdata=data.frame(age=seq(0,60,by=1)),interval="pred",nsim = 100000,alpha=alphaLVB)
pred.predF<-predictNLS(fitTypicalF, newdata=data.frame(age=seq(0,60,by=1)),interval="pred",nsim = 100000,alpha=alphaLVB)
pred.predsum<-pred.pred$summary
pred.predsum$age<-seq(0,60,by=1)
pred.predsumM<-pred.predM$summary
pred.predsumM$age<-seq(0,60,by=1)
pred.predsumF<-pred.predF$summary
pred.predsumF$age<-seq(0,60,by=1)


########################Plot estimates##########################################

#Not sex specific
plot(length ~ age, data=dataBRF,ylab= "Total Length (mm)",xlab="Age",pch=c(3),cex=0,ylim=c(10,650))
polygon(c(pred.predsum$age, rev(pred.predsum$age)), c(pred.predsum[, 11],rev(pred.predsum[, 12])), col = rgb(red=0,green=0,blue=0,alpha=0.2),lty = 0)
points(length ~ I(age), data=dataBRF, col= rgb(red=0,blue=0,green=0,alpha=0.2),pch=19,cex=1)# R1 Black
points(length ~ I(age+0.2), data=dataBDRF, subset=species!="142",col=rgb(red=0,green=.8,blue=0,alpha=0.2),pch=19,cex=1)#R1 Dusky
legend("bottomright",c("Black RF","Other"),pch=c(19,19),col=c("black",rgb(red=0,blue=0,green=.8,alpha=0.7)),bty = "n")

#Sex specific
plot(length ~ age, data=dataBRF,ylab= "Total Length (mm)",xlab="Age",pch=c(3),cex=0,ylim=c(10,650))
points(length ~ I(age), data=dataBRF, subset=sex==c("M","1"),col=rgb(red=0,green=0,blue=0.8,alpha=0.2),pch=19,cex=1)
points(length ~ I(age-0.3), data=dataBRF, subset=sex==c("F","2"),col=rgb(red=0.8,green=0,blue=0,alpha=0.2),pch=19,cex=1)
polygon(c(pred.predsumM$age, rev(pred.predsumM$age)), c(pred.predM$summary[, 11],rev(pred.predM$summary[, 12])), col = rgb(red=0,green=0,blue=.6,alpha=0.2),lty = 0)
polygon(c(pred.predsumF$age, rev(pred.predsumF$age)), c(pred.predF$summary[, 11],rev(pred.predF$summary[, 12])), col = rgb(red=.6,green=0,blue=0,alpha=0.2),lty = 0)

####################Export Table################################################
write.table(pred.predsum, "clipboard", sep="\t", row.names=FALSE)
write.table(pred.predsumM, "clipboard", sep="\t", row.names=FALSE)
write.table(pred.predsumF, "clipboard", sep="\t", row.names=FALSE)
###############################################################
###############################################################


