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
if(!require("FSA"))   install.packages("FSA")
if(!require("nlstools"))   install.packages("nlstools")
if(!require("propagate"))   install.packages("propagate")
if(!require("ggplot2"))   install.packages("ggplot2")
if(!require("tcltk2"))   install.packages("tcltk2")
if(!require("tidyverse"))   install.packages("tidyverse")
if(!require("readxl"))   install.packages("readxl")
if(!require("plotly"))   install.packages("plotly")
if(!require("rmarkdown"))   install.packages("rmarkdown")
##### Load data#########


dataBDRF<-read_excel(file.choose()) 
#dataBDRF<-read_csv(file=file.choose())

#Rename columns
#Rename the 'FIELDS' with the columns from your data to change them to the naming convention used in the script

CN_age<-'AGE'
CN_length<-'SPECIMEN.LENGTH'
CN_sex<-'GENDER_CODE'
CN_species<-'FIELD_SPECIES_CODE'
CN_otoW<-'AGE_STRUCTURE.WEIGHT'
dataBDRF<- dataBDRF %>% rename(age=CN_age,length=CN_length,sex=CN_sex,species=CN_species,otoW=CN_otoW)

if(max(dataBDRF$length,na.rm = TRUE)<200){
  dataBDRF$length<-dataBDRF$length*10 #convert cm to mm length
}

Tspecies<-"142" #set target species
dataBDRF$species<-character(dataBDRF$species)
dataBRF<-subset(dataBDRF,dataBDRF$species==Tspecies)
dataBDRF<-subset(dataBDRF,dataBDRF$age >-0.1)

###Assign target species
Tspecies<-"142" #Target species name from species column
dataBDRF$species<-character(dataBDRF$species)

dataBRF<-subset(dataBDRF,dataBDRF$species==Tspecies) #filter data to target species for model
table(dataBRF$age)
#####Plot data#######

ggplot(dataBDRF,aes(otoW,length,label=paste(SAMPLE," ",ID),col=as.factor(METHOD)),)+
  geom_jitter(size=2)+
  theme_classic()
ggplotly()
#####Model######

svTypical<-vbStarts(length~age,data=dataBRF,fixed=list(K=0.2,t0=-1),plot=TRUE,na.omit=TRUE)
#svTypical <- list(Linf=530,K=0.09,t0=-1)# black rockfish starting values 
#svTypical4 <- list(Linf=515,K=0.2,t0=.4)# alternative black rockfish starting values
vbTypical <- vbFuns() # get RHS of typical function
fitTypical <- nls(length~vbTypical(age,Linf,K,t0),data=dataBRF,start=svTypical)
fitTypicalM <- nls(length~vbTypical(age,Linf,K,t0),data=dataBRF,subset=sex== c("M","1"), start=svTypical)
fitTypicalF <- nls(length~vbTypical(age,Linf,K,t0),data=dataBRF,subset=sex==c("F","2"),start=svTypical)

#Model output for combined and sex specific
overview(fitTypical)
summary(fitTypical)
write.table(summary(fitTypical)$coefficients, "clipboard", sep="\t",col.names=FALSE)
fitPlot(fitTypical,xlab="Age",ylab="Total Length (mm)",main="")
residPlot(fitTypical)

overview(fitTypicalM)
summary(fitTypicalM)
fitPlot(fitTypicalM,xlab="Age",ylab="Total Length (mm)",main="")
residPlot(fitTypicalM)

overview(fitTypicalF)
summary(fitTypicalF)
fitPlot(fitTypicalF,xlab="Age",ylab="Total Length (mm)",main="")
residPlot(fitTypicalF)

#########################Monte Carlo CI and PI##################################

alphaLVB=0.01 #set alpha level for prediction interval

MaxAge=max(dataBDRF$age) #set maximum age for prediction range

pred.pred<-predictNLS(fitTypical, newdata=data.frame(age=seq(0,MaxAge,by=1)),interval="pred",nsim = 100000,alpha=alphaLVB)
pred.predsum<-pred.pred$summary
pred.predsum$age<-seq(0,MaxAge,by=1)

pred.predM<-predictNLS(fitTypicalM, newdata=data.frame(age=seq(0,MaxAge,by=1)),interval="pred",nsim = 100000,alpha=alphaLVB)
pred.predF<-predictNLS(fitTypicalF, newdata=data.frame(age=seq(0,MaxAge,by=1)),interval="pred",nsim = 100000,alpha=alphaLVB)
pred.predsumM<-pred.predM$summary
pred.predsumM$age<-seq(0,MaxAge,by=1)
pred.predsumF<-pred.predF$summary
pred.predsumF$age<-seq(0,MaxAge,by=1)


########################Plot estimates##########################################

#Not sex specific
#windows(5,4)
plot(length ~ age, data=dataBDRF,ylab= "Total Length (mm)",xlab="Age",pch=c(3),cex=0,ylim=c(200,max(na.omit(dataBDRF$length))),xlim=c(0,MaxAge))
polygon(c(pred.predsum$age, rev(pred.predsum$age)), c(pred.predsum[, 11],rev(pred.predsum[, 12])), col = "light blue",lty = 0)
points(length ~ I(age+0.5), data=dataBDRF, subset= species!=Tspecies,col="red",pch=19,cex=1)#R1 Dusky
points(length ~ I(age), data=dataBRF, col= "black" ,pch=19,cex=1)# R1 Black
legend("bottomright",c("Black Rockfish","Other"),pch=c(19,19),col=c("black","red"),bty = "n")
#savePlot("clipboard", type="wmf") #saves plot to WMF

#Sex specific
plot(length ~ age, data=dataBRF,ylab= "Total Length (mm)",xlab="Age",pch=c(3),cex=0,ylim=c(200,max(na.omit(dataBDRF$length))))
points(length ~ I(age), data=dataBRF, subset=sex==c("M","1"),col=rgb(red=0,green=0,blue=0.8,alpha=0.2),pch=19,cex=1)
points(length ~ I(age-0.3), data=dataBRF, subset=sex==c("F","2"),col=rgb(red=0.8,green=0,blue=0,alpha=0.2),pch=19,cex=1)
polygon(c(pred.predsumM$age, rev(pred.predsumM$age)), c(pred.predM$summary[, 11],rev(pred.predM$summary[, 12])), col = rgb(red=0,green=0,blue=.6,alpha=0.2),lty = 0)
polygon(c(pred.predsumF$age, rev(pred.predsumF$age)), c(pred.predF$summary[, 11],rev(pred.predF$summary[, 12])), col = rgb(red=.6,green=0,blue=0,alpha=0.2),lty = 0)
points(length ~ I(age+0.2), data=dataBDRF, subset= species!=Tspecies,col=rgb(red=0,green=0,blue=0,alpha=0.2),pch=19,cex=1)#R1 Dusky
####################Export Table################################################
write.table(pred.predsum, "clipboard", sep="\t", row.names=FALSE,col.names = FALSE)
write.table(pred.predsumM, "clipboard", sep="\t", row.names=FALSE)
write.table(pred.predsumF, "clipboard", sep="\t", row.names=FALSE)
###############################################################
###############################################################
