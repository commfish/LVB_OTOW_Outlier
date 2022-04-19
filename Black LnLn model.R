
#########################Backcalc LN:LN ######################
###Based on modeling from Ogle, D.H. 2015. FSA: Fisheries Stock Analysis. R package version 0.7.7.###

###Read Me####
#Columns used are age, sex, length, otoW, species
#Starting values are based on length in mm
#alpha for prediction intervals is set to 0.99 (99% PIs), model can take values greater then 0.99 for wider intervals
#plots are jittered by adding decimal values to age, consider for visual comparisons

#########################Backcalc LN:LN ######################
##Updated 11/18/2021 Kevin McNeel 

#Columns used are age, sex, length, otoW

#####Load packages######
if(!require("FSA"))   install.packages("FSA")
if(!require("ggplot2"))   install.packages("ggplot2")
if(!require("tcltk2"))   install.packages("tcltk2")
if(!require("tidyverse"))   install.packages("tidyverse")
if(!require("readxl"))   install.packages("readxl")


########################Load data#####################

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


###############################Model#######################################
#ln.mod<-lm(log(otoW)~I(log(age)),dataBRF) #linear Ln model
ln.mod<-lm(log(otoW)~log(age+0.00001)+age,dataBRF) #Ln +age model

alphaln<-.99

MaxAge<-max(dataBDRF$age)
a<-data.frame(exp(predict(ln.mod,newdata=data.frame(age=seq(1,MaxAge,by=1)),interval="prediction",level=alphaln)))
a$age<-seq(1,MaxAge,by=1)

####graphing; Ages offset .4-.2 to jitter plots#####
plot(otoW ~ age, data=dataBRF,ylab= "Otolith Weight (g)",xlab="Age",pch=c(3),xlim=c(0,MaxAge),ylim=c(0,max(na.omit(dataBDRF$otoW))*1.1),cex=0)
polygon(c(a$age, rev(a$age)), c(a[,2],rev(a[,3])), col = rgb(red=0,green=0,blue=0,alpha=0.1),lty = 0)
points(otoW ~ I(age+0.2), data=dataBRF,col=rgb(red=0.1,green=0.1,blue=0.1,alpha=0.2),pch=19,cex=1)#R1 Black RF



#points(otoW ~ I(age+0.2), data=dataBRF, subset=sex=="1",col=rgb(red=0,green=0,blue=0.8,alpha=0.2),pch=19,cex=1)#Male BRF
#points(otoW ~ age, data=dataBRF, subset=sex=="2",col=rgb(red=.8,green=0,blue=0,alpha=0.2),pch=19,cex=1)#Female BRF
points(otoW ~ I(age-0.2), data=dataBDRF, subset=species!=Tspecies,col=rgb(red=0,blue=0,green=.6,alpha=0.2),pch=19,cex=1,alpha=0.2)#R1 Dusky
#legend("bottomright",c("Male","Female","Dusky"),pch=c(19,19,19),col=c("cornflowerblue","red2","forestgreen"),bty = "n")
#legend("bottomright",c("Black M","Black F", "Dark"),pch=c(19.19,19),col=c("red2","blue2","forestgreen"),bty = "n")
legend("topleft",c(unique(dataBDRF$area)[run],"Other"),pch=c(19,19),
      col=c("black",rgb(red=0,blue=0,green=.8)),bty = "n")
plot(otoW ~ age, data=dataBRF,subset=age<5,ylim=c(0,30),xlim=c(0,5))
abline(h=2.5)
############################Export Data########################################
write.table(a, "clipboard", sep="\t", row.names=FALSE, col.names = FALSE)
