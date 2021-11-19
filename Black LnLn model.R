
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

########################Load data#####################

###Region 1 data

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
dataBRF<-subset(dataBDRF,dataBDRF$species==142)

library(readxl)
dataBDRF<-read_excel("data/Black_R1_142_154_172_173_7.2.2021.xlsx", 
                     col_types = c("text", "text", "text", "text", "skip", "date", 
                                   "text", "text", "numeric", "numeric", "text", "skip", 
                                   "text", "skip", "numeric", "numeric", "numeric", "numeric", "text", "numeric"))
names(dataBDRF)<-c("area","year","sample","ID","date","fishery","project","length",
                   "weight","species","sex","otoL","otoH","otoW","age","common","RA")
dataBRF<-subset(dataBDRF,dataBDRF$species==142)

###Region 4 data
library(readr)
dataBDRF<-CF_2017_2021_Corrected <- read_csv("data/CF_2017_2021_Corrected.csv",
                                              col_types = cols(Sample_Date = col_date(format = "%m/%d/%Y")))
names(dataBDRF2)<-c("year","Vessel_Name","area","Processor_Name","Port","Section",       
                    "Gear_Code","Delivery","Interview","date","species","sex","length","Maturity","weight","Sampler",
                    "Sample_Number","Otolith_Id","age","otoW","Weight_Right","Length_Left","Height_Left","Length_Right",  
                    "Height_Right","Cryst_Left","Cryst_Right","check","change")
dataBDRF2$length<-dataBDRF2$length*10 #convert cm to mm
dataBRF2<-subset(dataBDRF2,dataBDRF2$species==142)

###############################Model#######################################
#ln.mod<-lm(log(otoW)~I(log(age)),dataBRF) #linear Ln model
ln.mod<-lm(log(otoW)~I(log(age))+age,dataBRF) #Ln +age model

alphaln<-.99

a<-data.frame(exp(predict(ln.mod,newdata=data.frame(age=seq(1,60,by=1)),interval="prediction",level=alphaln)))
a$age<-seq(1,60,by=1)

####graphing; Ages offset .4-.2 to jitter plots#####
plot(otoW ~ age, data=dataBRF,ylab= "Otolith Weight (g)",xlab="Age",pch=c(3),xlim=c(0,60),ylim=c(.05,.8),cex=0)
polygon(c(a$age, rev(a$age)), c(a[,2],rev(a[,3])), col = rgb(red=0,green=0,blue=0,alpha=0.1),lty = 0)
points(otoW ~ I(age+0.2), data=dataBRF,col=rgb(red=0.1,green=0.1,blue=0.1,alpha=0.2),pch=19,cex=1)#R1 Black RF
#points(otoW ~ I(age+0.2), data=dataBRF, subset=sex=="1",col=rgb(red=0,green=0,blue=0.8,alpha=0.2),pch=19,cex=1)#Male BRF
#points(otoW ~ age, data=dataBRF, subset=sex=="2",col=rgb(red=.8,green=0,blue=0,alpha=0.2),pch=19,cex=1)#Female BRF
points(otoW ~ I(age-0.2), data=dataBDRF, subset=species !="142",col=rgb(red=0,blue=0,green=.6,alpha=0.2),pch=19,cex=1,alpha=0.2)#R1 Dusky
#legend("bottomright",c("Male","Female","Dusky"),pch=c(19,19,19),col=c("cornflowerblue","red2","forestgreen"),bty = "n")
#legend("bottomright",c("Black M","Black F", "Dark"),pch=c(19.19,19),col=c("red2","blue2","forestgreen"),bty = "n")
legend("topleft",c("Black RF","Other"),pch=c(19,19),
      col=c("black",rgb(red=0,blue=0,green=.8)),bty = "n")

############################Export Data########################################
write.table(y2, "clipboard", sep="\t", row.names=FALSE)
