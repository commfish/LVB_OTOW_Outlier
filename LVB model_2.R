########LVB Simulated Prediction Interval Error Check##############
#updated 10/11/2022
###Based on LVB modeling from Ogle, D.H. 2015. FSA: Fisheries Stock Analysis. R package version 0.7.7.###
###and Monte Carlo Simulations from package {predict}

###Script goes through and produces a clean dataframe based on LVB output#

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
if(!require("patchwork"))   install.packages("patchwork")


# Set working directory
setwd("C:/Users/abst_saviour/Documents/Rockfish/Stock Assessment/LVB_OTOW_Outlier-main")

# Create object of file path to "data" folder
dir.data <- file.path(getwd(), "data")

##### Load data#########

#harv.dat <- read.csv(file.path(dir.data, "Alaska-Salmon-Harvests.csv") #delete later # use if xlsx does not work

dataYERF<-read.csv("data/YERF_age_length_93-21_CI_NG_PWSO_simple.csv")
#dataYERF<-read_csv(file=file.choose())

str(dataYERF)

#Rename columns
#Rename the 'FIELDS' with the columns from your data to change them to the naming convention used in the script

CN_age<-'AGE'
CN_length<-'LENGTH'
CN_sex<-'SEX'
CN_species<-'SPECIES'
#CN_otoW<-'OTOW'
dataYERF<- dataYERF %>% rename(age=all_of(CN_age),length=all_of(CN_length),
                               sex=all_of(CN_sex),species=all_of(CN_species)) #,otoW=all_of(CN_otoW))


if(max(dataYERF$length,na.rm = TRUE)<200){
  dataYERF$length<-dataYERF$length*10 #convert cm to mm length
}

###Assign target species
Tspecies<-"145" #set target species.
dataYERF$species<-as.character(dataYERF$species)
dataYRF<-subset(dataYERF,dataYERF$species==Tspecies)#filter data to target species for model
dataYERF<-subset(dataYERF,dataYERF$age >-0.1)

#####Plot data#######

#ggplot(dataYERF,aes(age,length,label=paste(SAMPLE," ",ID),col=as.factor(METHOD)),)+
#  geom_jitter(size=2)+
#  theme_classic()
#ggplotly() ### Use to highlight unique IDs to find specifics. If your data has a unique ID for each fish, use that instead of sample or id

#simple data plot
ggplot(dataYERF,aes(age,length))+
  geom_jitter(size=1)+
  theme_classic()

#####Model######

#estimate starting values
svTypical<-vbStarts(length~age,data=dataYRF,plot=TRUE,na.omit=TRUE) #calculate starting values without fixing values
#svTypical<-vbStarts(length~age,data=dataYRF,fixed=list(K=0.05,t0=-5),plot=TRUE,na.omit=TRUE) ### fixing K and t0.
#svTypical <- list(Linf=530,K=0.09,t0=-1)# black rockfish starting values 
#svTypical4 <- list(Linf=515,K=0.2,t0=.4)# alternative black rockfish starting values

vbTypical <- vbFuns() # get RHS of typical function
fitTypical <- nls(length~vbTypical(age,Linf,K,t0),data=dataYRF,start=svTypical)
fitTypicalM <- nls(length~vbTypical(age,Linf,K,t0),data=dataYRF,subset=sex== c("M","1"), start=svTypical)
fitTypicalF <- nls(length~vbTypical(age,Linf,K,t0),data=dataYRF,subset=sex==c("F","2"),start=svTypical)

#Model output for combined and sex specific

#sex combined
overview(fitTypical)
summary(fitTypical)
write.table(summary(fitTypical)$coefficients, "clipboard", sep="\t",col.names=FALSE) #put summary coefficients in clipboard to paste

#fit plot
nlregdf <- dataYRF
nlregdf <- cbind(dataYRF,fit=predict(fitTypical,newdata=dataYRF))
ggplot(nlregdf,aes(x=age)) +
  geom_line(aes(y=fit),size=1) +
  geom_point(aes(y=length))+
  theme_classic()

#residual plot
tmp <- dplyr::select(dataYRF,length,age) %>%
  dplyr::mutate(fits=fitted(fitTypical),
                resids=resid(fitTypical),
                sresids=nlstools::nlsResiduals(fitTypical)$resi2[,"Standardized residuals"])

fit1<- ggplot(data=tmp,mapping=aes(x=resids)) +
  geom_histogram(color="gray30",fill="gray70",binwidth=0.5) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05)))+
  theme_classic()
fit2<-ggplot(data=tmp,mapping=aes(x=fits,y=resids)) +
  geom_point()+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()
fit1+fit2

##Separate Sex#

#Male model
overview(fitTypicalM)
summary(fitTypicalM)

#Female model
overview(fitTypicalF)
summary(fitTypicalF)

#fit plot
nlregdf <- dataYRF
nlregdf <- cbind(dataYRF,fitm=predict(fitTypicalM,newdata=dataYRF),fitf=predict(fitTypicalF,newdata=dataYRF))
ggplot(nlregdf,aes(x=age)) +
  geom_line(aes(y=fitf),size=1,col="red") +
  geom_line(aes(y=fitm),size=1,col="blue")+
  geom_point(aes(y=length,col=sex))+
  theme_classic()


#########################Monte Carlo CI and PI##################################

alphaLVB=0.01 #set alpha level for prediction interval

MaxAge=max(dataYERF$age) #set maximum age for prediction range

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
plot(length ~ age, data=dataYERF,ylab= "Total Length (mm)",xlab="Age",pch=c(3),cex=0,ylim=c(200,max(na.omit(dataYERF$length))),xlim=c(0,MaxAge))
polygon(c(pred.predsum$age, rev(pred.predsum$age)), c(pred.predsum[, 11],rev(pred.predsum[, 12])), col = "light blue",lty = 0)
# points(length ~ I(age+0.5), data=dataYERF, subset= species!=Tspecies,col="red",pch=19,cex=1)#R1 Dusky
points(length ~ I(age), data=dataYRF, col= "black" ,pch=19,cex=0.6)
#legend("bottomright",c("black Rockfish","Other"),pch=c(19,19),col=c("black","red"),bty = "n")
legend("bottomright",c("Yelloweye Rockfish"),pch=c(19, cex=0.6),col=c("black"),bty = "n")
#savePlot("clipboard", type="wmf") #saves plot to WMF

#Sex specific
plot(length ~ age, data=dataYRF,ylab= "Total Length (mm)",xlab="Age",pch=c(3),cex=0,ylim=c(200,max(na.omit(dataYERF$length))))
points(length ~ I(age), data=dataYRF, subset=sex==c("M","1"),col=rgb(red=0,green=0,blue=0.8,alpha=0.2),pch=19,cex=1)
points(length ~ I(age-0.3), data=dataYRF, subset=sex==c("F","2"),col=rgb(red=0.8,green=0,blue=0,alpha=0.2),pch=19,cex=1)
polygon(c(pred.predsumM$age, rev(pred.predsumM$age)), c(pred.predM$summary[, 11],rev(pred.predM$summary[, 12])), col = rgb(red=0,green=0,blue=.6,alpha=0.2),lty = 0)
polygon(c(pred.predsumF$age, rev(pred.predsumF$age)), c(pred.predF$summary[, 11],rev(pred.predF$summary[, 12])), col = rgb(red=.6,green=0,blue=0,alpha=0.2),lty = 0)
#points(length ~ I(age+0.2), data=dataYERF, subset= species!=Tspecies,col=rgb(red=0,green=0,blue=0,alpha=0.2),pch=19,cex=1)#R1 Dusky
####################Export Table################################################
write.table(pred.predsum, "clipboard", sep="\t", row.names=FALSE,col.names = FALSE)
write.table(pred.predsumM, "clipboard", sep="\t", row.names=FALSE)
write.table(pred.predsumF, "clipboard", sep="\t", row.names=FALSE)

####################Export Outliers###########################################


dataAll1<-dataYRF

names(pred.predsum)<-c("Prop.Mean.1","Prop.Mean.2","Prop.sd.1","Prop.sd.2",
                       "Prop.lw","Prop.hi","Sim.Mean","Sim.sd","Sim.Median","Sim.MAD","Sim.lw","Sim.hi","age")
names(pred.predsumM)<-c("M.Prop.Mean.1","M.Prop.Mean.2","M.Prop.sd.1",
                        "M.Prop.sd.2","M.Prop.lw","M.Prop.hi","M.Sim.Mean","M.Sim.sd","M.Sim.Median",
                        "M.Sim.MAD","M.Sim.lw","M.Sim.hi","age")
names(pred.predsumF)<-c("F.Prop.Mean.1","F. Prop.Mean.2","F.Prop.sd.1","F.Prop.sd.2",
                        "F.Prop.lw","F.Prop.hi","F.Sim.Mean","F.Sim.sd","F.Sim.Median","F.Sim.MAD","F.Sim.lw",
                        "F.Sim.hi","age")

dataAll1<-merge(dataAll1,pred.predsum, by= "age", all.x= TRUE, )
dataAll1<-merge(dataAll1,pred.predsumM, by= "age", all.x= TRUE, )
dataAll1<-merge(dataAll1,pred.predsumF, by= "age", all.x= TRUE, )
dataAll1 <- dataAll1  %>% mutate(outlierLVB = if_else( length >= Sim.lw & length<= Sim.hi, "in", "out"))

#####sex outlier ID###########
dataAll1 <- dataAll1  %>% mutate(outlierSex = case_when(
  sex=="M" & length <= M.Sim.hi & length >= M.Sim.lw ~ "in",
  sex=="F" & length <= F.Sim.hi & length >= F.Sim.lw ~ "in",
  TRUE ~ "out"))

#Identify data with missing values####
missing<-cbind(NAAGE=is.na(dataAll1$age), NALENG=is.na(dataAll1$length))
write.table(missing, "clipboard", sep="\t")

#generic plot
(LVB<-ggplot(dataAll1) +
    geom_point(aes(x=age,y=length,color=paste(dataAll1$sex,dataAll1$outlierLVB)),alpha=.5,position = "jitter")+
    geom_line(data=pred.predsum, aes(x=age, y=Sim.lw)) +
    geom_line(data=pred.predsum, aes(x=age, y=Sim.hi))+
    #geom_line(data=pred.predsum, aes(x=age, y=Sim.hi)) +
    labs(y="Fork Length (mm)",x="Age",color="Outlier limits")+
    ylim(min(na.omit(dataAll1$length)),max(na.omit(dataAll1$length)))+
    scale_color_brewer(palette="Set1")+
    theme(legend.position = "bottom")+
    theme_classic())

#sex specific plot
(LVB<-ggplot() +
   geom_point(subset(dataAll1, sex=="M"), mapping=aes(x=age,y=length,color=paste(subset(dataAll1, sex=="M")$outlierSex,subset(dataAll1, sex=="M")$sex)),alpha=.5,position = "jitter")+
   geom_point(subset(dataAll1, sex=="F"), mapping=aes(x=age,y=length,color=paste(subset(dataAll1, sex=="F")$outlierSex,subset(dataAll1, sex=="F")$sex)),alpha=.5,position = "jitter")+
    geom_line(data=pred.predsumF, aes(x=age, y=F.Sim.lw),color="red") +
    geom_line(data=pred.predsumF, aes(x=age, y=F.Sim.hi),color="red") +
    geom_line(data=pred.predsumM, aes(x=age, y=M.Sim.lw),color="blue") +
    geom_line(data=pred.predsumM, aes(x=age, y=M.Sim.hi),color="blue") +
   labs(y="Fork Length (mm)",x="Age",color="Outliers by Species")+
   ylim(min(na.omit(dataAll1$length)),max(na.omit(dataAll1$length)))+
  scale_color_brewer(palette="Set1")+
  theme(legend.position = "bottom")+
  theme_classic())+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

#summarize values

table(filter(dataAll1,species==Tspecies)$outlierLVB, useNA = "always")
table(filter(dataAll1,species==Tspecies)$outlierSex, useNA = "always")

###Data export####

outliers<- dataAll1 %>% filter(outlierLVB=="out") #make table of just outliers
clean_data<-dataAll1 %>% filter(outlierLVB!="out") #make table of no outliers using generic model
#clean_data<-dataAll1 %>% filter(outlierSex!="out") #make table of no outliers using sex model
write.csv(outliers, "outliers_only.csv", row.names=FALSE) #make csv of just outliers
write.csv(clean_data, "outlier_results_all.csv", row.names=FALSE) #make csv of all data with outlier results

###############################################################