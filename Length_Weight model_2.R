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

setwd("C:/Users/abst_saviour/Documents/Rockfish/Stock Assessment/LVB_OTOW_Outlier-main")

# Create object of file path to "data" folder
dir.data <- file.path(getwd(), "data")

##### Load data#########

#harv.dat <- read.csv(file.path(dir.data, "Alaska-Salmon-Harvests.csv") #delete later # use if xlsx does not work

dataYERF<-read.csv("data/YERF_age_length_93-21_CI_NG_PWSO_simple.csv")
dataYERF<-read_csv(file=file.choose())
dataYERF<-read_excel(file.choose())

str(dataYERF)

#Rename columns
#Rename the 'FIELDS' with the columns from your data to change them to the naming convention used in the script

CN_age<-'Age'
CN_length<-'Fork_Length_cm'
CN_weight<-'Whole_Weight_kg'
CN_sex<-'Sex'
CN_species<-'Species'
#CN_otoW<-'OTOW'
dataYERF<- dataYERF %>% rename(length=all_of(CN_length),weight=all_of(CN_weight),
                               sex=all_of(CN_sex), age=all_of(CN_age))


if(max(dataYERF$length,na.rm = TRUE)<200){
  dataYERF$length<-dataYERF$length*10 #convert cm to mm length
}

###Assign target species
Tspecies<-"145" #set target species.
dataYERF$species<-rep("145",length(dataYERF$SampleID))
dataYERF$species<-as.character(dataYERF$species)
dataYERF<-subset(dataYERF,dataYERF$age >-0.1)
dataYRF<-subset(dataYERF,dataYERF$species==Tspecies)#filter data to target species for model

###############################Model#######################################
#ln.mod<-lm(log(otoW)~I(log(age)),dataBRF) #linear Ln model
ln.mod<-lm(log(weight) ~ log(length), data = dataYRF,subset = weight!=0) #Sex generic
Fln.mod<-lm(log(weight) ~ log(length), data = dataYRF,subset = weight!=0 & sex==c("F","2")) #Female
Mln.mod<-lm(log(weight) ~ log(length), data = dataYRF,subset = weight!=0 & sex==c("M","1")) #male

a <- exp(coef(ln.mod)[1]) %>% as.vector()
b <- coef(ln.mod)[2] %>% as.vector()
pred <- (a * (seq(from=1, to=max(dataYRF$length, na.rm=TRUE)) ^ b))
syx <- summary(ln.mod)$sigma
cf <- exp((syx^2)/2)# this is the bias correction factor from Ogle

aF <- exp(coef(Fln.mod)[1]) %>% as.vector()
bF <- coef(Fln.mod)[2] %>% as.vector()
predF <- (aF * (seq(from=1, to=max(dataYRF$length, na.rm=TRUE)) ^ bF))
Fsyx <- summary(Fln.mod)$sigma
Fcf <- exp((Fsyx^2)/2)# this is the bias correction factor from Ogle

aM <- exp(coef(Mln.mod)[1]) %>% as.vector()
bM <- coef(Mln.mod)[2] %>% as.vector()
predM <- (aM * (seq(from=1, to=max(dataYRF$length, na.rm=TRUE)) ^ bM))
Msyx <- summary(Mln.mod)$sigma
Mcf <- exp((Msyx^2)/2)# this is the bias correction factor from Ogle

dataAll1<-dataYRF

alphaln<-.999

#### Outlier plot for fish length vs fish weight ####

dataAll1$lenwtfit <- ((predict(ln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * cf)[,"fit"]
dataAll1$lenwtlow <- ((predict(ln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * cf)[,"lwr"]
dataAll1$lenwt_hi <- ((predict(ln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * cf)[,"upr"]

dataAll1$Flenwtfit <- ((predict(Fln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * Fcf)[,"fit"]
dataAll1$Flenwtlow <- ((predict(Fln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * Fcf)[,"lwr"]
dataAll1$Flenwt_hi <- ((predict(Fln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * Fcf)[,"upr"]

dataAll1$Mlenwtfit <- ((predict(Mln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * Mcf)[,"fit"]
dataAll1$Mlenwtlow <- ((predict(Mln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * Mcf)[,"lwr"]
dataAll1$Mlenwt_hi <- ((predict(Mln.mod, data.frame(length = dataYRF$length), interval="prediction",level=alphaln)) %>% exp() * Mcf)[,"upr"]
dataAll1$sex<-as.character(dataAll1$sex)
dataAll1 <- dataAll1  %>%   mutate(outlierLW = if_else(weight >= lenwtlow & weight <= lenwt_hi, "in", "out"))
dataAll1 <- dataAll1  %>% mutate(outlierSexLW = case_when(
  sex %in% c("M","1") & weight <= Mlenwt_hi & weight >= Mlenwtlow ~ "in",
  sex %in% c("F","2") & weight <= Flenwt_hi & weight >= Flenwtlow ~ "in",
  sex %in% c(NA,"9") & weight <= lenwt_hi & weight >= lenwtlow ~ "in",
  is.na(weight) | is.na(length) ~as.character(NA),
  TRUE ~ "out"))
    
dataset4 <- data.frame(length=seq(from=1, to=max(dataYRF$length, na.rm=TRUE)))
dataset4$lenwtfit <- ((predict(ln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * cf)[,"fit"]
dataset4$lenwtlow <- ((predict(ln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * cf)[,"lwr"]
dataset4$lenwt_hi <- ((predict(ln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * cf)[,"upr"]
dataset4$Flenwtfit <- ((predict(Fln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * Fcf)[,"fit"]
dataset4$Flenwtlow <- ((predict(Fln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * Fcf)[,"lwr"]
dataset4$Flenwt_hi <- ((predict(Fln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * Fcf)[,"upr"]
dataset4$Mlenwtfit <- ((predict(Mln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * Mcf)[,"fit"]
dataset4$Mlenwtlow <- ((predict(Mln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * Mcf)[,"lwr"]
dataset4$Mlenwt_hi <- ((predict(Mln.mod, data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE))), interval="prediction",level=alphaln)) %>% exp() * Mcf)[,"upr"]

ggplot(dataset4) +
      geom_ribbon(data=dataset4,aes(x=length,ymin=lenwtlow, ymax=lenwt_hi), fill = "grey70") +
      geom_point(data=dataAll1, aes(x = length, y = weight,color=outlierLW), size = 3) +
      geom_line(data = data.frame(length = seq(from=1, to=max(dataYRF$length, na.rm=TRUE)), weight = pred), 
                aes(x = length, y = weight)) +
      scale_color_manual(values = c("black", "firebrick1")) +
      theme_bw() +
      
      theme(legend.position=c(0.15, 0.85), 
            legend.title = element_blank(),
            legend.key.size = unit(2,"line"))

ggplot(dataAll1) +
  geom_point(data=dataAll1, aes(x = length, y = weight,color=interaction(outlierSexLW,sex,sep=':')), size = 3) +
  geom_line(data=dataset4,aes(x=length,y=Flenwtlow), color = "red") +
  geom_line(data=dataset4,aes(x=length,y=Flenwt_hi), color = "red") +
  geom_line(data=dataset4,aes(x=length,y=Mlenwtlow), color = "blue") +
  geom_line(data=dataset4,aes(x=length,y=Mlenwt_hi), color = "blue") +
  facet_wrap(~sex)+
  theme_bw() +theme(legend.title = element_blank())


#summarize values

table(filter(dataAll1,species==Tspecies)$outlierLW, useNA = "always")
table(filter(dataAll1,species==Tspecies)$outlierSexLW, useNA = "always")

###Data export####

outliers_LW<- dataAll1 %>% filter(outlierLW=="out") #make table of just outliers
clean_data_LW<-dataAll1 %>% filter(outlierLW!="out") #make table of no outliers using generic model
#clean_data<-dataAll1 %>% filter(outlierSexLW!="out") #make table of no outliers using sex model
write.csv(outliers_LW, "outliers_LW_only.csv", row.names=FALSE) #make csv of just outliers
write.csv(clean_data_LW, "outlier_LW_results_all.csv", row.names=FALSE) #make csv of all data with outlier results
