######Put Queried data through estimated bounds#####
#install.packages("cowplot")
library(tidyverse)
library(cowplot)
#plot data

syx <- summary(ln.mod)$sigma
cf <- exp((syx^2)/2)# this is the bias correction factor from Ogle

dataAll1<-dataBDRF

names(pred.predsum)<-c("Prop.Mean.1","Prop.Mean.2","Prop.sd.1","Prop.sd.2",
  "Prop.lw","Prop.hi","Sim.Mean","Sim.sd","Sim.Median","Sim.MAD","Sim.lw","Sim.hi","age")
names(pred.predsumM)<-c("M.Prop.Mean.1","M.Prop.Mean.2","M.Prop.sd.1",
  "M.Prop.sd.2","M.Prop.lw","M.Prop.hi","M.Sim.Mean","M.Sim.sd","M.Sim.Median",
  "M.Sim.MAD","M.Sim.lw","M.Sim.hi","age")
names(pred.predsumF)<-c("F.Prop.Mean.1","F. Prop.Mean.2","F.Prop.sd.1","F.Prop.sd.2",
  "F.Prop.lw","F.Prop.hi","F.Sim.Mean","F.Sim.sd","F.Sim.Median","F.Sim.MAD","F.Sim.lw",
  "F.Sim.hi","age")


dataAll1$lnfit <- ((predict(ln.mod, newdata = dataAll1, interval="prediction",level=alphaln)) %>% exp() * cf)[,"fit"]
dataAll1$lnlow <- ((predict(ln.mod, newdata = dataAll1, interval="prediction",level=alphaln)) %>% exp() * cf)[,"lwr"]
dataAll1$lnhi <- ((predict(ln.mod, newdata = dataAll1, interval="prediction",level=alphaln)) %>% exp() * cf)[,"upr"]
dataAll1<-merge(dataAll1,pred.predsum, by= "age", all.x= TRUE, )
dataAll1<-merge(dataAll1,pred.predsumM, by= "age", all.x= TRUE, )
dataAll1<-merge(dataAll1,pred.predsumF, by= "age", all.x= TRUE, )
dataAll1 <- dataAll1  %>% mutate(outlierLVB = if_else( length >= Sim.lw & length<= Sim.hi, "in", "out"))
dataAll1 <- dataAll1  %>% mutate(outlierLN = if_else( otoW >= lnlow & otoW<= lnhi, "in", "out"))

#####R4 species and sex outlier ID###########
dataAll1 <- dataAll1  %>% mutate(outlierMales = if_else( sex==c("M","1") & length <= M.Sim.hi & length >= M.Sim.lw, "in", "out")) 
dataAll1 <- dataAll1  %>% mutate(outlierFemales = if_else( sex==c("F","2") & length <= F.Sim.hi & length >= F.Sim.lw, "in", "out"))
dataAll1 <- dataAll1  %>% mutate(outlierSpecies = if_else( length >= Sim.lw, "in", "out"))
dataAll1 <- dataAll1  %>% mutate(outlierSpecies = if_else( length >= Sim.lw, "in", "out"))

#Identify data with missing values####
missing<-table(NAAGE=is.na(dataAll1$age), NALENG=is.na(dataAll1$length),NAOTOW=is.na(dataAll1$otoW))
write.table(missing, "clipboard", sep="\t")

LVB<-ggplot(dataAll1) +
  geom_point(data=dataAll1,aes(x=age,y=length,color=paste(outlierLVB,species)),alpha=0.2)+
  geom_line(data=pred.predsum, aes(x=age, y=Sim.lw)) +
  geom_line(data=pred.predsum, aes(x=age, y=Sim.hi)) +
  labs(y="Fork Length (mm)",x="Age",color="Outliers by Species")+
  theme_classic() 

LN<-ggplot(dataAll1) +
  geom_point(aes(x=age, y=otoW, color = paste(outlierLN,species)), size = 3, alpha= 0.2) +
  geom_line(data=a, aes(age, a[,2])) +
  geom_line(data=a, aes(age, a[,3])) +
  labs(y="Otolith Weight (g)",x="Age",color="Outliers by Species")+
  theme_classic() 

plot_grid(LVB, LN, labels = "AUTO",ncol=1,nrow=2)


table(filter(dataAll1,species==Tspecies)$outlierLVB, useNA = "always")
table(filter(dataAll1,species==Tspecies)$outlierLN, useNA = "always")

outliers<- dataAll1 %>% filter(outlierLVB=="out" | outlierLN=="out") 
write.csv(outliers, "outliers.csv", sep=",", row.names=FALSE)
write.csv(dataAll1, "outlier_results.csv", sep=",", row.names=FALSE)

