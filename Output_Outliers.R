######Put Queried data through estimated bounds#####

if(!require("tidyverse"))   install.packages("tidyverse")
if(!require("cowplot"))   install.packages("cowplot")
if(!require("remotes"))   install.packages("remotes")
if(!require("adfgcolors"))   remotes::install_github("justinpriest/adfgcolors")
if(!require("RColorBrewer"))  install.packages("RColorBrewer")

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


dataBDRF4$Species_Code<-factor(dataBDRF4$Species_Code)
dataBDRF4<-mutate(dataBDRF4,factor(Species_Code, levels=c("173","142")))

(LVB<-ggplot(dataAll1) +
  geom_point(aes(x=age,y=length,color=species),alpha=.5,position = "jitter")+
  geom_line(data=pred.predsum, aes(x=age, y=Sim.lw)) +
  geom_line(data=pred.predsum, aes(x=age, y=Sim.hi)) +
  labs(y="Fork Length (mm)",x="Age",color="Outliers by Species")+
  ylim(min(na.omit(dataBDRF$length)),max(na.omit(dataBDRF$length)))+
  scale_color_brewer(palette="Set1")+
  theme(legend.position = "bottom")+
  theme_classic())
#geom_point(data=subset(dataBDRF4,dataBDRF4$Species_Code=="173"),aes(x=Final_Age,y=Length*10,color=factor(Species_Code)),position = "jitter")+
#geom_point(data=subset(dataBDRF4,dataBDRF4$Species_Code=="142"),aes(x=Final_Age,y=Length*10,color=factor(Species_Code)),position = "jitter")+
#guides(colour = guide_legend(override.aes = list(alpha = 1)))+

LN<-ggplot(dataAll1) +
  geom_point(aes(x=age, y=otoW, color = paste(outlierLN,species)), size = 3, alpha= 0.2) +
  geom_line(data=a, aes(age, a[,2])) +
  geom_line(data=a, aes(age, a[,3])) +
  labs(y="Otolith Weight (g)",x="Age",color="Outliers by Species")+
  scale_color_adfg(palette = "coho", discrete = TRUE, useexact = TRUE)+
  theme_classic() 

plot_grid(LVB, LN, labels = "AUTO",ncol=1,nrow=2)


table(filter(dataAll1,species==Tspecies)$outlierLVB, useNA = "always")
table(filter(dataAll1,species==Tspecies)$outlierLN, useNA = "always")

###Data export####
outliers<- dataAll1 %>% filter(outlierLVB=="out" | outlierLN=="out") #make table of just outliers
write.csv(outliers, "outliers.csv", sep=",", row.names=FALSE) #make csv of just outliers
write.csv(dataAll1, "outlier_results.csv", sep=",", row.names=FALSE) #make csv of all data with outlier results

