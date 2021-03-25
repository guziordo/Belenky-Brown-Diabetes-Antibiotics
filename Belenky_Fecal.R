#Set-Up----
setwd("C:/Users/dougg/OneDrive - Michigan State University/Lab Docs/Belenky Diabetic Microbiome Resiliance")

library(tidyverse)
library(readr)
library(randomForest)
library(vegan)
library(summarytools)
library(wordspace)
library(data.table)
library(ROCR)

set.seed(42)

rawdat <- read.csv("bucket.csv")
#GNPS run: https://gnps.ucsd.edu/ProteoSAFe/status.jsp?task=e4efce0c33fb4ada96e373d53460f2d5

#Data Wrangling----
pivot <- t(rawdat)
metadata <- rawdat[,c(1:9)]
pivot <- as.data.frame(pivot)
names(pivot) <- as.matrix(pivot[1, ])
pivot <- pivot[-(1:9), ] #Removes attribute rows, only clusters remain
pivot[] <- lapply(pivot, function(x) type.convert(as.numeric(x)))
norm <- scale(pivot, center = F, scale = colSums(pivot))
norm <- as.data.frame(t(norm))
norm <- setDT(norm, keep.rownames = TRUE)[]; names(norm)[1] <- "filename"
names(metadata)[1] <- "filename"
rebuilt <- merge(metadata, norm, by = "filename")

cipro <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic != "amoxicillin")
names(cipro) <- make.names(names(cipro));cipro <- droplevels(cipro)
amox <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic != "ciprofloxacin")
names(amox) <- make.names(names(amox)); amox <- droplevels(amox)

cipro_noV <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic == "ciprofloxacin")
names(cipro_noV) <- make.names(names(cipro_noV));cipro_noV <- droplevels(cipro_noV)
amox_noV <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic == "amoxicillin")
names(amox_noV) <- make.names(names(amox_noV)); amox_noV <- droplevels(amox_noV)

control <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic == "vehicle")
names(control) <- make.names(names(control)); control <- droplevels(control)

#RandomForests with vehicle controls----
cipro.rf <- randomForest(as.factor(cipro$ATTRIBUTE_Diabetic_Treatment) ~., data = cipro[,10:4597], ntree = 5000, na.rm=T, importance=T); cipro.rf
cipro.VIP <- varImpPlot(cipro.rf, type=1)
write.csv(cipro.VIP, file = "cipro_VarImp.csv")

amox.rf <- randomForest(as.factor(amox$ATTRIBUTE_Diabetic_Treatment) ~., data = amox[,10:4597], ntree = 5000, na.rm=T, importance=T); amox.rf
amox.VIP <- varImpPlot(amox.rf, type=1)
write.csv(amox.VIP, file = "amox_VarImp.csv")

cipro_noV.rf <- randomForest(as.factor(cipro_noV$ATTRIBUTE_Treatment) ~., data = cipro_noV[,10:4597], ntree = 5000, na.rm=T, importance=T); cipro_noV.rf
cipro.VIP <- varImpPlot(cipro.rf, type=1)
write.csv(cipro.VIP, file = "cipro_noV_VarImp.csv")

amox_noV.rf <- randomForest(as.factor(amox_noV$ATTRIBUTE_Treatment) ~., data = amox_noV[,10:4597], ntree = 5000, na.rm=T, importance=T); amox_noV.rf
amox_noV.VIP <- varImpPlot(amox_noV.rf, type=1)
write.csv(amox.VIP, file = "amox_noV_VarImp.csv")

control.rf <- randomForest(as.factor(control$ATTRIBUTE_Treatment) ~., data=control[,10:4597], ntree = 5000, na.rm=T, importance=T); control.rf
control.VIP <- varImpPlot(control.rf, type=1)
write.csv(control.VIP, file= "vehicle_VarImp.csv")

#Additional RF----
ctrl_VC <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic != "amoxicillin") %>% filter(ATTRIBUTE_Treatment != "stz")
ctrl_VA <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic != "ciprofloxacin") %>% filter(ATTRIBUTE_Treatment != "stz")
stz_VC <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic != "amoxicillin") %>% filter(ATTRIBUTE_Treatment != "control")
stz_VA <- rebuilt %>% filter(rebuilt$ATTRIBUTE_Antibiotic != "ciprofloxacin") %>% filter(ATTRIBUTE_Treatment != "control")

ctrl_VA.rf <- randomForest(as.factor(ctrl_VA$ATTRIBUTE_Diabetic_Treatment) ~., data = ctrl_VA[,10:4597], ntree = 5000, na.rm=T, importance=T); ctrl_VA.rf
ctrl_VA.VIP <- varImpPlot(ctrl_VA.rf, type=1)
write.csv(ctrl_VA.VIP, file = "ctrl_VA.csv")

ctrl_VC.rf <- randomForest(as.factor(ctrl_VC$ATTRIBUTE_Diabetic_Treatment) ~., data = ctrl_VC[,10:4597], ntree = 5000, na.rm=T, importance=T); ctrl_VC.rf
ctrl_VC.VIP <- varImpPlot(ctrl_VC.rf, type=1)
write.csv(ctrl_VC.VIP, file = "ctrl_VC.csv")

stz_VA.rf <- randomForest(as.factor(stz_VA$ATTRIBUTE_Diabetic_Treatment) ~., data = stz_VA[,10:4597], ntree = 5000, na.rm=T, importance=T); stz_VA.rf
stz_VA.VIP <- varImpPlot(stz_VA.rf, type=1)
write.csv(stz_VA.VIP, file = "stz_VA.csv")

stz_VC.rf <- randomForest(as.factor(stz_VC$ATTRIBUTE_Diabetic_Treatment) ~., data = stz_VC[,10:4597], ntree = 5000, na.rm=T, importance=T); stz_VC.rf
stz_VC.VIP <- varImpPlot(stz_VC.rf, type=1)
write.csv(stz_VC.VIP, file = "stz_VC.csv")
