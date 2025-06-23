# Normative models of grey matter volumes
# Models are multivariate fractional polynomial regression (MFPR) 
#
# by Max Korbmacher, max.korbmacher@gmail.com
# Created: 24 April 2025
# Last change: 28 April 2025
#
# ------------------------------------------ #
#                 Contents
# ------------------------------------------ #
#
# 1. Data wrangling and loading data
# 2. Training MFPR models
#
# ------------------------------------------ #
#
#
#
# 1. Data wrangling and loading data -------
# clean up
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
savepath = "/Users/max/Documents/Local/MS/NormativeModels/results/" # define results/save/oputput path
# load packages with pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,mfp,neuroCombat,MatchIt)
# load data
df = read.csv("/Users/max/Documents/Local/Data/Lifespan/cortical_subcortical.csv")
ms = read.csv("/Users/max/Documents/Local/Data/Lifespan/MS_long.csv")
# data wrangling
names(df)[names(df) == "Left.Thalamus.Proper"] = "Left.Thalamus"
names(df)[names(df) == "Right.Thalamus.Proper"] = "Right.Thalamus"
#
cross = df %>% filter(session != 2 & session != 3 & diagnosis == "HC")
cross = cross %>% filter(diagnosis == "HC")
cross$sex = ifelse(cross$sex == "F" | cross$sex == "Female","female","male")
# provide some summary stats of the healthy control sample (including both training and test/HC participants)
cross %>% group_by(data) %>% summarize(N = length(na.omit(age)), M = mean(na.omit(age)), SD = sd(na.omit(age)), Min = min(na.omit(age)), Max = max(na.omit(age)))
#
# split off test data
ms$diagnosis = "MS"
cross$diagnosis = "HC"
cross = rbind(ms %>% filter(session == 1) %>% select(names(cross)),cross)
cross = na.omit(cross)
# 1:1 NN PS matching w/o replacement
m.out = matchit(factor(diagnosis) ~ age + sex + EstimatedTotalIntraCranialVol,
                data = cross,
                method = "nearest",
                distance = "glm")
m.out = match_data(m.out)
write.csv(x = m.out,file = paste(savepath,"cross_MSvsHC.csv",sep=""))
#
#
# filter out the test data
cross = cross[!cross$eid %in% m.out$eid,]
table(cross$sex)/nrow(cross)
#
# harmonize
covars = cross %>% dplyr::select(eid,sex,scanner,age,data)
#covars$sex = ifelse(covars$sex == "F" | covars$sex == "Female",0,1)
datasets = covars$data
covars$data = as.numeric(factor(cross$data))
cross = neuroCombat(t(cross%>%dplyr::select(EstimatedTotalIntraCranialVol,starts_with("Left"),starts_with("Right"), starts_with("lh"),starts_with("rh"))),batch=as.numeric(factor(cross$scanner)),mod=model.matrix(~covars$age+covars$sex), mean.only = T)
cross = data.frame(t(cross$dat.combat))
cross = cbind(covars,cross)
#
#
# sex-split
Female = cross %>% filter(sex == "female")
Male = cross %>% filter(sex == "male")
# 2. Training MFPR models -----------------
#
region = cross %>% select(starts_with("Left"),starts_with("Right"), starts_with("lh"),starts_with("rh")) %>% names
for (i in 1:length(region)){
  f1 = formula(paste(region[i],"~EstimatedTotalIntraCranialVol+fp(age,df=4)"))
  mfp.female.model = mfp(f1, data=Female%>%select(age,EstimatedTotalIntraCranialVol,all_of(region[i])))
  mfp.male.model = mfp(f1, data=Male%>%select(age,EstimatedTotalIntraCranialVol,all_of(region[i])))
  save(mfp.female.model, file=paste(savepath,"models/",region[i],"_female.Rdata",sep=""))
  save(mfp.male.model, file=paste(savepath,"models/",region[i],"_male.Rdata",sep=""))
}
#
# Done.