## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

library(tidyverse)
library(readr)
library(reshape2)
library(list)


home <- file.path(Sys.getenv("HOME"), "Seafile\\Heidelberg\\Projects\\CoVideo\\Data")

# Drop completion codes
# 2E576104

######################################################################
######################### Read data ##################################
######################################################################
source(file.path(home, "dofiles/functions.r"))
source(file.path(home, "dofiles/build_data.r"))
dat_all <- readRDS(file.path(home, "Derived/dat_all.Rda"))
source(file.path(home, "dofiles/know.r"))

saveRDS(dat_all, file=file.path(home, "Derived", "dat_all.Rda")) 
######################################################################
######################### Test #######################################
######################################################################

sourced <- file.path(home, "Data_EN/v19")
Dem <- getDem("data_exp_17717-v19_questionnaire-lf5j.csv")
ListTrt <- getList("data_exp_17717-v19_task-lekh.csv")
ListCtrl <- getList("data_exp_17717-v19_task-7ce7.csv")
List <- rbind(ListCtrl, ListTrt)
dps <- duplicated(List[, c("ID", "Label")])
List <- List[!dps, ]
List <- doCast(List)
dd <- left_join(Dem, List, by="ID")
trt <- filter(dd, Treat==1)
ctrl <- filter(dd, Treat==0)
sd(ctrl$SocialDist)
mean(ctrl$SocialDist)
mean(trt$SocialDist)
sd(trt$SocialDist)
Know <- getKnow("data_exp_17717-v19_task-8qrr.csv")
Behav <- clean("data_exp_17717-v19_task-aaaf.csv")
CtrVid <- getVid("data_exp_17717-v19_task-7o8q.csv")

Know <- clean("data_exp_17717-v19_task-8qrr.csv")

# Sample size
muA=2.0
muB=2.1
kappa=1
sdA=0.85
sdB=0.95
tau=2
alpha=0.05
beta=0.20
(nA=(sdA^2+sdB^2/kappa)*((qnorm(1-alpha/tau)+qnorm(1-beta))/(muA-muB))^2)
ceiling(nA) # 85
z=(muA-muB)/sqrt(sdA^2+sdB^2/kappa)*sqrt(nA)
(Power=pnorm(z-qnorm(1-alpha/tau)))



