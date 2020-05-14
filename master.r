## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

library(tidyverse)
library(readr)
library(reshape2)


home <- file.path(Sys.getenv("HOME"), "Seafile\\Heidelberg\\Projects\\CoVideo\\Data")
source(file.path(home, "dofiles/functions.r"))

# Drop completion codes
# 2E576104

type <- c("questionnaire-lf5j", "task-lekh", "task-7ce7",  "task-8qrr", "task-aaaf", "task-7o8q")
cov_en <- mkName("17717-v19", type)
cov_de <- mkName("17845-v7", type)

######################################################################
######################### English ####################################
######################################################################
sourced <- file.path(home, "Phase1_EN") 
dat_en <- getData(sourced, cov_en)

sourced <- file.path(home, "Phase1_DE") 
dat_de <- getData(sourced, cov_de)





######################################################################
######################### Test #######################################
######################################################################

# Dem <- getDem("data_exp_17717-v19_questionnaire-lf5j.csv")
# ListTrt <- getList("data_exp_17717-v19_task-lekh.csv")
# ListCtrl <- getList("data_exp_17717-v19_task-7ce7.csv")
# List <- doCast(rbind(ListCtrl, ListTrt))
# Know <- getKnow("data_exp_17717-v19_task-8qrr.csv")
# Behav <- clean("data_exp_17717-v19_task-aaaf.csv")
# CtrVid <- getVid("data_exp_17717-v19_task-7o8q.csv")




