## Description: Build the datasets from Gorilla
## Project: CoVideo
## Author: AV / Created: 23May2020 

ename <- c("questionnaire-lf5j", "task-lekh", 
  "task-7ce7",  "task-8qrr", "task-aaaf", "task-7o8q")

######################################################################
######################### En #########################################
######################################################################
cov_en <- mkName("17717-v19", ename)
sourced <- file.path(home, "Data_EN/v19") 
en_v19 <- getData(sourced, cov_en)

cov_en <- mkName("17717-v20", ename)
sourced <- file.path(home, "Data_EN/v20") 
en_v20 <- getData(sourced, cov_en)

cov_en <- mkName("17717-v21", ename)
sourced <- file.path(home, "Data_EN/v21") 
en_v21 <- getData(sourced, cov_en)

dat_en <- rbind(en_v19, en_v20, en_v21)
saveRDS(dat_en, file=file.path(home, "Derived", "dat_en.Rda"))



######################################################################
######################### DE #########################################
######################################################################
cov_de <- mkName("17845-v7", ename)
sourced <- file.path(home, "Data_DE/v7") 
de_v7 <- getData(sourced, cov_de)

cov_de <- mkName("17845-v9", ename)
sourced <- file.path(home, "Data_DE/v9") 
de_v9 <- getData(sourced, cov_de)

cov_de <- mkName("17845-v13", ename)
sourced <- file.path(home, "Data_DE/v13") 
de_v13 <- getData(sourced, cov_de)

dat_de <- rbind(de_v7, de_v9, de_v13)
saveRDS(dat_de, file=file.path(home, "Derived", "dat_de.Rda"))

######################################################################
######################### SP #########################################
######################################################################



######################################################################
######################### MX #########################################
######################################################################
