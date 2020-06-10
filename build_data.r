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

cov_en <- mkName("17717-v25", ename)
sourced <- file.path(home, "Data_EN/v25") 
en_v25 <- getData(sourced, cov_en)

cov_en <- mkName("17717-v26", ename)
sourced <- file.path(home, "Data_EN/v26") 
en_v26 <- getData(sourced, cov_en)

dat_en <- rbind(en_v19, en_v20, en_v21, en_v25, en_v26)
dat_en <- mutate(dat_en, Language="EN")


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
dat_de <- mutate(dat_de, Language="DE")
saveRDS(dat_de, file=file.path(home, "Derived", "dat_de.Rda"))

######################################################################
######################### SP #########################################
######################################################################



saveRDS(dat_sp, file=file.path(home, "Derived", "dat_sp.Rda"))

######################################################################
######################### MX #########################################
######################################################################



saveRDS(dat_mx, file=file.path(home, "Derived", "dat_mx.Rda"))

######################################################################
######################### All ########################################
######################################################################
dat_en <- readRDS(file.path(home, "Derived/dat_en.Rda"))
dat_de <- readRDS(file.path(home, "Derived/dat_de.Rda"))
dat_mx <- readRDS(file.path(home, "Derived/dat_mx.Rda"))
dat_sp <- readRDS(file.path(home, "Derived/dat_sp.Rda"))
dat_all <- bind_rows(dat_en, dat_de, dat_sp, dat_mx)

# recode Age
dat_all <- mutate(dat_all, 
  Age = recode(Age, 
    `1`="18-24 yrs", 
    `2`="25-34 yrs", 
    `3`="35-44 yrs",
    `4`="45-54 yrs", 
    `5`="55-59 yrs", 
    `6`="60+ yrs"),
  Gender = recode(Gender, 
    `1`="Male", 
    `2`="Female", 
    `3`="Other"),
  Country = recode(Country, 
    `1`="US", 
    `2`="UK", 
    `3`="DE", 
    `4`="SP", 
    `5`="MX"),
  Educ = recode(Educ, 
    `1`="No Primary", 
    `2`="Some Primary", 
    `3`="Primary", 
    `4`="Some High school", 
    `5`="High ", 
    `6`="Some college", 
    `7`="Bachelors", 
    `8`="Master/Diploma", 
    `9`="Doctorate")
)

saveRDS(dat_all, file=file.path(home, "Derived", "dat_all.Rda"))
