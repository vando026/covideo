## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

library(tidyverse)
library(readr)
library(reshape2)


home <- Sys.getenv("HOME")
sourced <- file.path(home, "Seafile\\Heidelberg\\Projects\\CoVideo\\Data\\Phase1_EN") 
list.files(sourced)

# Drop completion codes
# 2E576104


name <- "data_exp_17717-v19_task-8qrr.csv"
dat <- read_csv(file.path(sourced, name), comment="END OF FILE", 
  col_types=cols_only(
  `Participant Private ID` = col_double(),
  Label = col_character(),
  Response=col_character()))
dat <- rename(dat, ID = `Participant Private ID`)


clean <- function(name, ctype=col_character) {
  dat <- read_csv(file.path(sourced, name), comment="END OF FILE", 
    col_types=cols_only(
      `Participant Private ID` = col_double(),
      Label = col_character(), 
      Response=ctype()))
  dat <- rename(dat, ID = `Participant Private ID`)
  dat <- filter(dat, !is.na(Response))
}

doCast <- function(dat) 
  dcast(dat, ID ~ Label, value.var="Response")

getList <- function(name)   {
  dat <- clean(name, ctype=col_integer)
  dat
}

getDem <- function(name) {
  dat <- read_csv(file.path(sourced, name), comment="END OF FILE")
  dat <- select(dat, ID=`Participant Private ID`, Date=`Local Date`, 
    starts_with("randomiser"), Age, Gender, Country, Educ )
  dat
}

getKnow <- function(name) {
  dat <- clean(name)
  dat <- mutate(dat, Response = as.numeric(Response=="True"))
  dat <- doCast(dat)
  dat
}

getBehav <- function(dat) {
  dat <- clean(name)
  dat <- doCast(dat)
  dat
}

getVid <- function(name) {
  dat <- read_csv(file.path(sourced, name), comment='END OF FILE')
  dat <- select(dat, ID=`Participant Private ID`, Stamp=`UTC Timestamp`, Label=`Zone Name`)
  dat <- filter(dat, !is.na(Label))
  dat <- arrange(dat, ID, Stamp)
  dat <- group_by(dat, ID) %>% 
    mutate(Lag = lag(Stamp), Time = round((Stamp - Lag)/1000, 2)) %>% 
    filter(row_number()==n()) %>% 
    select(ID, CtrVidTime=Time)
  dat
}


ListTrt <- getList("data_exp_17717-v19_task-lekh.csv")
ListCtrl <- getList("data_exp_17717-v19_task-7ce7.csv")
List <- rbind(ListCtrl, ListTrt)
ListLong <- dcast(List, ID ~ Label, value.var="Response")
Know <- clean("data_exp_17717-v19_task-8qrr.csv")
Behav <- clean("data_exp_17717-v19_task-aaaf.csv")
CtrVid <- getVid("data_exp_17717-v19_task-7o8q.csv")



dat <- Reduce(left_join, list(Dem, ListLong, Behav, Know, CtrVid))
dat <- mutate(dat, as.Date(gsub("\\s+.*", "", Date), format="%d/%m/%Y")) 

