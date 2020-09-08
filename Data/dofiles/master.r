## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

rm(list = ls())
library(tidyverse)
library(reshape2)
library(list)
library(plotrix)

######################################################################
######################### Settings ###################################
######################################################################
# Paths
home <- file.path(Sys.getenv("HOME"),
  "Seafile/Heidelberg/Projects/CoVideo")
datapath <- file.path(home, "Data")
dofiles <- file.path(datapath, "dofiles")

# Colors
set3 <- RColorBrewer::brewer.pal(3, "Set2") 

######################################################################
######################### Functions ##################################
######################################################################
source(file.path(datapath, "dofiles/functions.r"))
source(file.path(datapath, "dofiles/functions.r"))

######################################################################
######################### Build Dataset ##############################
######################################################################
# Option to make standalone Video dataset
doVideo = FALSE

# Rewrite getData function
if (doVideo) {
  getData <- function(ename) {
    getVid(ename$Vid)
  } 
}

# Put all the datasets together
source(file.path(dofiles, "build_data.r"))
all.equal(nrow(dat_all), 14499) 
all.equal(floor(mean(dat_all$TreatList)*1000), 500) 

if (doVideo) {
  readr::write_csv(dat_all, path=file.path(datapath, "Derived", "VideoTime.csv")) 
} else {
  # Additional preprocessing
  source(file.path(home, "main.R"))
  readr::write_csv(dat_all, path=file.path(datapath, "Derived", "dat_all.csv")) 
  saveRDS(dat_all, file=file.path(datapath, "Derived", "dat_all.Rda")) 
}
