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
output <- file.path(home, "drafts/output")

# Colors
set3 <- RColorBrewer::brewer.pal(3, "Set2") 

######################################################################
######################### Functions ##################################
######################################################################
source(file.path(home, "dofiles/functions.r"))

######################################################################
######################### Build Dataset ##############################
######################################################################
if (FALSE) {
  # Put all the datasets together
  source(file.path(home, "dofiles/build_data.r"))
  saveRDS(dat_all, file=file.path(datapath, "Derived", "dat_all.Rda")) 
}
# Additional preprocessing
dat_all <- readRDS(file=file.path(datapath, "Derived", "dat_all.Rda")) 
source(file.path(home, "dofiles/main.r"))


######################################################################
######################### Do Figures #################################
######################################################################
