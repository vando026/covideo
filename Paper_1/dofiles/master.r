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
dofiles <- file.path(home, "Paper_1/dofiles")
datapath <- file.path(home, "Data")
output <- file.path(home, "Paper_1/output")

# Colors
set3 <- RColorBrewer::brewer.pal(3, "Set2") 

######################################################################
######################### Read  Dataset ##############################
######################################################################

# Option to make standalone Video dataset
doVideo = FALSE

if (TRUE) {
  # Rewrite getData function
  if (doVideo) {
    getData <- function(sourced, ename) {
      getVid(sourced,  ename$Vid)
    } 
  }
  # Put all the datasets together
  source(file.path(home, "dofiles/build_data.r"))
  if (!doVideo) {
    all.equal(nrow(dat_all), 14499) 
    all.equal(floor(mean(dat_all$TreatList)*1000), 500) 
    saveRDS(dat_all, file=file.path(datapath, "Derived", "dat_all.Rda")) 
  } else  {
    readr::write_csv(dat_all, path=file.path(datapath, "Derived", "VideoTime.csv")) 
  }
}

# Additional preprocessing
dat_all <- readRDS(file=file.path(datapath, "Derived", "dat_all.Rda")) 
source(file.path(home, "dofiles/main.R"))
readr::write_csv(dat_all, path=file.path(datapath, "Derived", "dat_all.csv")) 
# Run results and figures
# source(file.path(home, "dofiles/do-analysis.R"))
# source(file.path(home, "dofiles/do-figs.R"))
