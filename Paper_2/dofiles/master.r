## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

rm(list = ls())
library(tidyverse)
library(list)
library(covideo)

######################################################################
######################### Settings ###################################
######################################################################
# Paths
home <- file.path(Sys.getenv("HOME"),
  "Seafile/Heidelberg/Projects/CoVideo")
dofiles <- file.path(home, "Paper_2/dofiles")
datapath <- file.path(home, "Data")
output <- file.path(home, "Paper_2/output")

# Colors
set3 <- RColorBrewer::brewer.pal(3, "Set2")

######################################################################
######################### Read  Dataset ##############################
######################################################################
# source(file.path(dofiles, "do-analysis.R"))


# dat_all <- load_covideo()
# readr::write_csv(dat_all, path=file.path(datapath, "Derived/dat_all.csv"))

setwd('~/Seafile/Heidelberg/Projects/CoVideo/Data/covideo/')
devtools::install(dependencies=FALSE)

