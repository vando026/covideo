## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

rm(list = ls())
library(tidyverse)
library(reshape2)
library(list)
library(plotrix)
library(covideo)

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
dat_all <- load_covideo()
