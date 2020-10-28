## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

#' @title set_paths
#' 
#' @description  Set paths
#' 
#' @param Path to Seafile folder
#' 
#' @return 
#'
#' @export 

set_paths  <- function(home="Seafile/Heidelberg/Projects/CoVideo") {
  home <- file.path(Sys.getenv("HOME"), home)
  datapath <- file.path(home, "Data")
  dofiles <- file.path(datapath, "dofiles")
  return(list(home=home, datapath=datapath, dofiles=dofiles))
}


#' @title load_covideo
#' 
#' @description  Load the Covideo data
#' 
#' @param Path to Seafile folder
#' 
#' @return data.frame
#'
#' @export 

load_covideo <- function(path=set_paths()) {
  readRDS(file=file.path(path$datapath, "Derived", "dat_all.Rda")) 
}

#' @title load_video
#' 
#' @description  Load the Covideo participation data
#' 
#' @param Path to Seafile folder
#' 
#' @return  data.frame
#'
#' @export 

load_video <- function(path=set_paths()) {
  readRDS(file=file.path(path$datapath, "Derived", "dat_video.Rda")) 
}
