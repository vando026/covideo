## Description: Build the datasets from Gorilla
## Project: CoVideo
## Author: AV / Created: 23May2020 

#' @title build_data
#' 
#' @description  Builds the CoVideo dataset
#' 
#' @param  datapath The path to the data
#' @param  getData The function to getData or getVideo data
#' 
#' @return data.frame
#'
#' @export 

build_data <- function(datapath=set_paths()$datapath,
  getDataFun=getData) {

  ename <- c("questionnaire-lf5j", "task-lekh", 
    "task-7ce7",  "task-8qrr", "task-aaaf", "task-7o8q", "task-4u1p")

  ######################################################################
  ######################### En #########################################
  ######################################################################
  sourced <- file.path(datapath, "Data_EN/v19") 
  cov_en <- mkName("17717-v19", ename, sourced)
  en_v19 <- getDataFun(cov_en)

  sourced <- file.path(datapath, "Data_EN/v20") 
  cov_en <- mkName("17717-v20", ename, sourced)
  en_v20 <- getDataFun(cov_en)

  sourced <- file.path(datapath, "Data_EN/v21") 
  cov_en <- mkName("17717-v21", ename, sourced)
  en_v21 <- getDataFun(cov_en)

  sourced <- file.path(datapath, "Data_EN/v25") 
  cov_en <- mkName("17717-v25", ename, sourced)
  en_v25 <- getDataFun(cov_en)

  sourced <- file.path(datapath, "Data_EN/v26") 
  cov_en <- mkName("17717-v26", ename, sourced)
  en_v26 <- getDataFun(cov_en)

  dat_en <- rbind(en_v19, en_v20, en_v21, en_v25, en_v26)
  dat_en <- mutate(dat_en, Language="EN")


  ######################################################################
  ######################### DE #########################################
  ######################################################################
  sourced <- file.path(datapath, "Data_DE/v7") 
  cov_de <- mkName("17845-v7", ename, sourced)
  de_v7 <- getDataFun(cov_de)

  sourced <- file.path(datapath, "Data_DE/v9") 
  cov_de <- mkName("17845-v9", ename, sourced)
  de_v9 <- getDataFun(cov_de)

  sourced <- file.path(datapath, "Data_DE/v13") 
  cov_de <- mkName("17845-v13", ename, sourced)
  de_v13 <- getDataFun(cov_de)

  dat_de <- rbind(de_v7, de_v9, de_v13)
  dat_de <- mutate(dat_de, Language="DE")

  ######################################################################
  ######################### SP #########################################
  ######################################################################
  sourced <- file.path(datapath, "Data_SP/v5")
  cov_sp <- mkName("18351-v5" , ename, sourced)
  dat_sp <- getDataFun(cov_sp)
  dat_sp <- mutate(dat_sp, Language="SP")

  ######################################################################
  ######################### MX #########################################
  ######################################################################
  sourced <- file.path(datapath, "Data_MX/v5")
  cov_mx <- mkName("18353-v5" , ename, sourced)
  dat_mx <- getDataFun(cov_mx)
  dat_mx <- mutate(dat_mx, Language="MX")

  ######################################################################
  ######################### All ########################################
  ######################################################################
  dat_all <- bind_rows(dat_en, dat_de, dat_sp, dat_mx)
  
  return(dat_all)
} 

#' @title build_covideo
#' 
#' @description  Build all the CoVideo data and write to file
#' 
#' @param  datapath
#' 
#' @return 
#'
#' @export 

build_covideo <- function(datapath=set_paths()$datapath) {
  dat <- build_data()
  dat <- recode_data(dat)
  dat <- dropMissKnow(dat)
  # Calculate knowledge scores
  dat$SpreadTotal <- apply(dat, 1, setKnow, sstate())
  dat$ClinicTotal <- apply(dat, 1, setKnow, cstate())
  dat$KnowledgeAll <- apply(dat, 1, setKnow, c(sstate(), cstate()))
  readr::write_csv(dat, path=file.path(datapath, "Derived", "dat_all.csv")) 
  saveRDS(dat, file=file.path(datapath, "Derived", "dat_all.Rda")) 
}


#' @title build_video
#' 
#' @description  Build only the Video data and write to file
#' 
#' @param  datapath
#' 
#' @return  data.frame
#'
#' @export 

build_video <- function(datapath=set_paths()$datapath) {
  dat <- build_data(getDataFun=getVid)
  readr::write_csv(dat, path=file.path(datapath, "Derived", "VideoTime.csv")) 
  saveRDS(dat, file=file.path(datapath, "Derived", "dat_video.Rda")) 
}


######################################################################
######################### recode #####################################
######################################################################
# recode variables into EN language (this is why choose quantized)
# Make over 60 into 59 years

#' @title recode_data
#' 
#' @description  Recode the CoVideo data
#' 
#' @param dat_all The data_all dataset created from build_data
#' 
#' @return data.frame
#'
#' @export 

recode_data <- function(dat_all) {

  dat_all$Age[dat_all$Age==6] <- 5
  dat_all <- mutate(dat_all, 
    Age = recode(Age, 
      `1`="18-24 years", 
      `2`="25-34 years", 
      `3`="35-44 years",
      `4`="45-54 years", 
      `5`="55-59 years"),
    Gender = recode(Gender, 
      `1`="Male", 
      `2`="Female", 
      `3`="Other"),
    Country = recode(Country, 
      `1`="United States", 
      `2`="United Kingdom", 
      `3`="Germany", 
      `4`="Spain", 
      `5`="Mexico"),
    Educ = recode(Educ, 
      `1`="No Primary", 
      `2`="Some Primary", 
      `3`="Primary school", 
      `4`="Some High school", 
      `5`="High school", 
      `6`="Some college", 
      `7`="Bachelors", 
      `8`="Master/Diploma", 
      `9`="Doctorate")
  )

  dat_all$Educ2 <- 
    ifelse(grepl("Primary|Some High", dat_all$Educ), "Primary or less", 
    ifelse(dat_all$Educ %in% c("High school"), "Completed High School",
    ifelse(dat_all$Educ %in% c("Some college", "Bachelors"), "Some College, BA", "MA, PhD")))
  dat_all$Educ2 <- factor(dat_all$Educ2, 
    levels = c("Primary or less", "Completed High School", "Some College, BA", "MA, PhD"))
  
  return(dat_all)

}

dropMissKnow <- function(dat_all) {
  kdat <- dat_all[, c("ID", names(sstate()), names(cstate()))]
  dropID <- kdat[apply(kdat, 1, function(x) any(is.na(x))), ]$ID
  dat_all <- dat_all[!dat_all$ID %in% dropID, ]
  dat_all
}

setKnow <- function(irow, state) {
  irow <- irow[names(state)]
  correct <- sapply(names(irow), function(x) state[[x]][2])
  sum(irow==correct)
}

