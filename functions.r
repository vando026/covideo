## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

# Read and clean data
clean <- function(name, ctype=col_character) {
  dat <- read_csv(file.path(sourced, name), comment="END OF FILE", 
    col_types=cols_only(
      `Participant Private ID` = col_integer(),
      Label = col_character(), 
      Response=ctype()))
  dat <- rename(dat, ID = `Participant Private ID`)
  dat <- filter(dat, !is.na(Response))
}

# Reshape to wide
doCast <- function(dat) {
  dat <- dcast(dat, ID ~ Label, value.var="Response")
  tibble::as_tibble(dat)
}

# Get list data
getList <- function(name)   {
  dat <- clean(name, ctype=col_integer)
  dat
}

# Get dem data
getDem <- function(name) {
  dat <- read_csv(file.path(sourced, name), comment="END OF FILE")
  dat <- select(dat, ID=`Participant Private ID`, Date=`Local Date`, 
    VideoArm=`randomiser-alpe`, ListArm=`randomiser-3tmz`, 
    Age=`Age-quantised`, Gender=`Gender-quantised`, 
    Country=`Country-quantised`, Educ=`Educ-quantised`)
  dat <- mutate(dat, 
    ID = as.integer(ID),
    Date = as.Date(gsub("\\s+.*", "", Date), format="%d/%m/%Y"))
  dat
}

# Get Kowledge data
getKnow <- function(name) {
  dat <- clean(name)
  dat <- mutate(dat, Response = as.numeric(Response=="True"))
  dat <- doCast(dat)
  dat
}

# Get Behav data
getBehav <- function(name) {
  dat <- clean(name)
  dat <- doCast(dat)
  dat
}

# Get video end data
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

# Make file names easier
mkName <- function(code="", type) {
  # browser()
  x <- as.list(paste0("data_exp_", code, "_", type, ".csv"))
  names(x) <- c("Dem", "ListTrt", "ListCtrl",  "Know", "Behav", "Vid")
  x
}

# Merge all the data
getData <- function(sourced, name=NULL) {
  Dem <- getDem(name$Dem)
  ListTrt <- getList(name$ListTrt)
  ListCtrl <- getList(name$ListCtrl)
  List <- doCast(rbind(ListCtrl, ListTrt))
  Know <- getKnow(name$Know)
  Behav <- getBehav(name$Behav)
  Vid <- getVid(name$Vid)
  dat <- Reduce(left_join, list(Dem, List, Behav, Know, Vid))
  dat
}

