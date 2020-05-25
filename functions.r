## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

# Read Prolific data
readProA <- function(name, fpath=file.path(home, "Prolific")) {
  dat <- read_csv(file.path(fpath, name))
  dat <- mutate(dat, TimeMin = time_taken/60)
  dat 
}

# Read and clean data
clean <- function(name, ctype=col_character) {
  dat <- suppressMessages(
    read_csv(file.path(sourced, name), comment="END OF FILE", 
    col_types=cols_only(
      `Participant Private ID` = col_integer(),
      Label = col_character(), 
      Response=ctype())))
  dat <- rename(dat, ID = `Participant Private ID`)
  dat <- filter(dat, !is.na(Response))
}

# Reshape to wide
doCast <- function(dat) {
  tidyr::pivot_wider(dat, names_from=Label, values_from=Response)
}

# Get list data
getList <- function(name)   {
  dat <- suppressWarnings(clean(name, ctype=col_integer))
  dat <- dropDups(dat)
  dat
}

# Get dem data
getDem <- function(name) {
  dat <- suppressMessages(
    read_csv(file.path(sourced, name), comment="END OF FILE"))
  dat <- select(dat, 
    ID=`Participant Private ID`, 
    SessionID=`Participant External Session ID`,
    Date=`Local Date`, 
    VideoArm=`randomiser-alpe`, ListArm=`randomiser-3tmz`, 
    Age=`Age-quantised`, Gender=`Gender-quantised`, 
    Country=`Country-quantised`, Educ=`Educ-quantised`)
  dat <- mutate(dat, 
    ID = as.integer(ID),
    Date = as.Date(gsub("\\s+.*", "", Date), format="%d/%m/%Y"),
    Treat = as.numeric(ListArm=="List Treatment"))
  dat
}

showDupList <- function(dat) {
  Dups <- group_by(dat, ID, Label) %>% 
    filter(n()>1)
  dupid <- (unique(Dups[, "ID", drop=TRUE]))
  if (length(dupid)>0) {
    message(" !! The following duplicates found")
    print(dupid)
  }
  return(dupid)
}

dropDups <- function(dat, dVars=c("ID", "Label")) {
  dupid <- showDupList(dat)
  if (length(dupid) > 0) {
    dups <- duplicated(dat[, dVars])
    dat <- dat[!dups, ]
  }
  return(dat)
}


# Get Kowledge data
getKnow <- function(name) {
  dat <- clean(name)
  dat <- mutate(dat, 
    Response = as.numeric(Response %in% c("True", "Wahr")))
  dat <- dropDups(dat)
  dat <- doCast(dat)
  dat
}

# Get Behav data
getBehav <- function(name) {
  dat <- clean(name)
  dat <- mutate(dat, 
    Response = as.numeric(!grepl("Disagree|nicht", Response)))
  dat <- dropDups(dat)
  dat <- doCast(dat)
  dat
}

# Get video end data
getVid <- function(name) {
  dat <- suppressMessages(
    read_csv(file.path(sourced, name), comment='END OF FILE'))
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
  List <- rbind(ListCtrl, ListTrt)
  # May be duplicates in arms
  List <- dropDups(List)
  List <- doCast(List )
  Know <- getKnow(name$Know)
  Behav <- getBehav(name$Behav)
  Vid <- getVid(name$Vid)
  dat <- suppressMessages(Reduce(left_join, list(Dem, List, Behav, Know, Vid)))
  if ("NA" %in% colnames(dat)) {
    message(sprintf(" Dropping NA column %s", which((colnames(dat)=="NA"))))
    dat <- select(dat, -(`NA`))
  }
  message(sprintf(" ==> Reading %s rows from %s\n", nrow(dat), sourced))
  dat
}

setKnow <- function(correct) {
  function(irow) {
    stopifnot(length(irow[-1])==length(correct))
    score <- sum(irow[-1]==correct)
    c(irow$ID, score)
  }
}
