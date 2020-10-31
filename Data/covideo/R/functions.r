## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

# Read Prolific data
readProA <- function(name, fpath=file.path(datapath, "Prolific")) {
  dat <- readr::read_csv(file.path(fpath, name))
  dat <- mutate(dat, TimeMin = time_taken/60)
  dat 
}

reviewStatus <- function(File, ln) {
  endat <- readProA(File)

  endat <- endat[!is.na(endat$completed_date_time), ]
  endat <- filter(endat, status == "AWAITING REVIEW")
  message("Total awaiting")
  print(table(endat$status))

  ddat <- filter(endat, TimeMin <=1)
  message("\nTo be rejected")
  print(table(ddat$status))
  ddat <- dplyr::select(ddat, participant_id)
  readr::write_delim(ddat, path=file.path(datapath, "Prolific", paste0(ln, "_reject.txt")))

  endat <- filter(endat, TimeMin > 1)
  message("\nTo be paid")
  print(table(endat$status))
  pdat <- dplyr::select(endat, participant_id)
  readr::write_delim(pdat, path=file.path(datapath, "Prolific", paste0(ln, "_pay.txt")))
}

#' @title clean
#' 
#' @description  Clean the data
#' 
#' @param name Name of csv file
#' @param ctype Type of column
#' 
#' @return data.frame
#' @import dplyr
#' @export 

clean <- function(name, ctype=col_character) {
  message(sprintf("==> Reading %s ", name))
  dat <- suppressMessages(
    readr::read_csv(file.path(name), comment="END OF FILE", 
    col_types=cols_only(
      `Participant Public ID` = col_character(),
      Label = col_character(), 
      Response=ctype())))
  dat <- rename(dat, ID = `Participant Public ID`)
  dat <- filter(dat, !is.na(Response))
}

# Reshape to wide
doCast <- function(dat) {
  tidyr::pivot_wider(dat, names_from=Label, values_from=Response)
}

# Get list data
getList <- function(name)   {
  dat <- suppressWarnings(clean(name, ctype=col_integer))
  # Fix label error
  dat$Label[dat$Label=="ShareUtensils"] = "CleanDishes"
  dat <- dropDups(dat)
  dat
}

#' @title getDem
#' 
#' @description  get Demographic data
#' 
#' @param name Name of csv file
#' 
#' @return data.frame
#'
#' @export 


getDem <- function(name) {
  message(sprintf("==> Reading %s ", name))
  dat <- suppressMessages(
    readr::read_csv(name, comment="END OF FILE"))
  dat <- dplyr::select(dat, 
    ID=`Participant Public ID`, 
    SessionID=`Participant External Session ID`,
    Date=`Local Date`, 
    VideoArm=`randomiser-alpe`, ListArm=`randomiser-3tmz`, 
    Age=`Age-quantised`, Gender=`Gender-quantised`, 
    Country=`Country-quantised`, Educ=`Educ-quantised`)
  dat <- mutate(dat, 
    Date = as.Date(gsub("\\s+.*", "", Date), format="%d/%m/%Y"),
    TreatList = as.numeric(ListArm=="List Treatment"))
  dat
}

#' @title showDupList
#' 
#' @description  Show duplicates
#' 
#' @param dat A dataset
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 

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


#' @title getKnow
#' 
#' @description  get Knowledge data
#' 
#' @param name Name of csv file
#' 
#' @return data.frame
#'
#' @export 


getKnow <- function(name) {
  dat <- clean(name)
  dat <- mutate(dat, Response = recode(Response, 
    True="True", False="False",
    Wahr="True", Falsch="False",
    Verdadero="True", Falso="False", 
    `9`="TimedOut", .default=NA_character_))
  dat <- dropDups(dat)
  dat <- doCast(dat)
  dat
}

# Get Behav data
getBehav <- function(name) {
  dat <- clean(name)
  dat$Label[dat$Label=="BEShareUtensils"] = "BECleanDishes"
  dat <- mutate(dat, 
    Response = as.numeric(!grepl("Disagree|nicht|desacuerdo", Response)))
  dat <- dropDups(dat)
  dat <- doCast(dat)
  dat
}


#' @title getVid
#' 
#' @description   Gets the video participation data
#' 
#' @param name Name of csv file
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 

getVid <- function(name) {
  message(sprintf("==> Reading %s ", name$Vid))
  dat <- suppressMessages(
    readr::read_csv(file.path(name$Vid), comment='END OF FILE'))
  dat <- dplyr::select(dat, 
    ID=`Participant Public ID`, 
    UTC_Timestamp=`UTC Timestamp`, 
    UTC_Date=`UTC Date`,
    Arm=`randomiser-alpe`,
    TrialNumber=`Trial Number`,
    ZoneName=`Zone Name`,
    ZoneType=`Zone Type`,
    Response=Response)
  gdat <- getGoodBye(name)
  dat <- rbind(dat, gdat)
  dat <- dplyr::arrange(dat, ID, UTC_Timestamp)
  dat <- group_by(dat, ID) %>% mutate(DiffTime = 
    round((UTC_Timestamp - lag(UTC_Timestamp))/1000, 2))
  idat <- split(dat, dat$ID)
  getTime <- function(irow) {
    ClickFinish = as.numeric(!is.na(any(irow$ZoneName == "FinishTime")))
    VideoSum = cumsum(as.numeric(irow$ZoneName=="Video" & !is.na(irow$ZoneName)))
    VideoMax = max(VideoSum)
    Error = any(grepl("ERROR|MEDIA_ERR", irow$Response))
    # cat(irow$ID[1], ClickFinish, VideoMax, Error)
    Time <- if(ClickFinish==1 & VideoMax>=1 & Error==0) {
      unlist(subset(irow, ZoneName=="FinishTime", DiffTime))
    } else if (ClickFinish==0 & VideoMax >= 1 & Error==0) {
      irow$DiffTime[(VideoSum==VideoMax)][1] 
    } else if (ClickFinish==1 & VideoMax==0) {
      0
    } else {
      NA
    }
    data.frame(ID=irow$ID[1], VideoTime=Time) 
  }
  ddat <- do.call(rbind, lapply(idat, getTime))
  left_join(dat, ddat)
}

#' @title getGoodBye
#' 
#' @description  Get Goodbye data
#' 
#' @param name Name of csv file
#' 
#' @return 
#' @import dplyr
#' @export 

getGoodBye <- function(name) {
  message(sprintf("==> Reading %s ", name$GBye))
  dat <- suppressMessages(
    readr::read_csv(file.path(name$GBye), comment='END OF FILE'))
  dat <- dplyr::select(dat, 
    ID=`Participant Public ID`, 
    UTC_Timestamp=`UTC Timestamp`, 
    UTC_Date=`UTC Date`,
    Arm=`randomiser-alpe`,
    TrialNumber=`Trial Number`,
    ZoneName=`Zone Name`,
    ZoneType=`Zone Type`,
    Response=Response)
  dat <- dplyr::filter(dat, TrialNumber %in% c("BEGIN TASK", "END TASK"))
  dat$ZoneName <- "Goodbye"
  dat
}



# Make file names easier
mkName <- function(code="", type, datapath) {
  # browser()
  x <- as.list(paste0("data_exp_", code, "_", type, ".csv"))
  names(x) <- c("Dem", "ListTrt", "ListCtrl",  "Know", "Behav", "Vid", "GBye")
  lapply(x, function(x) file.path(datapath, x))
}

#' @title getData
#' 
#' @description  Function to bring in all the data
#' 
#' @param name Name of csv file
#' 
#' @return data.frame
#' @import dplyr
#' @export
getData <- function(name) {
  Dem <- getDem(name$Dem)
  ListTrt <- getList(name$ListTrt)
  ListCtrl <- getList(name$ListCtrl)
  List <- rbind(ListCtrl, ListTrt)
  # May be duplicates in arms
  List <- dropDups(List)
  List <- doCast(List)
  Know <- getKnow(name$Know)
  Behav <- getBehav(name$Behav)
  dat <- suppressMessages(Reduce(left_join, list(Dem, List, Behav, Know)))
  if ("NA" %in% colnames(dat)) {
    message(sprintf(" Dropping NA column %s", which((colnames(dat)=="NA"))))
    dat <- dplyr::select(dat, -(`NA`))
  }
  message(sprintf("==> Reading %s rows from %s\n", 
    nrow(dat), name[1][[1]]))
  dat
}

#' @title bwrap
#' 
#' @description  Function to wrap behav statements
#' 
#' @param y The behav item
#' 
#' @return string
#'
#' @export 
bwrap <- function(y, len=14) 
  paste(strwrap(bstate()[y], len), collapse="\n")

#' @title kwrap
#' 
#' @description  Function to wrap know statements
#' 
#' @param y The know item
#' 
#' @return string
#'
#' @export 
kwrap <- function(y, len=14) {
  allknow = append(cstate(), sstate())
  paste(strwrap(allknow[[y]][1], len), collapse="\n")
}

######################################################################
######################### Analysis ###################################
######################################################################
getMeanSE <- function(x, dat) {
  mod <- lm(as.formula(paste(x, "~ -1 + VideoArm:TreatList")), data=dat)
  out <- data.frame(summary(mod)$coefficients[, c(1, 2)])
  colnames(out) <- c("Mn", "SE")
  out$Arm  <- gsub("VideoArm|:TreatF", "", rownames(out))
  out$Mn <- round(out$Mn, 3)
  out$SE  <- round(out$SE, 3)
  out <- tidyr::pivot_wider(out, names_from=Arm, values_from=c(Mn, SE))
  out[, unlist(Map(c, 1:6, 7:12))]
}


mkMod <- function(model=lm, RHS, ...) {
  function(y, dat) {
    mns = model(as.formula(paste(y, RHS)), data=dat, ...)
    out <- summary(mns)$coefficients 
    out[, 4] <- round(out[, 4], 4)
    out
  }
}

#' @title getListData
#' 
#' @description  Selects list experiment vars
#' 
#' @param dat
#' 
#' @return data.frame
#'
#' @export
getListData <- function(dat=load_covideo()) {
  ldat <- dplyr::select(dat, VideoArm, TreatList, 
    SocialDist, Wash, StockPile, CleanDishes, CleanSurfaces, UseMedia)
  mutate(ldat, 
    VideoArm = as.factor(VideoArm), TreatList = as.factor(TreatList))
}

######################################################################
######################### Plots ######################################
######################################################################
pbrack <- function(x0, x1, y, h=0.01, pval="p < 0.05", CEX=1, ...) {
  segments(x0, y, x1, y, ...)
  segments(x0, y-h, x0, y, ...)
  segments(x1, y-h, x1, y, ...)
  text((x0 + x1)/2, y + (h*1.2), pval, cex=CEX)
}

#' @title pfmt
#' 
#' @description  format pvalues
#' 
#' @param x
#' 
#' @return string
#'
#' @export 
pfmt <- function(x) {
  x <- formatC(x, format="f", digits=3)
  ifelse(x=="0.001", "p < 0.001", paste0("p = ", x))
}

fmt <- function(x, y=2) formatC(x, format="f", digits=y)



#' @title doRegDirect
#' 
#' @description  get direct behave means
#' 
#' @param LHS
#' @param dat
#' 
#' @return NULL
#'
#' @export 
# Function to get means and cis for direct behav intent
doRegDirect <- function(LHS, dat) {
    mod <- lm(as.formula(paste(LHS, " ~ 1")), data=dat)
    cf <- round(coef(mod)[1]*100, 1)
    ci <- round(confint(mod)*100, 1) 
    return(c(cf, ci))
}

#' @title plotKnow
#' 
#' @description  Plot knowledge
#' 
#' @param  LHS
#' @param  Title
#' 
#' @return 
#'
#' @export 

plotKnow <- function(LHS, Title="", yLim, ppos, H=0.01, plt=TRUE, write=TRUE, ...) {
  dat <- load_covideo()
  cmod <- lm(as.formula(paste(LHS, " ~ -1 + VideoArm")), data=dat)
  cpair <- pairwise.t.test(dat[[LHS]], dat[["VideoArm"]],
     p.adj = "none")
  att <- lincom("1*VideoArmPlacebo - 1*VideoArmControl = 0", cmod)
  cont <- lincom("1*VideoArmTreatment - 1*VideoArmPlacebo = 0", cmod)
  tot <- lincom("1*VideoArmTreatment - 1*VideoArmControl = 0", cmod)
  y <- cmod$coefficients
  cis <- confint(cmod)
  lis <- y - cis[, 1] 
  uis <- cis[, 2] - y
  se <- summary(cmod)$coefficients[, "Std. Error"]
  #
  if (plt) {
    if (write) {
      png(file.path(output, paste0(LHS, ".png")),
        units="in", width=5, height=5.0, pointsize=9, 
        res=500, type="cairo")
    }
    plotCI(1:3, y, liw=lis, uiw=uis, main=Title, 
      bty="n", ylim=yLim, lwd=3, pch=16, font.lab=2, cex.lab=1.2,
      xlab="Trial arm", ylab="", xaxt="n", col=set3[c(3, 2, 1)], ...)
    title(ylab="Mean score", line=3, cex.lab=1.2, font.lab=2)
    axis(1, 1:3, c("Control", "APC", "CoVideo"))
    text(c(1,2,2.75), y, pos=4, labels=formatC(y, format="f", digits=2))
    pbrack(1, 2, ppos[1], H, CEX=0.9,
      pval=paste0("Attention Effect = ", fmt(att$diff, 2),
        ",\n 95% CI (", fmt(att$lb, 2), ", ", fmt(att$ub, 2), "), ", 
        fmt(att$pval, 2)))
    pbrack(2, 3, ppos[2], H, CEX=0.9,
      pval=paste0("Content Effect = ", fmt(cont$diff, 2),
        ",\n 95% CI (", fmt(cont$lb, 2), ", ", fmt(cont$ub, 2), "), ", 
        fmt(cont$pval, 2)))
    pbrack(1, 3, ppos[3], H, CEX=0.9,
      pval=paste0("Total Effect = ", fmt(tot$diff, 2),
        ",\n 95% CI (", fmt(tot$lb, 2), ", ", fmt(tot$ub, 2), "), ", 
        fmt(tot$pval, 2)))
    if (write) dev.off()
  }
  return(list(means=y, se=se, cis=cis,
    attdiff=att, contdiff=cont, totdiff=tot))
}


# Regressions for knowledge
doRegKnow2 <- function(RHS, dat) {
  fmtp <- function(x) 
    ifelse(x <0.001, "<0.001", formatC(x, digits=3, format="f"))
  modc <- lm(as.formula(paste(RHS, "~ Age + Gender + Country + Educ2")),
    data=dat)
  modc <- summary(modc)$coefficients
  modc <- as.data.frame(modc)
  modc[4] <- sapply(modc[4], function(x) fmtp(x))
  rownames(modc) <- gsub("Age", "Age: ", rownames(modc))
  rownames(modc) <- gsub("Gender", "Gender: ", rownames(modc))
  rownames(modc) <- gsub("Country", "Residence: ", rownames(modc))
  rownames(modc) <- gsub("Educ2", "Education: ", rownames(modc))
  modc
}

#' @title doRegKnow
#' 
#' @description  Compute means for knowledge items
#' 
#' @param var Name of the know item
#' 
#' @return list
#'
#' @export 
doRegKnow <- function(var, donorm = FALSE) {
  allknow = append(cstate(), sstate())
  answer <- allknow[[var]][2]
  kdat[var][kdat[var] == "TimedOut"] <- NA
  kdat[var] <- as.numeric(kdat[var] == answer)
  Eq <- as.formula(paste(var, "~ -1 + VideoArm"))
  mod <- lm(Eq, data = kdat)
  cf = coef(mod)
  if (donorm) {
    cf <- cf - cf[1]
    se <- summary(mod)$coefficients[, 2]
    ci <- data.frame(rbind(
     VideoArmPlacebo = cf[2] + c(-1, 1) * (1.96 * se[2]),
     VideoArmTreatment = cf[3] + c(-1, 1) * (1.96 * se[3])))
  } else {
    ci = data.frame(confint(mod))
  }
  list(cf = cf, ci =ci)
}



######################################################################
######################### Diff and Diff ##############################
######################################################################
lincom <- function(EQ, mod) {
  lcom <- car::linearHypothesis(mod, EQ)
  t1 <- attributes(lcom)
  tdiff <- t1$value[1]
  tse <- sqrt(t1$vcov[1])
  tci <- tdiff + c(-1, 1) * (1.96 * tse)
  tpval <- lcom[2, 6]
  data.frame(est=tdiff, se=tse, 
    lb=tci[1], ub=tci[2],
    pval=tpval)
}

eqs <- list(
  ctrctr = "1*VideoArmControl:TreatList0 = 0",
  ctrtrt = "1*VideoArmControl:TreatList1 = 0",
  apcctr = "1*VideoArmPlacebo:TreatList0 = 0",
  apctrt = "1*VideoArmPlacebo:TreatList1 = 0",
  trtctr = "1*VideoArmTreatment:TreatList0 = 0",
  trttrt = "1*VideoArmTreatment:TreatList1 = 0",
  ctrdif = "1*VideoArmControl:TreatList1 - 1*VideoArmControl:TreatList0 = 0",
  apcdif = "1*VideoArmPlacebo:TreatList1 - 1*VideoArmPlacebo:TreatList0 = 0",
  trtdif = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 = 0",
  atteq = "1*VideoArmPlacebo:TreatList1 - 1*VideoArmPlacebo:TreatList0  -
     1*VideoArmControl:TreatList1 + 1*VideoArmControl:TreatList0  = 0",
  trteq = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 -
    1*VideoArmPlacebo:TreatList1 + 1*VideoArmPlacebo:TreatList0 = 0",
  toteq = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 -
    1*VideoArmControl:TreatList1 + 1*VideoArmControl:TreatList0 = 0")

#' @title doDiffReg
#' 
#' @description  Does the diff and diff analysis
#' 
#' @param  LHS
#' @param  dat
#' 
#' @return data.frame
#'
#' @export 
doDiffReg <- function(LHS, dat) {
  dat <- mutate(dat, 
    VideoArm = as.factor(VideoArm),
    TreatList = as.factor(TreatList))
  mod <- lm(as.formula(paste(LHS, "~ -1 + VideoArm:TreatList")), data=dat)
  data.frame(sapply(eqs, lincom, mod))
}

#' @title diffplot
#' 
#' @description  Plot diff and diff analysis for list experiments
#' 
#' @param  dat
#' @param  LHS
#' @param  ylim
#' 
#' @return  NULL
#'
#' @export 
diffPlot <- function(dat, LHS="", yLim, yvals=NULL, H=1) {
  nm <- names(dat); dat <- dat[[1]]
  y <- unlist(dat["est", c("ctrdif", "apcdif", "trtdif")]) * 100
  se <- unlist(dat["se", c("ctrdif", "apcdif", "trtdif")]) * 100
  toteq <- dat[["toteq"]]
  trteq <- dat[["trteq"]]
  plotrix::plotCI(1:3, y, se, cex.lab=1.2,
    bty="n", ylim=yLim, xaxt="n", xlab="", ylab="Prevalence ",
    lwd=3, pch=16, col=set3, font.lab=2, cex.axis=1.15)
  title(paste("This week I will", bwrap(nm, 34)), cex.main=1.3)
  axis(1, at=1:3, label=c("Control", "APC", "CoVideo"), cex.axis=1.2, font=2)
  text(x = c(1, 2, 2.80) + 0.025, y=y + 0.5, 
    label=formatC(y, format="f", digits=1),
    adj=c(0, 0), cex=1.0)
  abline(h=y[1], lwd=1, lty=2, col="grey70")
  pbrack(1, 3, yvals[1], h=H, CEX=1.0,
    pval=paste0("Total effect = ", 
      fmt(toteq$est*100, 1), ",\n 95% CI (", 
      fmt(toteq$lb*100, 1), ", ", 
      fmt(toteq$ub*100, 1), "), ", 
      pfmt(toteq$pval)))
  pbrack(2, 3, yvals[2], h=H, CEX=1.0,
    pval=paste0("Content effect = ", 
      fmt(trteq$est*100, 1), ",\n 95% CI (", 
      fmt(trteq$lb*100, 1), ", ", 
      fmt(trteq$ub*100, 1), "), ", 
      pfmt(trteq$pval)))
}

