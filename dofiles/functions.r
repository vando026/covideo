## Description: Master file for Covid analysis
## Project:  CoVideo
## Author: AV / Created: 13May2020 

# Read Prolific data
readProA <- function(name, fpath=file.path(datapath, "Prolific")) {
  dat <- read_csv(file.path(fpath, name))
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
  ddat <- select(ddat, participant_id)
  readr::write_delim(ddat, path=file.path(datapath, "Prolific", paste0(ln, "_reject.txt")))

  endat <- filter(endat, TimeMin > 1)
  message("\nTo be paid")
  print(table(endat$status))
  pdat <- select(endat, participant_id)
  readr::write_delim(pdat, path=file.path(datapath, "Prolific", paste0(ln, "_pay.txt")))
}

# Read and clean data
clean <- function(name, ctype=col_character) {
  message(sprintf("==> Reading %s ", name))
  dat <- suppressMessages(
    read_csv(file.path(sourced, name), comment="END OF FILE", 
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

# Get dem data
getDem <- function(name) {
  message(sprintf("==> Reading %s ", name))
  dat <- suppressMessages(
    read_csv(file.path(sourced, name), comment="END OF FILE"))
  dat <- select(dat, 
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

# Get video end data
getVid <- function(name) {
  message(sprintf("==> Reading %s ", name))
  dat <- suppressMessages(
    read_csv(file.path(sourced, name), comment='END OF FILE'))
  dat <- select(dat, 
    ID=`Participant Public ID`, 
    Stamp=`UTC Timestamp`, 
    Label=`Zone Name`,
    Time=`Reaction Time`)
  dat <- filter(dat, !is.na(Label))
  dat <- arrange(dat, ID, Stamp)
  dat <- group_by(dat, ID) %>% 
    mutate(
      Lag = lag(Stamp), 
      Time = round((Stamp - Lag)/1000, 2)) %>% 
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
  message(sprintf("==> Reading %s rows from %s\n", nrow(dat), sourced))
  dat
}

######################################################################
#########################  Functions #################################
######################################################################
# Function to wrap statements
bwrap <- function(y, len=14) 
  paste(strwrap(bstate[y], len), collapse="\n")


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

######################################################################
######################### Behav intent ###############################
######################################################################
getListData <- function(dat=dat_all) {
  ldat <- select(dat_all, VideoArm, TreatList, 
    SocialDist, Wash, StockPile, CleanDishes, CleanSurfaces, UseMedia)
  mutate(ldat, 
    VideoArm = as.factor(VideoArm), TreatList = as.factor(TreatList))
}



lmMN <- mkMod(RHS = " ~ -1 + VideoArm:TreatList")
glmMN <- mkMod(model=glm, RHS = " ~ -1 + VideoArm:TreatList", 
  family=poisson)
lmDiffDiff <- mkMod(RHS = " ~ VideoArm*TreatList")
glmDiffDiff <- mkMod(model=glm, RHS = " ~ VideoArm*TreatList", 
  family=poisson)
lmDiff <- mkMod(RHS = "~ VideoArm")
glmDiff <- mkMod(model=glm, RHS = "~ VideoArm", 
  family=poisson)

doEffects <- function(getMn, getDiff, getDiffDiff) {
  function(y, dat=ldat, model) {
    # For now focus on Treatment effect = Total - Attention effects
    trt_dat <- filter(dat, VideoArm != "Control")
    trt_dat$VideoArm <- droplevels(trt_dat$VideoArm)
    trt_dat2 <- filter(trt_dat, TreatList==1)
    # For now focus on Total effect = Total - Control effects
    tot_dat <- filter(dat, VideoArm != "Placebo")
    tot_dat$VideoArm <- droplevels(tot_dat$VideoArm)
    tot_dat2 <- filter(tot_dat, TreatList==1)

    cat("\n-------------------------- Total Effects -----------------------------------\n")
    # Get Means
    cat("\n===> Shows the means and standard errors of each trial condition\n")
    mntot = getMn(y, tot_dat)
    print(mntot)

    # Total Effects
    cat("\n===> Shows the total effect for TreatList, CoVid vs Control\n")
    ddat <- getDiff(y, tot_dat2)
    print(ddat)

    cat("\n===> Shows difference in difference for total effect, CoVid vs Control\n")
    did_tot <- getDiffDiff(y, tot_dat)
    print(did_tot)

    cat("\n --------------------------- Treatment Effect --------------------------------\n")

    cat("\n===> Shows the means and standard errors of each trial condition\n")
    mntrt = getMn(y, trt_dat)
    print(mntrt)

    cat("\n===> Shows the treatment effect for TreatList, CoVid vs APC\n")
    tdat <- getDiff(y, trt_dat2)
    print(tdat)

    cat("\n===> Shows difference in difference for treatment effect, CoVid vs APC\n")
    did_trt <- getDiffDiff(y, trt_dat)
    print(did_trt)

    # invisible(list(means=mns, treatdiff=tdat, totdiff=ddat, diff_trt=did_trt, diff_tot=did_tot))
  }
}

# Run linear models only on list questionsll
lmEffects <- doEffects(lmMN, lmDiff, lmDiffDiff)
# Run poisson models on direct behavioral questions
glmEffects <- doEffects(glmMN, glmDiff, glmDiffDiff)



######################################################################
######################### Plots ######################################
######################################################################
pbrack <- function(x0, x1, y, h=0.01, pval="p < 0.05", CEX=1, ...) {
  segments(x0, y, x1, y, ...)
  segments(x0, y-h, x0, y, ...)
  segments(x1, y-h, x1, y, ...)
  text((x0 + x1)/2, y + (h*0.8), pval, cex=CEX)
}

pfmt <- function(x) {
  x <- formatC(x, format="f", digits=3)
  ifelse(x=="0.000", "p < 0.001", paste0("p = ", x))
}

fmt <- function(x, y=2) formatC(x, format="f", digits=y)

# Function to get means and cis for direct behav intent
doRegDirect <- function(LHS, dat) {
    mod <- lm(as.formula(paste(LHS, " ~ TreatList")), data=dat)
    cf <- round(coef(mod)[2]*100, 1)
    ci <- round(confint(mod)[2, ]*100, 1) 
    return(c(cf, ci))
}

doRegIndirect <- function(LHS, dat) {
  mod <- lm(as.formula(paste(LHS, "~ -1 + VideoArm:TreatList")), data=dat)
  out <- data.frame(summary(mod)$coefficients[, c(1, 2)])
  colnames(out) <- c("Mn", "SE")
  out$Arm  <- gsub("VideoArm|:TreatF", "", rownames(out))
  out$Mn <- round(out$Mn, 3)
  out$SE  <- round(out$SE, 3)
  out[c(1, 4, 2, 5, 3, 6), ]
}

getCTR <- function(LHS, dat) {
  xx <- doRegIndirect(LHS, dat)
  xx <- cbind(xx[1, 1:2], xx[2, 1:2])
  colnames(xx) <- c("Ctr_Mn", "Ctr_SE", "Trt_Mn", "Trt_SE")
  xx
}

plotKnow <- function(LHS, Title="", yLim, ppos, H=0.01, plt=TRUE) {
  cmod <- lm(as.formula(paste(LHS, " ~ -1 + VideoArm")),
     data=dat_all)
  cpair <- pairwise.t.test(dat_all[[LHS]], dat_all[["VideoArm"]],
     p.adj = "none")
  y <- cmod$coefficients
  cis <- confint(cmod)
  lis <- y - cis[, 1] 
  uis <- cis[, 2] - y
  se <- summary(cmod)$coefficients[, "Std. Error"]
  pvals <- pfmt(cpair$p.value)
  #
  if (plt) {
    png(file.path(output, paste0(LHS, ".png")),
      units="in", width=5, height=5.0, pointsize=9, 
      res=500, type="cairo")
    plotCI(1:3, y, liw=lis, uiw=uis, main=Title, 
      bty="n", ylim=yLim, lwd=3, pch=16, font.lab=2,
      xlab="Trial arm", ylab="Mean score", xaxt="n", col=set3)
    axis(1, 1:3, c("Control", "APC", "CoVideo"))
    text(c(1,2,2.8), y, pos=4, labels=formatC(y, format="f", digits=2))
    pbrack(1, 2, ppos[1], H,
      pval=paste0("Att. Diff = ", fmt(y[2] - y[1]), ", ", pvals[1, 1]))
    pbrack(2, 3, ppos[2], H,
      pval=paste0("Trt. Diff = ", fmt(y[3] - y[2]), ", ", pvals[2, 2]))
    pbrack(1, 3, ppos[3], H,
      pval=paste0("Tot. Diff = ", fmt(y[3] - y[1]), ", ", pvals[2, 1]))
    dev.off()
  }
  return(list(means=y, se=se, cis=cis, pvals=pvals))
}


# Regressions for knowledge
doRegKnow <- function(RHS, dat) {
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


######################################################################
######################### Diff and Diff ##############################
######################################################################
lincom <- function(EQ, mod, dig=2) {
  lcom <- car::linearHypothesis(mod, EQ)
  t1 <- attributes(lcom)
  tdiff <- t1$value[1]
  tse <- sqrt(t1$vcov[1])
  tpval <- lcom[2, 6]
  c(diff=fmt(tdiff, dig), se=fmt(tse, dig), pval=pfmt(tpval))
}

doDiffs <- function(LHS, dat=ldat) {
  mod <- lm(as.formula(paste(LHS, "~ -1 + VideoArm:TreatList")), data=dat)
  data.frame(lapply(eqs, lincom, mod))
}
