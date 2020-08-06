## Description: Do the analysis
## Project: COVID-19
## Author: AV / Created: 20May2020 

######################################################################
######################### CONSORT ####################################
######################################################################
# Total sompleted by arms
cdat <- readr::read_csv(file.path(datapath, "Derived", "CONSORT.csv"))
fdat <- select(dat_all, ID, VideoArm, TreatList)
fdat <- data.frame(with(fdat, table(VideoArm, TreatList)))
fdat_sum <- tapply(fdat$Freq, fdat$VideoArm, sum)
fdat_sum

getStats <- function(x) {
  dat <- cdat[grepl(x, cdat$Var), -1]
  sum(apply(dat, 1, function(x) sum(x, na.rm=TRUE)))
}
nms <- c("In", "Start", "Trt", "APC", "End")
flow <- lapply(setNames(nms, nms), getStats)
flow

# Total completed
flow$FinalN <- sum(fdat_sum)

# Total going into arms
flow$ArmsN <- flow$In - flow$Start
flow$CoVidN <- fdat_sum["Treatment"] + flow$Trt
flow$APCN <- fdat_sum["Placebo"] + flow$APC
flow$CtrlN <- flow$In - flow$Start  - flow$CoVidN - flow$APCN - 17 #dropped because of missing 
flow$Ctrl <- flow$CtrlN - fdat_sum["Control"]


######################################################################
######################### Demo   #####################################
######################################################################
getTot <- function(Var) {
  dat <- group_by(dat_all, VideoArm, .data[[Var]]) %>% tally()
  group_by(dat, VideoArm) %>% 
    mutate(Tot = sum(n), Perc=round(n/Tot * 100, 1)) %>% select(-Tot)
}

doStat <- function(Var) {
  Var$n <- format(Var$n, big.mark=",")
  out <- pivot_wider(Var, names_from=VideoArm, 
    values_from=c(n, Perc))
  data.frame(out[, c(1, unlist(Map(c, 2:4, 5:7)))])
}

dat1 <- lapply(c("Age", "Gender", "Country", "Educ2", "Language"), 
  getTot)
tab0 <- lapply(dat1, doStat)

res <- list()
res$dates <- format(c(min(dat_all$Date), max(dat_all$Date)), "%d %B %Y")
dtab <- function(x) round(prop.table(table(x))*100,1)
res$lan <- dtab(dat_all$Language)
res$ctry <- dtab(dat_all$Country)
res$educ <- dtab(dat_all$Educ2)

######################################################################
######################### Knowledge ##################################
######################################################################
getTot1 <- function(x, state) {
  dat <- dat_all[c("VideoArm", x)]
  correct <- state[[x]][2]
  yy  <- ifelse(correct=="False", "True", "False")
  dat[[x]][dat[[x]]=="TimedOut"] <- yy
  xx <- table(dat)
  xt <- prop.table(xx, 1)
  xx <- format(xx, big.mark=",")
  xt <- xt[, colnames(xt) == correct]
  xt <- formatC(xt*100, 1, format="f")
  xx <- data.frame(cbind(xx, pp=xt))
  xx <- data.frame(xx[1, ], xx[2, ], xx[3, ])
  xx
}
tabclinic <- data.frame(t(sapply(names(cstate), getTot1, cstate)))
tabclinic$Label <- names(cstate)
rownames(tabclinic) <- unlist(Map(paste0,
  c(seq(length(cstate))), c(". "),  
  sapply(names(cstate), function(x) cstate[[x]][1])))
tabspread <- data.frame(t(sapply(names(sstate), getTot1, sstate)))
tabspread$Label <- names(sstate)
rownames(tabspread) <- unlist(Map(paste0,
  c(seq(length(sstate))), c(". "),  
  sapply(names(sstate), function(x) sstate[[x]][1])))

# save(tab0, tabclinic, tabspread, 
#   file=file.path(datapath, "Derived", "Tables.Rdata"))

res_know <- plotKnow("ClinicTotal", plt=FALSE)
res_spr <- plotKnow("SpreadTotal", plt=FALSE)
res_all <- plotKnow("KnowledgeAll", plt=FALSE)

######################################################################
######################### Get behav in Ctrl ##########################
######################################################################
bstate2  <- bstate
bstate2["UseMedia"] <- 
  "This week I wil seek health information from animated videos made by health experts"
ldat <- getListData(dat_all)
behav = data.frame(t(sapply(names(bstate), getCTR, ldat)))
rownames(behav) <- sapply(rownames(behav), function(x) bstate2[[x]])


######################################################################
######################### Primary outcome ############################
######################################################################
ldat <- getListData(dat_all)
varn <- names(ldat)[-c(1,2)]
tablist <- data.frame(do.call(rbind, 
  lapply(setNames(varn, varn), getMeanSE, ldat)))



######################################################################
######################### Diff #######################################
######################################################################
ldat <- getListData(dat_all)
ddat <- lapply(setNames(names(bstate), names(bstate)), doDiffs, ldat)

pstar <- function(x)  {
  if (x < 0.01) return("*")
  if (x < 0.05) return("**")
  if (x < 0.001) return("***")
}

diffTab <- function(dat) {
  est <- sapply(dat[1, 1:5], 
    function(x) formatC(x*100, 2, format="f"))
  se <- sapply(dat[2, 1:3], 
    function(x) paste0("(", formatC(x*100, 2, format="f"), ")"))
  r1 <- c(paste(est[1], se[1]), paste(est[2], se[2]), paste(est[3], se[3]))
  difp <- c(paste0(est[4], pstar(dat[3, 4])), paste0(est[5], pstar(dat[3, 5])))
  c(r1, difp)
}
difftable <- do.call(rbind, lapply(ddat, diffTab))


######################################################################
######################### ############################################
######################################################################
# # debugonce(doEffects)
# socdist <- doEffects("SocialDist")
# doEffects("Wash")

# APCCtrlX =  filter(ldat2, VideoArm=="Placebo" & TreatList==0)  %>% summarize(mean(SocialDist))
# APCTrtX =  filter(ldat2, VideoArm=="Placebo" & TreatList==1)  %>% summarize(mean(SocialDist))
# VidCtrlX =  filter(ldat2, VideoArm=="Treatment" & TreatList==0)  %>% summarize(mean(SocialDist))
# VidTreatX = filter(ldat2, VideoArm=="Treatment" & TreatList==1) %>% summarize(mean(SocialDist))
# tapply(ldat2$SocialDist, list(ldat2$VideoArm, ldat2$TreatList), mean)


# mod = lm(SocialDist ~ VideoArm*TreatList, data=ldat2)
# summary(mod)
# coefs <- coef(mod) 
# APCCtrl_ = coefs["(Intercept)"]
# APCTrt_ <- coefs["(Intercept)"] + coefs["TreatList"]
# VidCtrl_ <- coefs["(Intercept)"] + coefs["VideoArmTreatment"]
# VidTrt_ <- coefs["(Intercept)"] + coefs["VideoArmTreatment"] + coefs["TreatList"] + coefs["VideoArmTreatment:TreatList"]

# lapply(ls(pattern="_$"), function(x) get(x))
# lapply(ls(pattern="X$"), function(x) get(x))

# # Difference in difference
# coefs["VideoArmTreatment:TreatList"]
# (VidTreatX - VidCtrlX) - (APCTrtX - APCCtrlX)
# tapply(ldat$SocialDist, list(ldat$VideoArmF, ldat$TreatListF), sd)

# debugonce(doEffects)
# doEffects("CleanDishes")

# bedat <- select(dat_all, ID, VideoArm, TreatList, starts_with("BE"))
# bedat <- mutate(bedat, 
#   VideoArm = as.factor(VideoArm), TreatList = as.factor(TreatList))
# trt_bedat <- filter(bedat, VideoArm != "Control")

# mod = glm(BESocialDist ~ VideoArm*TreatList, data=trt_bedat, family=poisson(link="log"))
# summary(mod)$coefficients

# doEffects("SocialDist", dat=bedat, model=glm, family=poisson)



# adat <- filter(dat_all, !is.na(ClinicTotal1) | !is.na(SpreadTotal1)) %>% 
#   select(ClinicTotal1, SpreadTotal1, VideoArm)

