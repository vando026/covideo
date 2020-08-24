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


#####################################################################
######################## Demo   #####################################
#####################################################################
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
tabc <- data.frame(t(sapply(names(cstate), getTot1, cstate)))
tabc$Label <- names(cstate)
rownames(tabc) <- unlist(Map(paste0,
  c(seq(length(cstate))), c(". "),  
  sapply(names(cstate), function(x) cstate[[x]][1])))
tabs <- data.frame(t(sapply(names(sstate), getTot1, sstate)))
tabs$Label <- names(sstate)
rownames(tabs) <- unlist(Map(paste0,
  c(seq(length(sstate))), c(". "),  
  sapply(names(sstate), function(x) sstate[[x]][1])))

res_know <- plotKnow("ClinicTotal", plt=FALSE)
res_spr <- plotKnow("SpreadTotal", plt=FALSE)
res_all <- plotKnow("KnowledgeAll", plt=FALSE)

dat_ctrl <- filter(dat_all, VideoArm=="Control")
dat_trt <- filter(dat_all, VideoArm=="Treatment")
dat_tot <- filter(dat_all, VideoArm!="Placebo")
dat_apc <- filter(dat_all, VideoArm!="Control")
reg_clinic <- doRegKnow("ClinicTotal", dat_ctrl)
reg_clinic_trt <- doRegKnow("ClinicTotal", dat_trt)
reg_spread <- doRegKnow("SpreadTotal", dat_ctrl)
reg_spread_trt <- doRegKnow("SpreadTotal", dat_trt)


# group_by(dat_ctrl, Country) %>% summarize(NN = mean(ClinicTotal))
# group_by(dat_trt, Country) %>% summarize(NN = mean(ClinicTotal))
# lm(ClinicTotal ~ -1 + Country, data=dat_ctrl)

######################################################################
######################### Get behav in Ctrl ##########################
######################################################################
# Get baseline measure of behav intent, remove social desire
ldat <- getListData(dat_all)
bdat <- filter(ldat, VideoArm=="Control") 
behav = data.frame(t(sapply(names(bstate), doRegDirect, bdat)))
behav <- arrange(behav, TreatList1)
names(behav) <- c("Est", "LB", "UB")
bnames <- lapply(rownames(behav), bwrap)

# ldat <- getListData(dat_all)
# behav = data.frame(t(sapply(names(bstate), getCTR, ldat)))
# rownames(behav) <- sapply(rownames(behav), function(x) bstate2[[x]])

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
# ldat <- getListData(dat_all)
# ddat <- lapply(setNames(names(bstate), names(bstate)), doDiffs, ldat)

# pstar <- function(x)  {
#   if (x < 0.01) return("*")
#   if (x < 0.05) return("**")
#   if (x < 0.001) return("***")
# }

# diffTab <- function(dat) {
#   browser()
#   est <- sapply(dat[1, 1:5], 
#     function(x) formatC(x*100, 2, format="f"))
#   se <- sapply(dat[2, 1:3], 
#     function(x) paste0("(", formatC(x*100, 2, format="f"), ")"))
#   r1 <- c(paste(est[1], se[1]), paste(est[2], se[2]), paste(est[3], se[3]))
#   difp <- c(paste0(est[4], pstar(dat[3, 4])), paste0(est[5], pstar(dat[3, 5])))
#   c(r1, difp)
# }
# difftable <- do.call(rbind, lapply(ddat, diffTab))


modc <- lm(ClinicTotal ~ - 1 + Age:VideoArm, data=dat_tot)
mods <- lm(SpreadTotal ~ - 1 + Age:VideoArm, data=dat_tot)

c1824 = "1*Age18-24 years:VideoArmTreatment - 1*Age18-24 years:VideoArmControl = 0"
c2534 = "1*Age25-34 years:VideoArmTreatment - 1*Age25-34 years:VideoArmControl = 0"
c3544 = "1*Age35-44 years:VideoArmTreatment - 1*Age35-44 years:VideoArmControl = 0"
c4554 = "1*Age45-54 years:VideoArmTreatment - 1*Age45-54 years:VideoArmControl = 0"
c5559 = "1*Age55-59 years:VideoArmTreatment - 1*Age55-59 years:VideoArmControl = 0"
cnm <- ls(pattern="^c[1-9]") 
agediff <- data.frame(
  sapply(setNames(c(c1824, c2534, c3544, c4554, c5559), cnm), lincom, modc))
agediffs <- data.frame(
  sapply(setNames(c(c1824, c2534, c3544, c4554, c5559), cnm), lincom, mods))

modc <- lm(ClinicTotal ~ - 1 + Age:VideoArm, data=dat_apc)
mods <- lm(SpreadTotal ~ - 1 + Age:VideoArm, data=dat_apc)
c1824 = "1*Age18-24 years:VideoArmTreatment - 1*Age18-24 years:VideoArmPlacebo = 0"
c2534 = "1*Age25-34 years:VideoArmTreatment - 1*Age25-34 years:VideoArmPlacebo = 0"
c3544 = "1*Age35-44 years:VideoArmTreatment - 1*Age35-44 years:VideoArmPlacebo = 0"
c4554 = "1*Age45-54 years:VideoArmTreatment - 1*Age45-54 years:VideoArmPlacebo = 0"
c5559 = "1*Age55-59 years:VideoArmTreatment - 1*Age55-59 years:VideoArmPlacebo = 0"
cnm <- ls(pattern="^c[1-9]") 
agediff_apc <- data.frame(
  sapply(setNames(c(c1824, c2534, c3544, c4554, c5559), cnm), lincom, modc))
agediffs_apc <- data.frame(
  sapply(setNames(c(c1824, c2534, c3544, c4554, c5559), cnm), lincom, mods))


save(tab0, flow, res, tabc, tabs, res_know, res_spr, res_all,
  reg_clinic, reg_clinic_trt, reg_spread, reg_spread_trt, behav,
  cstate, sstate, bstate, agediff, agediffs,
  file=file.path(output, "Results.RData"))


######################################################################
######################### List Intent ################################
######################################################################

plotList <- function(LHS, yLim=c(0, 1.0), dat=dat_all) {

  dat_ctrl <- filter(dat_all, VideoArm=="Control")
  dat_ctrl <- mutate(dat_ctrl, 
    Treat  = as.numeric(dat_ctrl$ListArm=="List Treatment"),
    EducNum = as.numeric(as.factor(Educ2)))
  fdat <- filter(dat_ctrl, Gender=="Female")
  mdat <- filter(dat_ctrl, Gender=="Male")


  fmod <- ictreg(as.formula(paste(LHS, "~ -1 + Age")), data=data.frame(fdat),
    treat = "Treat", J=5, method= "lm")
  mmod <- ictreg(as.formula(paste(LHS, "~ -1 + Age")), data=data.frame(mdat),
    treat = "Treat", J=5, method= "lm")

  fres <- summary(fmod)
  fcoefs <- fres$par.treat
  fcoef_age <- fcoefs[grepl("Age", names(fcoefs))] 
  print(fres)

  # ndat <- data.frame(Age = sort(unique(dat_all$Age)), EducNum=3)
  # predict(fmod, ndat)

  mres <- summary(mmod)
  print(mres)
  mcoefs <- mres$par.treat
  mcoef_age <- mcoefs[grepl("Age", names(mcoefs))] 
  x <- seq(length(fcoefs))

  plot(x, fcoef_age, ylim=yLim, bty="l", type="n", ylab="Estimated proportion",
       xlab="Age ", main=paste("This week I will ", bwrap(LHS, 30)), font.lab=2, xaxt="n")
  lines(ksmooth(x, mcoef_age, "normal", bandwidth=2, n.points=200), lwd=2.0, lty=1, col=set3[3])
  lines(ksmooth(x, fcoef_age, "normal", bandwidth=2, n.points=200), lwd=2.0, lty=2, col=set3[2])
  axis(1, x,  gsub("(Age)", "", names(mcoef_age)))
  legend("bottom", legend=c("Men", "Women"), lty=c(1, 2), col=c(set3[3], set3[2]), 
         ncol=2, bty="n", lwd=3 )
}

png(file.path(output, paste0("List_", "1", ".png")),
  units="in", width=7, height=8, pointsize=10, 
  res=500, type="cairo")
par(mfrow=c(3, 2))
plotList("SocialDist")
plotList("StockPile")
plotList("UseMedia")
plotList("CleanDishes")
plotList("CleanSurfaces")
plotList("Wash")
dev.off()

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

