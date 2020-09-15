## Description: Do the analysis
## Project: COVID-19
## Author: AV / Created: 20May2020 

######################################################################
######################### CONSORT ####################################
######################################################################
# Total sompleted by arms
cdat <- readr::read_csv(file.path(datapath, "Derived", "CONSORT.csv"))
fdat <- select(load_covideo(), ID, VideoArm, TreatList)
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
tabc <- data.frame(t(sapply(names(cstate()), getTot1, cstate())))
tabc$Label <- names(cstate())
rownames(tabc) <- unlist(Map(paste0,
  c(seq(length(cstate()))), c(". "),  
  sapply(names(cstate()), function(x) cstate()[[x]][1])))
tabs <- data.frame(t(sapply(names(sstate()), getTot1, sstate())))
tabs$Label <- names(sstate())
rownames(tabs) <- unlist(Map(paste0,
  c(seq(length(cstate()) + 1, length(c(cstate(), sstate())))), c(". "),  
  sapply(names(sstate()), function(x) sstate()[[x]][1])))

res_know <- plotKnow("ClinicTotal", plt=FALSE)
res_spr <- plotKnow("SpreadTotal", plt=FALSE)
res_all <- plotKnow("KnowledgeAll", plt=FALSE)

dat_ctrl <- filter(dat_all, VideoArm=="Control")
dat_trt <- filter(dat_all, VideoArm=="Treatment")
dat_tot <- filter(dat_all, VideoArm!="Placebo")
dat_apc <- filter(dat_all, VideoArm!="Control")
dat_trt_n = nrow(dat_trt)
dat_ctrl_n = nrow(dat_ctrl)

reg_clinic <- doRegKnow("ClinicTotal", dat_ctrl)
reg_clinic_trt <- doRegKnow("ClinicTotal", dat_trt)
reg_spread <- doRegKnow("SpreadTotal", dat_ctrl)
reg_spread_trt <- doRegKnow("SpreadTotal", dat_trt)
reg_all <- doRegKnow("KnowledgeAll", dat_ctrl)
reg_all_trt <- doRegKnow("KnowledgeAll", dat_trt)

######################################################################
######################################################################
######################################################################
# Shows difference in knowlegde by age

modc <- lm(ClinicTotal ~ - 1 + Age:VideoArm, data=dat_tot)
mods <- lm(SpreadTotal ~ - 1 + Age:VideoArm, data=dat_tot)
moda <- lm(KnowledgeAll ~ - 1 + Age:VideoArm, data=dat_tot)

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
agediffa <- data.frame(
  sapply(setNames(c(c1824, c2534, c3544, c4554, c5559), cnm), lincom, moda))

modc <- lm(ClinicTotal ~ - 1 + Age:VideoArm, data=dat_apc)
mods <- lm(SpreadTotal ~ - 1 + Age:VideoArm, data=dat_apc)
moda <- lm(KnowledgeAll ~ - 1 + Age:VideoArm, data=dat_apc)
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
agediffa_apc <- data.frame(
  sapply(setNames(c(c1824, c2534, c3544, c4554, c5559), cnm), lincom, moda))


save(tab0, flow, res, tabc, tabs, res_know, res_spr, res_all,
  reg_clinic, reg_clinic_trt, reg_spread, reg_spread_trt, dat_trt_n, 
  reg_all, reg_all_trt, agediff, agediffs, agediffa, 
  file=file.path(output, "Results.RData"))

######################################################################
######################### Video data #################################
######################################################################
