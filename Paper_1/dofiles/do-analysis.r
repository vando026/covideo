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
flow$CtrlN; flow$APCN; flow$CoVidN
tapply(dat_all$ID, dat_all$VideoArm, function(x) length(unique(x)))

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
######################### Video data #################################
######################################################################
vdat <- haven::read_dta(file.path(datapath, "Derived/VideoTime_ID.dta"))
vdat <- filter(vdat, VideoTime_category != 4)
leftwatch <- alainr::dx(vdat$ID)[2]
watched <- 1 - prop.table(table(vdat$VideoTime_category))[1]
vdat <- filter(vdat, VideoTime_category != 1)
less150 <-  prop.table(table(vdat$VideoTime_category))[1]
vdat <- mutate(vdat, VideoTime = ifelse(VideoTime > 150, 150, VideoTime))
meanwatch <- mean(vdat$VideoTime)

save(tab0, flow, res, tabc, tabs, res_know, res_spr, res_all,
  reg_clinic, reg_clinic_trt, reg_spread, reg_spread_trt, dat_trt_n, 
  reg_all, reg_all_trt, agediff, agediffs, agediffa, watched, less150,
  leftwatch, meanwatch, file=file.path(output, "Results.RData"))

