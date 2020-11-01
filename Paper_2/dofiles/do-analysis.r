## Description: Do the analysis
## Project: COVID-19
## Author: AV / Created: 20May2020 

#####################################################################
######################## Demo   #####################################
#####################################################################
dat_all <- load_covideo()
getTot <- function(Var) {
  dat <- group_by(dat_all, VideoArm, TreatList, .data[[Var]]) %>% tally()
  group_by(dat, VideoArm, TreatList) %>% 
    mutate(Tot = sum(n), Perc=round(n/Tot * 100, 1)) %>% select(-Tot)
}

doStat <- function(Var) {
  browser()
  Var$n <- format(Var$n, big.mark=",")
  out <- pivot_wider(Var, names_from=c(VideoArm, TreatList), 
    values_from=c(n, Perc))
  data.frame(out[, c(1, unlist(Map(c, 2:7, 8:13)))])
}

dat1 <- lapply(c("Age", "Gender", "Country", "Educ2", "Language"), 
  getTot)
tab1 <- lapply(dat1, doStat)


######################################################################
######################### Direct Behave comparisons ##################
######################################################################
bdat <- getBehavData() %>% filter(TreatList == 0)

doBehavReg <- function(Var, dat = bdat) {
  mod <- lm(as.formula(paste(Var, '~ VideoArm')), data = dat)
  summary(mod)
}

bnames <- names(bdat)[grepl("^BE", names(bdat))][bnames != "BEUseMedia"]
lapply(setNames(bnames, bnames), doBehavReg)


save(tab1, file=file.path(output, "Results.RData"))


