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


save(tab1, file=file.path(output, "Results.RData"))

ldat <- getListData(load_covideo())
behav = data.frame(t(sapply(names(bstate()), getCTR, ldat)))
rownames(behav) <- sapply(rownames(behav), function(x) bstate()[[x]])

######################################################################
######################### Primary outcome ############################
######################################################################
# ldat <- getListData(dat_all)
# varn <- names(ldat)[-c(1,2)]
# tablist <- data.frame(do.call(rbind, 
  # lapply(setNames(varn, varn), getMeanSE, ldat)))

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


