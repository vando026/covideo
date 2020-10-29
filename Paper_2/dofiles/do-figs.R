## Description: Do figures
## Project: CoVideo
## Author: AV / Created: 18Jul2020 

######################################################################
####################### PLot Behav means #############################
######################################################################
ldat <- getListData()
ddat <- lapply(setNames(names(bstate()), names(bstate())), doDiffReg, ldat)

plotBar <- function(Var, dat=ddat) {
  dat <- unlist(ddat[[Var]]["est", 1:6])
  nms <- rep(c("Ctrl", "Trt"), 3)
  bp <- barplot(dat, ylim=c(0, max(dat)*1.1), names.arg="", 
    ylab="Mean ", font.lab=2, cex.axis=1.1,  cex.lab=1.2,
    main=paste("This week, I will", bwrap(Var, 25)), cex.main=1.2,
    col=c(
      c(adjustcolor(set3[1], alpha.f=0.5), set3[1]),
      c(adjustcolor(set3[2], alpha.f=0.5), set3[2]),
      c(adjustcolor(set3[3], alpha.f=0.5), set3[3])))
  text(bp, y=-0.05, label=nms, xpd=TRUE, adj=c(0.5, 1), cex=1.0)
  text(bp, dat, label=formatC(dat, format="f", digits=2), 
       cex=1.1, pos=3)
}

mat <- matrix(c(1:6, 7, 7), 4, 2, byrow=TRUE)
png(file.path(output, "BehavMeans.png"),
  units="in", width=6, height=8, pointsize=11,
  res=200, type="cairo-png")
nf <- layout(mat, heights=c(rep(6, 3), 1))
par(mai=c(0.4,0.52,0.3,0.1))
plotBar("SocialDist")
plotBar("Wash")
plotBar("StockPile")
plotBar("CleanDishes")
plotBar("CleanSurfaces")
plot.new()
legend("topleft", legend=c("Do-nothing", "APC", "CoVideo"), 
  bty="n", ncol=1, cex=1.1, fill=c(set3[1], set3[2], set3[3]), 
  text.width=0.15)
dev.off()

######################################################################
######################### Behave Indirect ##############################
######################################################################
getEst <- function(Var, dat=ddat)
  unlist(dat[[Var]]["ctrdif"])
behav <- data.frame(t(sapply(names(ddat), getEst)))
behav <- behav[!(rownames(behav) %in% "UseMedia"), ]
behav[1:4] <- lapply(behav[1:4], function(x) round(x*100, 1))
behav <- arrange(behav, ctrdif.est)
bnames <- lapply(rownames(behav), bwrap)


png(file.path(output, "BehavIntent.png"),
  units="in", width=6.5, height=5.0, pointsize=10, 
  res=500, type="cairo")
  bp <- barplot(behav$ctrdif.est, xaxt="n", ylim=c(0, 100),
    main="This week, I will ...", ylab="Prevalence", font.lab=2,
    col="darkslategray3", border="darkslategray4")
  text(x = bp, y=-1, label=unlist(bnames), xpd=TRUE, 
       adj=c(0.5, 1), cex=0.8, srt=0)
  text(x = bp-0.05, y=behav$ctrdif.est+1, label=behav$ctrdif.est,
       adj=c(1, 0), offset=0.5, cex=0.8)
  plotCI(bp, behav$ctrdif.est, li=behav$ctrdif.lb, ui=behav$ctrdif.ub, 
    main="Behavioral Intent", bty="n", add=TRUE,
    ylim=c(0, 100), lwd=1, pch=16, 
    xlab="Trial arm", xaxt="n", col="gray30")
dev.off()



######################################################################
######################### Diff and Diff ##############################
######################################################################
mat <- matrix(c(1:6), 3, 2, byrow=TRUE)
png(file.path(output, "BehavDiffs.png"),
  units="in", width=8, height=11.3, pointsize=12, 
  res=300, type="cairo")
nf <- layout(mat, heights=rep(10, 3))
par(mai=c(0.4,0.6,0.8,0.2))
diffPlot(ddat["SocialDist"], yLim=c(14, 42), yvals=c(40, 35), H=1.4)
diffPlot(ddat["Wash"], yLim=c(82, 104), yvals=c(102, 98), H=1.2)
diffPlot(ddat["StockPile"], yLim=c(22, 48), yvals=c(46, 41), H=1.3)
diffPlot(ddat["CleanDishes"], yLim=c(81, 104), yvals=c(102, 97), H=1.1)
diffPlot(ddat["CleanSurfaces"], yLim=c(67, 93), yvals=c(90, 85), H=1.2)
dev.off()

######################################################################
######################### List Intent ################################
######################################################################
plotList <- function(LHS, yLim=c(0, 1.0), dat=load_covideo()) {

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
       xlab="Age (years) ", main=paste("This week I will ", bwrap(LHS, 30)), font.lab=2, xaxt="n")
  lines(ksmooth(x, mcoef_age, "normal", bandwidth=2, n.points=200), lwd=2.0, lty=1, col=set3[3])
  lines(ksmooth(x, fcoef_age, "normal", bandwidth=2, n.points=200), lwd=2.0, lty=2, col=set3[2])
  axis(1, x,  gsub("(Age)|years", "", names(mcoef_age)))
}

png(file.path(output, paste0("List_", "1", ".png")),
  units="in", width=6, height=8, pointsize=10, 
  res=300, type="cairo")
  par(mfrow=c(3, 2))
  plotList("SocialDist")
  plotList("StockPile")
  plotList("CleanDishes")
  plotList("CleanSurfaces")
  plotList("Wash")
  plot.new()
  legend("topleft", legend=c("Men", "Women"), lty=c(1, 2), col=c(set3[3], set3[2]), 
         ncol=1, bty="n", lwd=2, cex=1.2, seg.len=3 )
dev.off()

save(behav, ddat, file=file.path(output, "ResultsFigs.RData"))



######################################################################
######################### Behave Direct ##############################
######################################################################
# Get baseline measure of behav intent, remove social desire
# ldat <- getListData(load_covideo())
# bdat <- filter(ldat, VideoArm=="Control") 
# behav = data.frame(t(sapply(names(bstate()), doRegDirect, bdat)))
# names(behav) <- c("Est", "LB", "UB")
# behav <- arrange(behav, Est) 
# behav <- behav[!(rownames(behav) %in% "UseMedia"), ]
# bnames <- lapply(rownames(behav), bwrap)
  

# png(file.path(output, "BehavIntent.png"),
#   units="in", width=6.5, height=5.0, pointsize=10, 
#   res=500, type="cairo")
#   bp <- barplot(behav$Est, xaxt="n", ylim=c(0, 100),
#     main="This week, I will ...", ylab="Percentage", font.lab=2,
#     col="darkslategray3", border="darkslategray4")
#   text(x = bp, y=-1, label=unlist(bnames), xpd=TRUE, 
#        adj=c(0.5, 1), cex=0.8, srt=0)
#   text(x = bp-0.05, y=behav$Est+1, label=behav$Est,
#        adj=c(1, 0), offset=0.5, cex=0.8)
#   plotCI(bp, behav$Est, li=behav$LB, ui=behav$UB, 
#     main="Behavioral Intent", bty="n", add=TRUE,
#     ylim=c(0, 100), lwd=1, pch=16, 
#     xlab="Trial arm", xaxt="n", col="gray30")
# dev.off()

