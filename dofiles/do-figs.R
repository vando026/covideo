## Description: Do figures
## Project: CoVideo
## Author: AV / Created: 18Jul2020 

######################################################################
######################### CONSORT ####################################
######################################################################
cdat <- readr::read_csv(file.path(datapath, "Derived", "CONSORT.csv"))

getStats <- function(x) {
  dat <- cdat[grepl(x, cdat$Var), -1]
  sum(apply(dat, 1, function(x) sum(x, na.rm=TRUE)))
}
nms <- c("In", "Start", "Trt", "APC", "End")
flow <- lapply(setNames(nms, nms), getStats)
flow

fdat <- select(dat_all, ID, VideoArm, TreatList)
fdat <- data.frame(with(fdat, table(VideoArm, TreatList)))

# Total completed
sum(fdat$Freq)
fdat_sum <- tapply(fdat$Freq, fdat$VideoArm, sum)
fdat_sum
Final <- length(unique(dat_all$ID))
  
# Total enrolled
Total_In <- flow$In
Total_In

# Lost at Consent
First_Lost <- flow$Start

# Total going into arms
ArmsN <- Total_In - First_Lost
CoVidN <- fdat_sum["Treatment"] + flow$Trt
APCN <- fdat_sum["Placebo"] + flow$APC
CtrlN <- ArmsN - CoVidN - APCN
# Lost at Crtl
Ctrl_Lost <- CtrlN - fdat_sum["Control"] 
Ctrl_Lost

# Total enrolled into Arms
ArmsN == (APCN + CtrlN + CoVidN)
ArmsN

# Checks 
(CoVidN - flow$Trt) == fdat_sum["Treatment"]
(APCN - flow$APC) == fdat_sum["Placebo"]
(CtrlN - Ctrl_Lost) == fdat_sum["Control"]

######################################################################
######################### KNowledge ##################################
######################################################################
# cmod <- lm(ClinicTotalRC ~ VideoArm, data=adat)

plotKnow <- function(LHS, Title="", yLim, ppos, H=0.01) {
  cmod <- lm(as.formula(paste(LHS, " ~ -1 + VideoArm")),
     data=dat_all)
  cpair <- pairwise.t.test(dat_all[[LHS]], dat_all[["VideoArm"]],
     p.adj = "none")
  y <- cmod$coefficients
  cis <- confint(cmod)
  lis <- y - cis[, 1] 
  uis <- cis[, 2] - y
  pvals <- pfmt(cpair$p.value)
  #
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

plotKnow("ClinicTotal", "Clinical knowledge of COVID-19 (10 items)", 
  yLim=c(9.15, 9.39), ppos=c(9.29, 9.338, 9.38))

plotKnow("SpreadTotal", "Knowledge of preventing COVID-19 spread (8 items)", 
  yLim=c(7.62, 7.75), ppos=c(7.715, 7.695, 7.74), H=0.005)

plotKnow("KnowledgeAll", "All knowledge (18 items)", 
 yLim=c(16.8, 17.05), ppos=c(16.95, 17.005, 17.048))


######################################################################
######################### Behave Direct ##############################
######################################################################
# Make the data 
ldat <- getListData(dat_all)
bdat <- filter(ldat, VideoArm=="Control") 
behav = data.frame(t(sapply(names(bstate), doRegDirect, bdat)))
behav <- arrange(behav, TreatList1)
names(behav) <- c("Est", "LB", "UB")
bnames <- lapply(rownames(behav), bwrap)

png(file.path(output, "BehavIntent.png"),
  units="in", width=6.5, height=5.0, pointsize=10, 
  res=500, type="cairo")
bp <- barplot(behav$Est, xaxt="n", ylim=c(0, 100),
  main="This week, I will ...", ylab="Prevalence", font.lab=2,
  col="darkslategray3", border="darkslategray4")
text(x = bp, y=-1, label=bnames, xpd=TRUE, 
     adj=c(0.5, 1), cex=0.8, srt=0)
text(x = bp-0.05, y=behav$Est+1, label=behav$Est,
     adj=c(1, 0), offset=0.5, cex=0.8)
plotCI(bp, behav$Est, li=behav$LB, ui=behav$UB, 
  main="Behavioral Intent", bty="n", add=TRUE,
  ylim=c(0, 100), lwd=1, pch=16, 
  xlab="Trial arm", xaxt="n", col="gray30")
dev.off()

######################################################################
####################### PLot Behav means #############################
######################################################################

plotBar <- function(Var, Data=ldat) {
  dat <- doRegIndirect(Var, Data)
  nms <- rep(c("Control", "Treatment"), 3)
  bp <- barplot(dat$Mn, ylim=c(0, max(dat$Mn)*1.1), 
    ylab="Mean score", font.lab=2, main=paste("I will", unlist(bstate[Var])),
    col=c(rep("lightblue", 2), rep("darkseagreen2", 2), rep("bisque2", 2)))
  text(bp, y=-0.05, label=nms, xpd=TRUE, adj=c(0.5, 1),
    cex=1.1)
  text(bp, dat$Mn, label=formatC(dat$Mn, format="f", digits=2), 
       cex=0.9, pos=3)
}

mat <- matrix(c(1:6, 7, 7), 4, 2, byrow=TRUE)
png(file.path(output, "BehavMeans.png"),
  units="in", width=7, height=8.5, pointsize=11, 
  res=72, type="cairo")
nf <- layout(mat, heights=c(rep(6, 3), 1))
par(mai=c(0.4,0.3,0.3,0.1))
plotBar("SocialDist")
plotBar("Wash")
plotBar("StockPile")
plotBar("CleanDishes")
plotBar("CleanSurfaces")
plotBar("UseMedia")
par(mai=c(0.1,0.1,0.0,0.1))
plot.new()
legend("bottom", legend=c("No Video", "APC", "CoVideo"), bty="n", ncol=3, 
  cex=1.7, fill=c("lightblue", "darkseagreen2", "bisque2"), 
  text.width=0.15)
dev.off()

######################################################################
######################### Diff and Diff ##############################
######################################################################
ldat <- getListData(dat_all)

eqs <- list(
  ctrdif = "1*VideoArmControl:TreatList1 - 1*VideoArmControl:TreatList0 = 0",
  apcdif = "1*VideoArmPlacebo:TreatList1 - 1*VideoArmPlacebo:TreatList0 = 0",
  trtdif = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 = 0",
  toteq = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 -
    1*VideoArmControl:TreatList1 + 1*VideoArmControl:TreatList0 = 0",
  trteq = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 -
    1*VideoArmPlacebo:TreatList1 + 1*VideoArmPlacebo:TreatList0 = 0")
btitle <- lapply(setNames(names(bstate), names(bstate)), bwrap, 25)

diffPlot <- function(dat, LHS="", yLim, yvals=NULL) {
  nm <- names(dat); dat <- dat[[1]]
  y <- unlist(dat[1, 1:3]) * 100
  plotCI(1:3, y, dat[2, 1:3]*100, cex.lab=1.2,
    bty="n", ylim=yLim, xaxt="n", xlab="", ylab="Mean ",
    lwd=3, pch=16, col=set3, font.lab=2, cex.axis=1.15)
  title(paste("This week I will", unlist(btitle[nm])), cex.main=1.3)
  axis(1, at=1:3, label=c("Control", "APC", "CoVideo"), cex.axis=1.2, font=2)
  text(x = c(1, 2, 2.77) + 0.025, y=y + 0.5, 
    label=formatC(y, format="f", digits=1),
    adj=c(0, 0), cex=1.0)
  abline(h=y[1], lwd=1, lty=2, col="grey70")
  pbrack(1, 3, yvals[1], h=1, CEX = 1.1,
    pval=paste0("Tot. Diff = ", fmt(dat[1, "toteq"]*100, 1), ", pval = ", 
    fmt(dat[3, "toteq"], 3)))
  pbrack(2,  3, yvals[2], h=1, CEX = 1.1,
    pval=paste0("Trt. Diff = ", fmt(dat[1, "trteq"]*100, 1), ", pval = ", 
    fmt(dat[3, "trteq"], 3)))
}
ddat <- lapply(setNames(names(bstate), names(bstate)), doDiffs)

mat <- matrix(c(1:6), 3, 2, byrow=TRUE)
png(file.path(output, "BehavDiffs.png"),
  units="in", width=7, height=9.5, pointsize=12, 
  res=300, type="cairo")
nf <- layout(mat, heights=rep(10, 3))
par(mai=c(0.4,0.6,0.4,0.2))
diffPlot(ddat["SocialDist"], yLim=c(10, 42), yvals=c(40, 35))
diffPlot(ddat["Wash"], yLim=c(80, 104), yvals=c(102, 98))
diffPlot(ddat["StockPile"], yLim=c(20, 46), yvals=c(45, 41))
diffPlot(ddat["CleanDishes"], yLim=c(80, 104), yvals=c(102, 98))
diffPlot(ddat["CleanSurfaces"], yLim=c(65, 90), yvals=c(89, 85))
diffPlot(ddat["UseMedia"], yLim=c(15, 41), yvals=c(40, 36))
dev.off()

