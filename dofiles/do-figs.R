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
  
# Total enrolled
Total_In <- flow$In
# Lost at Consent
First_Lost <- flow$Start
# Total going into arms
ArmsN <- Total_In - First_Lost
CoVidN <- fdat_sum["Treatment"] + flow$Trt
APCN <- fdat_sum["Placebo"] + flow$APC
CtrlN <- ArmsN - CoVidN - APCN
# Lost at Crtl
Ctrl_Lost <- CtrlN - fdat_sum["Control"] 
# Total enrolled into Arms
ArmsN == (APCN + CtrlN + CoVidN)

# Checks 
(CoVidN - flow$Trt) == fdat_sum["Treatment"]
(APCN - flow$APC) == fdat_sum["Placebo"]
(CtrlN - Ctrl_Lost) == fdat_sum["Control"]

######################################################################
###################### Plots #########################################
######################################################################
pbrack <- function(x0, x1, y, h=0.01, pval="p < 0.05", ...) {
  segments(x0, y, x1, y, ...)
  segments(x0, y-h, x0, y, ...)
  segments(x1, y-h, x1, y, ...)
  text((x0 + x1)/2, y + (h*0.8), pval)
}
fmt <- function(x) {
  x <- formatC(x, format="f", digits=3)
  ifelse(x=="0.000", "p < 0.001", paste0("p = ", x))
}

dif1 <- function(x) {
  y <- list(cv = x[3]-x[1], ac = x[2]-x[1], av = x[3]-x[2])
  lapply(y, function(x) paste0("Diff = ", formatC(x, format="f", digits=3)))
}

######################################################################
######################### KNowledge ##################################
######################################################################
adat <- filter(dat_all, !is.na(ClinicTotal) | !is.na(SpreadTotal)) %>% 
  select(ClinicTotal, SpreadTotal, VideoArm)
  
cmod <- lm(ClinicTotal ~ -1 + VideoArm, data=adat)
cpair <- with(adat, pairwise.t.test(ClinicTotal, VideoArm, p.adj = "none"))
y <- cmod$coefficients
cis <- confint(cmod)
lis <- y - cis[, 1] 
uis <- cis[, 2] - y
pvals <- fmt(cpair$p.value)
difc <- dif1(y)

png(file.path(output, "Know_Clinic.png"),
  units="in", width=5, height=5.0, pointsize=9, 
  res=500, type="cairo")
plotCI(1:3, y, liw=lis, uiw=uis, main="Knowledge (Clinical)", 
  bty="n", ylim=c(9.15, 9.39), lwd=2, pch=16, font.lab=2,
  xlab="Trial arm", ylab="Mean score", xaxt="n")
axis(1, 1:3, c("Control", "APC", "CoVideo"))
pbrack(1, 2, 9.29, pval=paste0(difc$ac, ", ", pvals[1, 1]))
pbrack(2, 3, 9.34, pval=paste0(difc$av, ", ", pvals[2, 2]))
pbrack(1, 3, 9.38, pval=paste0(difc$cv, ", ", pvals[2, 1]))
dev.off()

cmod <- lm(SpreadTotal ~ -1 + VideoArm, data=adat)
cpair <- with(adat, pairwise.t.test(SpreadTotal, VideoArm, p.adj = "none"))
y <- cmod$coefficients
cis <- confint(cmod)
lis <- y - cis[, 1] 
uis <- cis[, 2] - y
pvals <- fmt(cpair$p.value)
difs <- dif1(y)

png(file.path(output, "Know_Spread.png"),
  units="in", width=5, height=5.0, pointsize=9, 
  res=500, type="cairo")
plotCI(1:3, y, liw=lis, uiw=uis, main="Knowledge (Spread)", 
  bty="n", ylim=c(8.40, 8.55), lwd=2, pch=16, font.lab=2,
  xlab="Trial arm", ylab="Mean score", xaxt="n")
axis(1, 1:3, c("Control", "APC", "CoVideo"))
pbrack(1, 2, 8.48, h=0.005, pval=paste0(difs$ac, ", ", pvals[1, 1]))
pbrack(2, 3, 8.51, h=0.005, pval=paste0(difs$av, ", ", pvals[2, 2]))
pbrack(1, 3, 8.54, h=0.005, pval=paste0(difs$cv, ", ", pvals[2, 1]))
dev.off()

######################################################################
######################### Behave Direct ##############################
######################################################################
# Function to wrap statements
bwrap <- function(y) paste(strwrap(bstate[y], 14), collapse="\n")

# Function to get means and cis
doReg <- function(LHS) {
    mod <- lm(as.formula(paste(LHS, " ~ TreatList")), data=bdat)
    cf <- round(coef(mod)[2]*100, 1)
    ci <- round(confint(mod)[2, ]*100, 1) 
    return(c(cf, ci))
}

# Make the data 
ldat <- getListData(dat_all)
bdat <- filter(ldat, VideoArm=="Control") 
behav = data.frame(t(sapply(names(bstate), doReg)))
behav <- arrange(behav, TreatList1)
names(behav) <- c("Est", "LB", "UB")
bnames <- lapply(rownames(behav), bwrap)

png(file.path(output, "BehavIntent.png"),
  units="in", width=6.5, height=5.0, pointsize=9, 
  res=500, type="cairo")
bp <- barplot(behav$Est, xaxt="n", ylim=c(0, 100),
  main="This week I will, ...", ylab="Prevalence", font.lab=2,
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
doReg2 <- function(LHS) {
  mod <- lm(as.formula(paste(LHS, "~ -1 + VideoArm:TreatList")), data=ldat)
  out <- data.frame(summary(mod)$coefficients[, c(1, 2)])
  colnames(out) <- c("Mn", "SE")
  out$Arm  <- gsub("VideoArm|:TreatF", "", rownames(out))
  out$Mn <- round(out$Mn, 3)
  out$SE  <- round(out$SE, 3)
  out[c(1, 4, 2, 5, 3, 6), ]
}


plotBar <- function(Var) {
  dat <- doReg2(Var)
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
# trtdat <- filter(ldat, VideoArm != "Control")
# totdat <- filter(ldat, VideoArm != "Placebo")

# mod1 <- lm(as.formula(paste("SocialDist", "~ -1 + VideoArm:TreatList")), data=trtdat)
# mod1s <- summary(mod1)
# mod1c <- coef(mod1s)
# mod2 <- lm(as.formula(paste("SocialDist", "~ VideoArm*TreatList")), data=trtdat)
# mod2s <- summary(mod2)
# mod2c <- coef(mod2s)
# mod3 <- lm(as.formula(paste("SocialDist", "~ VideoArm*TreatList")), data=totdat)
# mod3s <- summary(mod3)
# mod3c <- coef(mod3s)

# mod4 <- lm(as.formula(paste("SocialDist", "~ VideoArm*TreatList")), data=ldat)
# mod4s <- summary(mod4)
# mod4c <- coef(mod4s)

eqs <- list(
  ctrdif = "1*VideoArmControl:TreatList1 - 1*VideoArmControl:TreatList0 = 0",
  apcdif = "1*VideoArmPlacebo:TreatList1 - 1*VideoArmPlacebo:TreatList0 = 0",
  trtdif = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 = 0",
  toteq = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 -
    1*VideoArmControl:TreatList1 + 1*VideoArmControl:TreatList0 = 0",
  trteq = "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 -
    1*VideoArmPlacebo:TreatList1 + 1*VideoArmPlacebo:TreatList0 = 0")

lincom <- function(EQ) {
  lcom <- car::linearHypothesis(mod, EQ)
  t1 <- attributes(lcom)
  tdiff <- t1$value[1]
  tse <- sqrt(t1$vcov[1])
  tpval <- lcom[2, 6]
  out <- c(diff=tdiff, se=tse, pval=tpval)
  round(out, 4)
}

set3 <- RColorBrewer::brewer.pal(3, "Set2") 
diffPlot <- function(dat, LHS="") {
  y <- unlist(dat[1, 1:3])
  bp <- barplot(y, xaxt="n", ylim=c(0, 0.5),
    main=paste("This week I will", unlist(bstate[LHS])),
    ylab="Prevalence", font.lab=2, col=set3)
  text(x = bp, y=-0.01, label=c("Control", "APC", "CoVideo"), xpd=TRUE, 
       adj=c(0.5, 1), cex=1.1, srt=0)
  text(x = bp-0.05, y=y+0.01, label=y,
       adj=c(1, 0), offset=0.5, cex=0.8)
  plotCI(bp, y, dat[2, 1:3],
    add=TRUE, lwd=1, pch=16, col="gray30")
  pbrack(bp[1], bp[3], 0.40, pval=paste0("Diff = ", dat[1, "toteq"], ", pval = ", dat[3, "toteq"]))
  pbrack(bp[2], bp[3], 0.48, pval=paste0("Diff = ", dat[1, "trteq"], ", pval = ", dat[3, "trteq"]))
}

doDiffs <- function(LHS) {
  mod <- lm(as.formula(paste(LHS, "~ -1 + VideoArm:TreatList")), data=ldat)
  dat <- data.frame(lapply(eqs, lincom))
  diffPlot(dat, LHS)
  return(dat)
}
doDiffs("SocialDist")

car::linearHypothesis(mod4, trtdif, verbose=TRUE)


tt = car::linearHypothesis(mod4, 
  "1*VideoArmTreatment:TreatList1 - 1*VideoArmTreatment:TreatList0 -
  1*VideoArmControl:TreatList1 + 1*VideoArmControl:TreatList0 = 0")
