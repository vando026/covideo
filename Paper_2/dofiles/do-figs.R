## Description: Do figures
## Project: CoVideo
## Author: AV / Created: 18Jul2020 

load(file.path(output, "Results.RData"))

######################################################################
######################### Behave Direct ##############################
######################################################################
# Get baseline measure of behav intent, remove social desire
ldat <- getListData(load_covideo())
bdat <- filter(ldat, VideoArm=="Control") 
behav = data.frame(t(sapply(names(bstate()), doRegDirect, bdat)))
names(behav) <- c("Est", "LB", "UB")
behav <- arrange(behav, Est) 
behav <- behav[!(rownames(behav) %in% "UseMedia"), ]
bnames <- lapply(rownames(behav), bwrap)
  

png(file.path(output, "BehavIntent.png"),
  units="in", width=6.5, height=5.0, pointsize=10, 
  res=500, type="cairo")
  bp <- barplot(behav$Est, xaxt="n", ylim=c(0, 100),
    main="This week, I will ...", ylab="Percentage", font.lab=2,
    col="darkslategray3", border="darkslategray4")
  text(x = bp, y=-1, label=unlist(bnames), xpd=TRUE, 
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
ldat <- getListData(load_covideo())
plotBar <- function(Var, Data=ldat) {
  dat <- doRegIndirect(Var, Data)
  nms <- rep(c("Ctrl", "Trt"), 3)
  bp <- barplot(dat$Mn, ylim=c(0, max(dat$Mn)*1.1), 
    ylab="Mean ", font.lab=2, cex.axis=1.1,  cex.lab=1.3,
    main=paste("I will", bwrap(Var, 35)), cex.main=1.3,
    col=c(
      c(adjustcolor(set3[1], alpha.f=0.5), set3[1]),
      c(adjustcolor(set3[2], alpha.f=0.5), set3[2]),
      c(adjustcolor(set3[3], alpha.f=0.5), set3[3])))
  text(bp, y=-0.05, label=nms, xpd=TRUE, adj=c(0.5, 1),
    cex=1.0, srt=0)
  text(bp, dat$Mn, label=formatC(dat$Mn, format="f", digits=2), 
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
legend("topleft", legend=c("No Video", "APC", "CoVideo"), bty="n", ncol=1, 
  cex=1.3, fill=c(set3[1], set3[2], set3[3]), 
  text.width=0.15)
# par(mai=c(0.1,0.1,0.0,0.1))
dev.off()

######################################################################
######################### Diff and Diff ##############################
######################################################################
ldat <- getListData(load_covideo())
ddat <- lapply(setNames(names(bstate()), names(bstate())), doDiffs, ldat)

mat <- matrix(c(1:6), 3, 2, byrow=TRUE)
png(file.path(output, "BehavDiffs.png"),
  units="in", width=8, height=12, pointsize=12, 
  res=300, type="cairo")
nf <- layout(mat, heights=rep(10, 3))
par(mai=c(0.4,0.6,0.8,0.2))
diffPlot(ddat["SocialDist"], yLim=c(10, 42), yvals=c(40, 35), H=1.3)
diffPlot(ddat["Wash"], yLim=c(80, 104), yvals=c(102, 98), H=1.3)
diffPlot(ddat["StockPile"], yLim=c(22, 48), yvals=c(46, 41), H=1.3)
diffPlot(ddat["CleanDishes"], yLim=c(80, 104), yvals=c(102, 98))
diffPlot(ddat["CleanSurfaces"], yLim=c(65, 93), yvals=c(90, 85), H=1.2)
dev.off()

