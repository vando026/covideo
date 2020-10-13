## Description: Do figures
## Project: CoVideo
## Author: AV / Created: 18Jul2020 

######################################################################
######################### KNowledge ##################################
######################################################################
png(file.path(output, paste0("KnowledgeAll", ".png")),
  units="in", width=5, height=5.0, pointsize=9, 
  res=500, type="cairo")
know_res <- plotKnow("KnowledgeAll", "COVID-19 Knowledge ", 
 yLim=c(16.8, 17.10), ppos=c(16.95, 17.02, 17.08), write=FALSE, 
 yaxt="n")
axis(2, at=seq(16.80, 17.10, by=0.10), las=1)
dev.off()

######################################################################
######################### Knowledge ##################################
######################################################################
kdat <- load_covideo()
cnames <- c(names(cstate()), names(sstate()))

bindDat <- function(Arm, Norm=FALSE) {
  function(v) {
    out <- doRegKnow(v, donorm = Norm)
    cbind(name = v, est=out$cf[[Arm]], ci = out$ci[Arm, ])
  }
}

sortDat <- function(dat) arrange(dat, est)
ctl <- sortDat(do.call(rbind, lapply(cnames, bindDat("VideoArmControl"))))
plac <- sortDat(do.call(rbind, lapply(cnames, bindDat("VideoArmPlacebo"))))
trt <- sortDat(do.call(rbind, lapply(cnames, bindDat("VideoArmTreatment"))))


png(file.path(output, "forestknow.png"), width=14, height=17, 
    units="cm", type = "cairo-png", pointsize=7, res=300)
par(mar=c(5, 19, 4, 0.8) + 0.1, xpd = TRUE)
plot(x = ctl[[2]], y = seq(nrow(ctl)), xlim=c(0.8, 1), col=set3[3],
  yaxt="n", ylab="", xlab="", bty="n", pch=20)
lapply(seq(nrow(ctl)), 
  function(x) lines(ctl[x, 3:4],y=rep(x, 2), lwd=2, col=set3[3]))
lapply(seq(nrow(plac)), 
  function(x) lines(plac[x, 3:4],y=rep(x - 0.15, 2), lwd=2, col=set3[2]))
points(x = plac[[2]],y=seq(nrow(plac))-0.15, col=set3[2], pch=20)
lapply(seq(nrow(trt)), 
  function(x) lines(trt[x, 3:4],y=rep(x + 0.15, 2), lwd=2, col=set3[1]))
points(x = trt[[2]],y=seq(nrow(trt)) + 0.15, col=set3[1], pch=20)
title(xlab = "Percent correct")
lapply(seq(nrow(trt)), 
   function(x) text(x=0.66, y = x, kwrap(x, 50), cex=0.9, adj=0))
legend("bottomright", c("E-E Video", "APC", "Control"), lty=1, col=set3,
   bty="n", lwd=2, ncol=1, cex=1.0)
dev.off()

plac0 <- sortDat(do.call(rbind, lapply(cnames, bindDat("VideoArmPlacebo", Norm=TRUE))))
trt0 <- sortDat(do.call(rbind, lapply(cnames, bindDat("VideoArmTreatment", Norm=TRUE))))

png(file.path(output, "forestknow0.png"), width=18, height=16, 
    units="cm", type = "cairo-png", pointsize=7, res=300)
par(mar=c(5, 20, 4, 0.8) + 0.1, xpd = TRUE)
plot(x = plac0[[2]], y = seq(nrow(plac0)), xlim=c(-0.05, 0.05), col=set3[2],
  yaxt="n", ylab="", xlab="", bty="n", pch=20, main="Knowledge items normalized to control")
lapply(seq(nrow(plac0)), 
  function(x) lines(plac0[x, 3:4],y=rep(x, 2), lwd=2, col=set3[2]))
lapply(seq(nrow(trt0)), 
  function(x) lines(trt0[x, 3:4],y=rep(x + 0.15, 2), lwd=2, col=set3[1]))
points(x = trt0[[2]],y=seq(nrow(trt0)) + 0.15, col=set3[1], pch=20)
title(xlab = "Percent correct")
lapply(seq(nrow(trt0)), 
   function(x) text(x=-0.095, y = x, kwrap(x, 50), cex=0.9, adj=0))
legend("bottomright", c("E-E Video", "APC"), lty=1, col=set3[c(1, 2)],
   bty="n", lwd=2, ncol=1, cex=1.2)
dev.off()




######################################################################
######################### Bar ########################################
######################################################################
colnames(ctl) <- c("name", "est", "lb", "ub")
ctl <- arrange(ctl, (est))

png(file.path(output, "barknow.png"), width=15, height=15, 
    units="cm", type = "cairo-png", pointsize=7, res=300)
par(mar=c(5, 22, 4, 0.8) + 0.1, xpd = TRUE)
bar <- barplot(ctl$est, horiz=TRUE, xlim=c(0, 1), 
  col=set3[1], xpd=FALSE, border=NA, xlab="Proportion correctly answered (with 95% CIs)",
main = "COVID-19 Knowledge", width=2)
lapply(seq(nrow(trt)), 
   function(x) text(x=-0.80, y = bar[x], kwrap(x, 65), cex=0.85, adj=0))
lapply(seq(nrow(ctl)), 
  function(x) lines(ctl[x, 3:4],y=rep(bar[x], 2), lwd=1, col="black"))
dev.off()



######################################################################
######################### results ####################################
######################################################################
save(know_res, file=file.path(output, "ResultsFigs.RData"))
