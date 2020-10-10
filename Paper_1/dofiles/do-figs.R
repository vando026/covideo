## Description: Do figures
## Project: CoVideo
## Author: AV / Created: 18Jul2020 

load(file.path(output, "Results.RData"))

######################################################################
######################### KNowledge ##################################
######################################################################

png(file.path(output, paste0("KnowledgeAll", ".png")),
  units="in", width=5, height=5.0, pointsize=9, 
  res=500, type="cairo")
plotKnow("KnowledgeAll", "COVID-19 Knowledge ", 
 yLim=c(16.8, 17.10), ppos=c(16.95, 17.02, 17.08), write=FALSE, 
 yaxt="n")
axis(2, at=seq(16.80, 17.10, by=0.10), las=1)
dev.off()




######################################################################
######################### Knowledge ##################################
######################################################################

kdat <- load_covideo()
cnames <- c(names(cstate()), names(sstate()))
allknow = append(cstate(), sstate())



doRegKnow <- function(var, donorm = FALSE) {
  answer <- allknow[[var]][2]
  kdat[var][kdat[var] == "TimedOut"] <- NA
  kdat[var] <- as.numeric(kdat[var] == answer)
  Eq <- as.formula(paste(var, "~ -1 + VideoArm"))
  mod <- lm(Eq, data = kdat)
  cf = coef(mod)
  if (donorm) {
    cf <- cf - cf[1]
    ci <- t(sapply(summary(mod)$coefficients[, 2], 
      function(x) x * c(-1.96, 1.96)))
  } else {
    ci = confint(mod)
  }
  list(cf = cf, ci =ci)
}

bindDat <- function(v) {
  out <- doRegKnow(v, donorm = FALSE)
  c(name = v, est=out$cf[[1]], lb=out$ci[1,1], ub=out$ci[1, 2])
}

sortDat <- function(dat) {
  arrange(dat, desc(est))
}

ctl <- plac <- trt  <- data.frame()
for (v in cnames) {
  out <- doRegKnow(v, donorm = FALSE)
  ctl <- bindDat(ctl, out)
  plac <- bindDat(plac, out)
  trt <- bindDat(trt, out)
}
}

lapply(cnames, bindDat)

kwrap <- function(y, len=14) {
  paste(strwrap(allknow[[y]][1], len), collapse="\n")
}

png(file.path(output, "forestknow.png"), width=14, height=9, 
    units="cm", type = "cairo-png", pointsize=7, res=300)
par(mar=c(5, 14, 4, 0.8) + 0.1, xpd = TRUE)
plot(x = ctl[[2]], y = seq(nrow(ctl)), xlim=c(0.8, 1), col=set3[2],
  yaxt="n", ylab="", xlab="", bty="n", pch=20)
lapply(seq(nrow(ctl)), 
  function(x) lines(ctl[x, 3:4],y=rep(x, 2), lwd=2, col=set3[2]))
lapply(seq(nrow(plac)), 
  function(x) lines(plac[x, 3:4],y=rep(x - 0.15, 2), lwd=2, col=set3[1]))
points(x = plac[[2]],y=seq(nrow(plac))-0.15, col=set3[1], pch=20)
lapply(seq(nrow(trt)), 
  function(x) lines(trt[x, 3:4],y=rep(x + 0.15, 2), lwd=2, col=set3[3]))
points(x = trt[[2]],y=seq(nrow(trt)) + 0.15, col=set3[3], pch=20)
title(xlab = "Percent correct")
lapply(seq(nrow(trt)), 
   function(x) text(x=0.72, y = x, kwrap(x, 40), cex=0.8, adj=0))
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
