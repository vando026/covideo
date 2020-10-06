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
 yLim=c(16.8, 17.10), ppos=c(16.95, 17.005, 17.06), write=FALSE, 
 yaxt="n")
axis(2, at=seq(16.80, 17.10, by=0.10), las=1)
dev.off()




