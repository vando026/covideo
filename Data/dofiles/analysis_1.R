## Description: Analysis_1
## Project: CoVideo
## Author: AV / Created: 08Jun2020 


ldat <- select(dat_all, VideoArm, Treat, StockPile:UseMedia)

calcListGroup <- function(x) 
  tapply(as.matrix(x), list(ldat$Treat), mean)

calcListArm <- function(x) 
  tapply(as.matrix(x), list(ldat$VideoArm, ldat$Treat), mean)

nm <- c(
  "SocialDist",
  "Wash",
  "StockPile",
  "CleanSurfaces",
  "ShareUtensils",
  "UseMedia")

calcList(ldat$StockPile)
lapply(setNames(ldat[-c(1:2)], nm), calcListGroup)
lapply(setNames(ldat[-c(1:2)], nm), calcListArm)
