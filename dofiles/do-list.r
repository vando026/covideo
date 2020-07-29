## Description: Do list analysis
## Project: COVID
## Author: AV / Created: 15May2020 

# Set a seed for reproducibility
set.seed(123) 

# Define subject types.

# Truthfully respond "Yes" to direct question
N.trueadmitter <- 500

# Falsely respond "No" to direct question
N.withholder <- 500

# Truthfully respond "No" to direct question
N.innocent <- 500

type <- rep(c("TA", "WH", "IN"), times=c(N.trueadmitter, N.withholder, N.innocent))


D <- ifelse(type=="TA", 1, 0)
direct.est <- mean(D)
direct.est
## [1] 0.3333333


N <- length(type)
# Generate list response potential outcomes

# Control potential outcome
Y0 <- sample(1:4, N, replace=TRUE)

# Treated potential outcome is 1 higher for true admitters and withholders
Y1 <- Y0 + ifelse(type %in% c("TA", "WH"), 1, 0)

# Conduct random assignment
Z <- rbinom(N, 1, 0.5)

# Reveal list responses
Y <- Z*Y1 + (1-Z)*Y0

list.est <- mean(Y[Z==1]) - mean(Y[Z==0])
list.se <- sqrt((var(Y[Z==1])/sum(Z) + var(Y[Z==0])/sum(1-Z)))
list.est
## [1] 0.619082
list.se
## [1] 0.05910645


data(race)

set.seed(1)

diff.in.means.results <- ictreg(y ~ 1, data = race, 
  treat = "treat", J=3, method = "lm")


res <- ictreg(SocialDist ~ 1, data=data.frame(dat_en),
  treat = "Treat", J=5, method= "lm")
