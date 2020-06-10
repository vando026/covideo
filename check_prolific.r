######################################################################
######################### Prolific ###################################
######################################################################
# This gets all prolific data
getProData <- function(File) {
  dat <- readProA(File)
  dat <- select(dat, participant_id, status, TimeMin, age, `Current Country of Residence`)
  mutate(dat, Date=File)
}

pdat <- rbind(
  getProData("EN_13May2020.csv"),
  getProData("EN_16May2020.csv"),
  getProData("EN_5June2020.csv")
)


pdat <- mutate(pdat, Dup = duplicated(participant_id))
pdat <- arrange(pdat, participant_id, Date)
readr::write_csv(pdat, path=file.path(home, "Prolific/EN_All.csv"))





######################################################################
###### Reject ########### ############################################
######################################################################
reviewStatus("EN_5May2020.csv", "en")
reviewStatus("EN_10June2020.csv", "en")
reviewStatus("DE_14May2020.csv", "de")
reviewStatus("MX_23May2020.csv", "mx")
debugonce(reviewStatus)
reviewStatus("MX_23May2020_2.csv", "mx")
reviewStatus("SP_23May2020.csv", "sp")

Note to pay EN from 1:


######################################################################
######################### Pay ########################################
######################################################################
endat <- readProA("EN_5May2020.csv")

table(is.na(endat$completed_date_time))
endat <- endat[!is.na(endat$completed_date_time), ]

# endat <- arrange(endat, TimeMin) %>% 
#   select(participant_id, TimeMin, status, contains("_date_time"))
  
ddat <- filter(endat, TimeMin <=1)
ddat <- filter(ddat, status == "AWAITING REVIEW")

ddat <- select(ddat, participant_id)
