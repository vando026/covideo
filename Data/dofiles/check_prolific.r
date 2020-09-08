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
readr::write_csv(pdat, path=file.path(datapath, "Prolific/EN_All.csv"))

######################################################################
###### Reject ########### ############################################
######################################################################
reviewStatus("EN_5May2020.csv", "en")
reviewStatus("EN_5June2020.csv", "en")
reviewStatus("DE_14May2020.csv", "de")
reviewStatus("MX_23May2020.csv", "mx")
reviewStatus("SP_23May2020.csv", "sp")

# Note to pay EN from 1:


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

######################################################################
######################### 5 June #####################################
######################################################################
may16 <- readProA("EN_16May2020.csv")
may16 = select(may16, participant_id, status, reviewed_at_datetime)
jun5 = read_delim(file.path(datapath, "Prolific/en_reject.txt"), delim="\t")
jun5$Return = 1
dat = left_join(jun5, may16)
dat$Date = format(dat$reviewed_at_datetime, "%d-%b-%Y")
dat = filter(dat, status=="APPROVED")
dat = select(dat, participant_id)
write_delim(data.frame(dat), path=file.path(datapath, "Prolific/return.txt"))
