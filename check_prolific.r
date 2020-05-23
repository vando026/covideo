dedat <- readProA("prolific_export_5ebce4433d396b13dc87bd82.csv")
dat1 <- select(dedat, participant_id, TimeMin,  time_taken)


endat <- readProA("prolific_export_5ebf7bdeb587fe363d3e683b.csv")
dat1 <- select(endat, participant_id, TimeMin,  time_taken)
arrange(dat1, TimeMin)
