
library(ggplot2)
library(dplyr)

### clunky data processing which is only run when the site is first accessed.

load(file = "data.Rdata")

# dat   <- read.csv("../../data/dsn_pull/rf-1990-2015.csv", header = TRUE, as.is = TRUE)
# sites <- read.csv("../../data/dsn_pull/rf_sites.csv", header = TRUE, as.is = TRUE )
# 
# dat$date <- as.Date(dat$Activity.Start.Date, format = "%m/%d/%Y")
# dat$Result.Value <- as.numeric(dat$Result.Value)
# dat_dt <- as.tbl(dat)
# # 
#  events <- unique( small_dat[ , names(dat) %in% c("Activity.ID", "date", "Monitoring.Location.ID", "Monitoring.Location.Name" ) ] )       
#  events <- as.tbl(events)
# # 
# events_by_sites2 <- filter(events,date >= as.Date("1/1/2000", format = "%m/%d/%Y") )  %>% group_by(Monitoring.Location.ID )%>% 
#      summarize( n_events = n_distinct(date),
#                 earliest = min(date), 
#                 latest = max(date)) %>% 
#      arrange( desc(n_events))
# 
# 
# main_sites <- filter(events_by_sites2, n_events > 99 ) %>% select(Monitoring.Location.ID)
# 
# 
# small_dat<- filter( dat_dt, Monitoring.Location.ID %in% main_sites$Monitoring.Location.ID & date >= as.Date("1/1/2000", format = "%m/%d/%Y")  ) 
# 
# save(small_dat, file = "data.Rdata")
# Monitoring.Location.ID %in% main_sites$Monitoring.Location.ID &  # removed from filter since only using select sites.
analytes <- filter(small_dat, date >= as.Date("1/1/2000", format = "%m/%d/%Y") ) %>% 
    group_by(Characteristic.Name) %>% 
    summarise(n = n(), min = min(Result.Value), max = max(Result.Value), n_zero = sum(Result.Value == 0 ), n_detects =  sum(Result.Value > 0 ) ) %>% 
    arrange(desc(n))
analytes <- filter(analytes, n > 150)
