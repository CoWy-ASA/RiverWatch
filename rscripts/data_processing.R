### consolidate data processing to create a single script that can be sourced to create the following data objects
# full_dat - full dataset queried from Colorado Data Sharing Network (CDSN)
# small_dat - most common sites and analytes - currently 27 analytes collected at sites after 1999 with data from at least 10 years.
# main_sites
# events - defined by unique locations and dates
# events_by_sites - pivot table
# 


## save as R objects for faster loading

library(dplyr)

dat   <- read.csv("data/dsn_pull/rf-1990-2015.csv", header = TRUE, as.is = TRUE)
sites <- read.csv("data/dsn_pull/rf_sites.csv", header = TRUE, as.is = TRUE )

dat$date <- as.Date(dat$Activity.Start.Date, format = "%m/%d/%Y")
dat$Result.Value <- as.numeric(dat$Result.Value)

## create an analyte name the combines total and dissolved info.

dat$analyte <- dat$Characteristic.Name
ii <- dat$Sample.Fraction != ""
dat$analyte[ii] <- paste(dat$Characteristic.Name[ii], "_", dat$Sample.Fraction[ii], sep = "" )


dat_dt <- as.tbl(dat)

### removed empty columns

ii <- apply(dat_dt, 2, function(x){sum(!is.na(x))})  > 0## identify columns without data >0
dat_dt <- dat_dt[, ii]

full_dat <- dat_dt

events <- unique( dat[ , names(dat) %in% c("Activity.ID", "date", "Monitoring.Location.ID", "Monitoring.Location.Name", "Monitoring.Location.Latitude", "Monitoring.Location.Longitude" ) ] )       
events$year <- as.numeric(format(events$date, "%Y") ) 
events <- as.tbl(events)

events_after2000 <- filter(events,date >= as.Date("1/1/2000", format = "%m/%d/%Y") ) 

## eliminate sites with fewer than 25 samples
       
       events_by_sites <-  events_after2000 %>% group_by(Monitoring.Location.ID, Monitoring.Location.Name, Monitoring.Location.Latitude, Monitoring.Location.Longitude )%>% 
           summarize( n_events = n_distinct(date),
                      earliest = min(date), 
                      latest = max(date),
                      n_years = length(unique(year ))) %>% 
           arrange( desc(n_events))
       
       ## select sites with 10 or more years of data.

       main_sites <- filter(events_by_sites, n_years > 10 ) #%>% select(Monitoring.Location.ID)
       
       
       ### examine date range with plot
      
       
       analytes <- filter(dat_dt, Monitoring.Location.ID %in% main_sites$Monitoring.Location.ID & date >= as.Date("1/1/2000", format = "%m/%d/%Y") ) %>% 
           group_by(analyte) %>% 
           summarise(n = n(), min = min(Result.Value), max = max(Result.Value), n_zero = sum(Result.Value == 0 ), n_detects =  sum(Result.Value > 0 ) ) %>% 
           arrange(desc(n))
       
### remove analytes seldom sampled

analytes <- filter(analytes, n > 200)  ## note this combines total and dissolved

### smaller data set

small_dat <- filter(dat_dt, analyte %in% analytes$analyte & Monitoring.Location.ID %in% main_sites$Monitoring.Location.ID & date >= as.Date("1/1/2000", format = "%m/%d/%Y")   )
 

### data cleaning

### create vector with all column names except Result.Value and Result.UID
grp_cols <- names(small_dat)[! names(small_dat) %in% c("Result.Value", "Result.UID")]
dots <- lapply(grp_cols, as.symbol)

# Perform frequency counts
small_dat %>% group_by_(.dots=dots) %>% summarise(n = length(Result.Value), min = min(Result.Value), max = max(Result.Value), dl = max(Detection.Quantitation.Limit.Value1))  -> new_dat  ## note the underscore in group_by

new_dat$flag <- NA
new_dat$flag[new_dat$n > 1] <- 777
new_dat$Result.Value <- new_dat$max

## hard coded error changes

i <- new_dat$analyte == "pH" & (new_dat$Result.Value > 14 | new_dat$Result.Value < 5 )
#new_dat$Result.Value[i] <- NA
#new_dat$flag[i] <- 888
new_dat <- new_dat[!i, ]




small_dat <- new_dat


save(list = c("small_dat", "main_sites", "events", "events_by_sites", "full_dat"),file = "data/binaryDat.Rdata")

## save also to shiny file
save(list = c("small_dat", "main_sites", "events", "events_by_sites", "full_dat"),file = "rscripts/shinyPlot/binaryDat.Rdata")
