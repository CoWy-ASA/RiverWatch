### file for reading and rearranging Excel file data.
rm(list= ls() )

library(XLConnect)
library(reshape2)
library(dplyr)


files <- dir("data/roaring_fork_excel/", include.dirs = TRUE, full.names = TRUE)

## open and read each file
## reading is as character to deal with dates and times.
dimCheck <- 0
stations<- fields <- metals <- nutrs <- NULL

for( i in 1:length(files)){
    
    wb <- loadWorkbook(filename = files[i] )

    # print(    getSheets(wb) )  # check wbs have same sheets

stations <- rbind( stations, readWorksheet(object = wb, sheet = "Stations" ) )
nutrs  <- rbind(nutrs, readWorksheet(object = wb, sheet = "Nutrients", colTypes = "character" ) )
fields <- rbind(fields, readWorksheet(object = wb, sheet = "Field", colTypes = "character" ) )
print(nrow(fields))

metals <- rbind( metals, readWorksheet(object = wb, sheet = "Metals",  colTypes = "character" ) )

}

## correct typo

names(stations)[ 6] <- "Longitude"
stations  <- stations[-c( 33, 77:81), ]  ## remove extra rows plus station without lat/long.

## create data class
i <- is.na(nutrs$Time)
nutrs$Time[i] <- "XXXX-XX-XX-00:00:00"  ## assign missing times the to 00:00:00 
nutrs$date <- paste(substr(nutrs$Date, start = 1, stop = 10), substr(x = nutrs$Time, start = 12, 19) )

# nutrs$date <- strptime(nutrs$date, format = "%Y-%m-%d %H:%M:%S")
nuts_tall <- melt(nutrs, id.vars = c("date", "Event.", "Stn."), measure.vars = c("Ammonia", "Chloride", "Nitrate.Nitrite", "Sulfate", "Tot.N", "Tot.P", "TSS"),  variable.name = "analyte", value.name = "result", na.rm = TRUE)
nuts_tall$sample_fraction <- NA
nuts_tall$sample_fraction <- as.character(nuts_tall$sample_fraction)

nuts_tall$analyte <- as.character(nuts_tall$analyte)

##  fix field data

i <- is.na(fields$Time)
fields$Time[i] <- "XXXX-XX-XX-00:00:00"  ## assign missing times the to 00:00:00 
fields$date <- paste(substr(fields$Date, start = 1, stop = 10), substr(x = fields$Time, start = 12, 19) )

# fields$date <- strptime(fields$date, format = "%Y-%m-%d %H:%M:%S")
fields_tall <- melt(fields, id.vars = c("date", "Event.", "Org.Name", "Stn."), measure.vars = c("PH", "X.deg.C",  "Phen.Alk", "Tot.Alk",  "Tot.Hard", "DO" ),  variable.name = "analyte", value.name = "result", na.rm = TRUE)

## fix metal data

i <- is.na(metals$Time)
metals$Time[i] <- "XXXX-XX-XX-00:00:00"  ## assign missing times the to 00:00:00 
metals$date <- paste(substr(metals$Date, start = 1, stop = 10), substr(x = metals$Time, start = 12, 19) )

# metals$date <- strptime(metals$date, format = "%Y-%m-%d %H:%M:%S")

metals <- metals[, !names(metals)%in% c("Date", "Time") ]  ## drop old date and time values


metals_tall <- melt(metals, id.vars = c("date", "Event.", "Stn."),  variable.name = "analyte", value.name = "result", na.rm = TRUE)

metals_tall$analyte <- as.character(metals_tall$analyte)
metals_tall$sample_fraction <- substr(metals_tall$analyte, start = 4, stop = 6)
metals_tall$analyte <- substr(metals_tall$analyte, start = 1, stop = 2)

####

### create event ta


## distinct(fields, Org.Name)  ## crashes R session :<
## resolved by keeping data as character

events <- unique(fields[, names(fields) %in% c( "Event.", "Org.Name", "Stn.", "date" )] )

wq_data <- bind_rows( metals_tall, nuts_tall, fields_tall )

### convert date to date class

wq_data$date <- as.POSIXct( strptime( wq_data$date, format = "%Y-%m-%d %H:%M:%S") )
events$date <- as.POSIXct( strptime( events$date, format = "%Y-%m-%d %H:%M:%S") )

if(FALSE){  ## only overwrite files with a reason - then recommit.
    
write.table(wq_data, file = "data/processed/wq_data.csv", sep = ",", row.names = FALSE )
write.table(events, file = "data/processed/events.csv", sep = ",", row.names = FALSE )
write.table(stations, file = "data/processed/stations.csv", sep = ",", row.names = FALSE )

}

### write files in csv format