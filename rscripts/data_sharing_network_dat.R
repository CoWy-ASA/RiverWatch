### look at data from data sharing network
### mjp

library(ggplot2)

dat   <- read.csv("data/dsn_pull/rf-1990-2015.csv", header = TRUE, as.is = TRUE)
sites <- read.csv("data//dsn_pull/rf_sites.csv", header = TRUE, as.is = TRUE )

dat$date <- as.Date(dat$Activity.Start.Date, format = "%m/%d/%Y")
dat$Result.Value <- as.numeric(dat$Result.Value)
dat_dt <- as.tbl(dat)

select(dat_dt, distinct(as.character(Monitoring.Location.ID) )

#events <- unique(fields[, names(fields) %in% c( "Event.", "Org.Name", "Stn.", "date" )] )


events <- unique( dat[ , names(dat) %in% c("Activity.ID", "date", "Monitoring.Location.ID", "Monitoring.Location.Name" ) ] )       
events <- as.tbl(events)


       events_by_sites2 <- group_by( events, Monitoring.Location.ID )%>% 
           summarize( n_events = n_distinct(date),
                      earliest = min(date), 
                      latest = max(date)) %>% 
           arrange( desc(n_events))

## eliminate sites with fewer than 25 samples

events_by_sites2 <- filter(events,date >= as.Date("1/1/2000", format = "%m/%d/%Y") )  %>% group_by(Monitoring.Location.ID )%>% 
    summarize( n_events = n_distinct(date),
               earliest = min(date), 
               latest = max(date)) %>% 
    arrange( desc(n_events))


main_sites <- filter(events_by_sites2, n_events > 25 ) %>% select(Monitoring.Location.ID)


### examine date range

filter(events, Monitoring.Location.ID %in% main_sites$Monitoring.Location.ID) %>% ggplot( aes(x = date, y = Monitoring.Location.Name )) + geom_point()

### after viewing, focus on date collected after 2000 more than 25 events
filter(events, Monitoring.Location.ID %in% main_sites$Monitoring.Location.ID & date >= as.Date("1/1/2000", format = "%m/%d/%Y") ) %>% 
    ggplot( aes(x = date, y = Monitoring.Location.Name )) + geom_point()

### so what is measured during these events?


### filter dat for only main sites and data after 2000

analytes <- filter(dat_dt, Monitoring.Location.ID %in% main_sites$Monitoring.Location.ID & date >= as.Date("1/1/2000", format = "%m/%d/%Y") ) %>% 
    group_by(Characteristic.Name) %>% 
    summarise(n = n(), min = min(Result.Value), max = max(Result.Value), n_zero = sum(Result.Value == 0 ), n_detects =  sum(Result.Value > 0 ) ) %>% 
    arrange(desc(n))


filter(dat_dt, Monitoring.Location.ID %in% main_sites$Monitoring.Location.ID & date >= as.Date("1/1/2000", format = "%m/%d/%Y") & Characteristic.Name == "Alkalinity, total" ) %>%
    ggplot( aes(x = date, y = Result.Value)) + facet_wrap( ~ Monitoring.Location.Name, scales = "free_y") + geom_point()
    



