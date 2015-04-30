## 

## requires readExcelFiles.R be sourced

wq_dt <- as.tbl(wq_data)

wq_dt <- select(wq_dt, -Stn. , -Org.Name)



events_by_sites <- group_by( events, Stn.)%>% 
    summarize( n_events = n_distinct(Event.),
               n_orgs = n_distinct(Org.Name),
               earliest = min(date),
               latest = max(date)) %>% 
    arrange( desc(n_events))


active_stations <- filter(events_by_sites, n_events > 100  )
active_events <- filter(events, Stn. %in% active_stations$Stn.)
active_data <- filter(wq_dt, Event. %in% active_events$Event.)


table(active_data$analyte, active_data$sample_fraction, useNA = "always")
