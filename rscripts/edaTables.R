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
