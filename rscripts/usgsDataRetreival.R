### reading USGS XLM

# download.file("http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&huc=14010004&siteType=ST" ,destfile = "../data/usgs.xml")

# install.packages("dataRetrieval")

library(dataRetrieval)

# Choptank River near Greensboro, MD:
siteNumber <- "01491000"
parameterCd <- "00060" # Discharge
startDate <- "2009-10-01"
endDate <- "2012-09-30"


rf_sites <- c("09072550", "09073005", "09073300", "09073400", "09074000", "09074500", "09075400", "09078141", "09078475", "09079450", "09080400", "09081000", "09081600", "09085000")

#http://waterdata.usgs.gov/nwis/current?huc_cd=14010004&sitefile_output_format=html_table&PARAmeter_cd=STATION_NM

discharge <- readNWISdv(siteNumber = rf_sites,
                        parameterCd, startDate, endDate)

### example of XML query, contains attribute information.
startDate <- "1990-09-01"
endDate <- "2014-10-01"
offering <- '00003'
property <- '00060'

obs_url <- constructNWISURL(rf_sites,property,startDate,endDate,'dv')

data <- importWaterML1(obs_url)

usgs_site_info <- attr(data, which="siteInfo")
names(usgs_site_info)[6:7] <- c("latitude", "longitude")


### map and compare with monitoring sites

dat   <- read.csv("../data/dsn_pull/rf-1990-2015.csv", header = TRUE, as.is = TRUE)

sites <- read.csv("../data/dsn_pull/rf_sites.csv", header = TRUE, as.is = TRUE )

dat$date <- as.Date(dat$Activity.Start.Date, format = "%m/%d/%Y")
dat$Result.Value <- as.numeric(dat$Result.Value)
dat_dt <- as.tbl(dat)


events <- unique( dat[ , names(dat) %in% c("Activity.ID", "date", "Monitoring.Location.ID", "Monitoring.Location.Name", "Monitoring.Location.Latitude", "Monitoring.Location.Longitude"   ) ] )       
events <- as.tbl(events)

main_sites <- filter(events,date >= as.Date("1/1/2000", format = "%m/%d/%Y") )  %>% group_by(Monitoring.Location.ID,Monitoring.Location.Name, Monitoring.Location.Latitude, Monitoring.Location.Longitude  )%>% 
    summarize( n_events = n_distinct(date),
               earliest = min(date), 
               latest = max(date)) %>% 
    arrange( desc(n_events)) %>%  filter( n_events > 25 )

names(main_sites)[3:4] <- c("latitude", "longitude")



dw_sites <- unique( dat[ , names(dat) %in% c("Monitoring.Location.ID", "Monitoring.Location.Name" ) ] )  


### map

m = leaflet() %>% addTiles()
m %>% addCircles(data=main_sites) %>% 
    addCircles(data = usgs_site_info,color = "red",fill = "red") %>% 
    addMarkers(data=usgs_site_info, popup=usgs_site_info$site_no )

usgs_dat <- as.tbl(data)
names(usgs_dat)[6] <- "flow_cfs"

temp <- filter(usgs_dat, site_no %in% c("09085000", "09081600", "09081000", "09080400", "09073400" ) )
library(reshape2)
flow_wide <- dcast(data = temp, formula = dateTime ~site_no, value.var =  "flow_cfs"  )
