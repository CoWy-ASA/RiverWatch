### reading USGS XLM

# download.file("http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&huc=14010004&siteType=ST" ,destfile = "../data/usgs.xml")

# install.packages("dataRetrieval")

library(dataRetrieval)
library(reshape2)

## roaring fork sites

rf_sites <- c("09072550", "09073005", "09073300", "09073400", "09074000", "09074500", "09075400", "09078141", "09078475", "09079450", "09080400", "09081000", "09081600", "09085000")

# source
# http://waterdata.usgs.gov/nwis/current?huc_cd=14010004&sitefile_output_format=html_table&PARAmeter_cd=STATION_NM



### example of XML query, contains attribute information.
startDate <- "2000-01-01"
endDate <- "2014-10-01"
offering <- '00003'
property <- '00060'

#discharge <- readNWISdv(siteNumber = rf_sites,
#                        parameterCd, startDate, endDate)

obs_url <- constructNWISURL(rf_sites,property,startDate,endDate,'dv')

data <- importWaterML1(obs_url,asDateTime = TRUE)

usgs_dat <- as.tbl(data)
names(usgs_dat)[6] <- "flow_cfs"

usgs_dat <- as.tbl(data)
names(usgs_dat)[6] <- "flow_cfs"

## a subset of sites on major branches selected visually.

# temp <- filter(usgs_dat, site_no %in% c("09085000", "09081600", "09081000", "09080400", "09073400" ) ) 
flow_wide <- dcast(data = usgs_dat, formula = dateTime ~site_no, value.var =  "flow_cfs"  )

## chose sites with few missing values
ii <- apply(flow_wide, 2, function(x){sum(is.na(x)) } ) < 2  ## 

flow_wide <- flow_wide[,ii]

cor(flow_wide[, -1],use = "pairwise.complete.obs")  ## also drop site 09080400 because it is below Ruedi Res and is not well correlated with others.  Add caveat.

flow_wide <- flow_wide[, names(flow_wide)!= "09080400"]
flow_wide$dateTime <- as.Date(flow_wide$dateTime)

names(flow_wide)[1] <- "date"

if(FALSE){
    dygraph(flow_wide[,1:2])

    
}
flow_scaled <- flow_wide

for(i in 2:ncol(flow_wide) ){
    flow_scaled[,i] <- scale(flow_wide[,i])
}

flow_scaled$avg <- apply(flow_scaled[,-1],1, mean )

ff <- ecdf( flow_scaled$avg)
flow_scaled$ecdf <- ff(flow_scaled$avg)
flow_scaled$glenwood <- flow_wide[, names(flow_wide)=="09085000" ]

## keep in mind that ecdf scaling cannot be transposed back to flow units.
## wq

#obs_url <- constructNWISURL(rf_sites,"00910",startDate,endDate,'qw')
#wq_data    <- importWaterML1(obs_url)


usgs_site_info <- attr(data, which="siteInfo")
names(usgs_site_info)[6:7] <- c("latitude", "longitude")
usgs_site_info$major <- usgs_site_info$site_no %in% names(flow_wide)


save( list = c("usgs_site_info", "flow_wide"), file = "../data/binaryFlow.Rdata")
