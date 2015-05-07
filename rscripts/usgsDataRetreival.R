### reading USGS XLM

download.file("http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&huc=14010004&siteType=ST" ,destfile = "../data/usgs.xml")

install.packages("dataRetrieval")

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
