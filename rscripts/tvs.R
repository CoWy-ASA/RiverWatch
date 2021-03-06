#
# Functions to spit out standards for Roaring Fork river segments
#

library(plyr)
library(reshape2)
library(data.table)
library(ggplot2)

# Table Value Standards for analytes that have an acute and chronic standard
# Analyte should be from analytelist below
# type is chronic or acute
# hardness, pH, and temperature (Celsius) are other inputs needed for some analytes (not all)
# from CO_water_quality_standards.pdf
tvs <- function(analyte, type = "chronic", hardness = NA, pH = NA, temperature = NA, meas = "Dissolved"){
    analyte_type <- paste(analyte,meas,type,sep = "_")
    analytelist <- c("Aluminum", "Ammonia", "Cadmium", "CadmiumTrout", "Copper", "Lead", "Manganese", "Selenium", "Zinc", "ZincSculpin")
    if (!(analyte %in% analytelist)){
        stop(paste('Analyte must be one of:',paste(analytelist, collapse = ", ")))
    }
    if (analyte_type == "Ammonia_Dissolved_chronic" & is.na(T)){
        warning('Must supply value for T (Temperature, in Celsius)')
        return(NA)
    } else if (analyte_type %in% c("Aluminum_Dissolved_chronic", "Ammonia_acute", "Ammonia_chronic") & is.na(pH)){
        warning('Must supply value for pH')
        return(NA)
    } else if (!(analyte == "Selenium") & is.na(hardness)){
        warning('Must supply value for hardness')
        return(NA)
    } else switch(analyte_type,
           Aluminum_Dissolved_acute = ifelse(is.na(hardness), NA, exp(1.3695*log(min(hardness,220))+1.8308)),
           Aluminum_Dissolved_chronic = ifelse(is.na(hardness) | is.na(pH), NA, exp(1.3695*log(min(hardness,220))-0.1158)*(pH >= 7) +
               min(87, exp(1.3695*log(min(hardness,220))-0.1158))*(pH < 7)),
           Ammonia_Dissolved_acute = ifelse(is.na(pH), NA, 0.275/(1 + 10^(7.204 - pH)) + 39.0/(1 + 10^(pH-7.204))), # cold water value
           Ammonia_Dissolved_chronic = ifelse(is.na(temperature) | is.na(pH), NA, 
                                    (0.0577/(1+10^(7.688-pH)) + 2.487/(1+10^(pH-7.688)))*
                                        min(2.85,1.45*10^(0.28*(25-temperature)))), # cold water value
           Cadmium_Dissolved_acute = ifelse(is.na(hardness), NA, (1.136672-0.041838*log(min(hardness,400)))*exp(0.9151*log(min(hardness,400))-3.1485)),
           CadmiumTrout_Dissolved_acute = ifelse(is.na(hardness), NA, (1.136672-0.041838*log(min(hardness,400)))*exp(0.9151*log(min(hardness,400))-3.6236)),
           Cadmium_Dissolved_chronic = ifelse(is.na(hardness), NA, (1.101672-0.041838*log(min(hardness,400)))*exp(0.7998*log(min(hardness,400))-4.4451)),
           Copper_Dissolved_acute = ifelse(is.na(hardness), NA, exp(0.9422*log(min(hardness,400))-1.7408)),
           Copper_Dissolved_chronic = ifelse(is.na(hardness), NA, exp(0.8545*log(min(hardness,400))-1.7428)),
           Lead_Dissolved_acute = ifelse(is.na(hardness), NA, (1.46203-0.145712*log(min(hardness,400)))*exp(1.237*log(min(hardness,400))-1.46)),
           Lead_Dissolved_chronic = ifelse(is.na(hardness), NA, (1.46203-0.145712*log(min(hardness,400)))*exp(1.237*log(min(hardness,400))-4.705)),
           Manganese_Dissolved_acute = ifelse(is.na(hardness), NA, exp(0.3331*log(min(hardness,400))+6.4676)),
           Manganese_Dissolved_chronic = ifelse(is.na(hardness), NA, exp(0.3331*log(min(hardness,400))+5.8743)),
           Selenium_Dissolved_acute = 18.4,
           Selenium_Dissolved_chronic = 4.6,
           Zinc_Dissolved_acute = ifelse(is.na(hardness), NA, 0.978*exp(0.9094*log(min(hardness,400))+0.9095)),
           Zinc_Dissolved_chronic = ifelse(is.na(hardness), NA, 0.986*exp(0.9094*log(min(hardness,400))+0.6235)),
           ZincSculpin_Dissolved_chronic = exp(2.140*log(min(hardness,400))-5.084) # if hardness < 102 mg/l CaCO3 
           )
}

# Table value standards for temperature use different data
# need to specify segment (1-12, possibly with a letter attached)
# dm stands for "Daily Maximum", if dm is false, measurement is
# taken to be mwat, which is "Maximum Weekly Average Temperature")
# These standards literally apply to DM or MWAT, not to individual event 
# measurements!
tvstemp <- function(segment, date, dm = T){
    require(lubridate)
    type <- ifelse(dm == T, "dm", "mwat")
    if ( segment %in% c("01", "02", "03A", "03D", "04", "05", "06", "07", "08", "09", "10A", "10B") ){
        tier <- "CS-I"
    } else if (segment %in% c("03B", "03C")){
        tier <- "CS-II"
    } else if ( segment %in% c("11", "12") ){
        tier <- "CLL"
    } else {
        stop('Invalid segment')
    }
    tier_type <- paste(tier, type,sep = "_")
    mo <- month(date)
    switch(tier_type,
           "CS-I_mwat" = ifelse(mo %in% c(6:9), 17.0, 9.0),
           "CS-I_dm" = ifelse(mo %in% c(6:9), 21.7, 13.0),
           "CS-II_mwat" = ifelse(mo %in% c(4:10), 18.3, 9.0),
           "CS-II_dm" = ifelse(mo %in% c(4:10), 23.9, 13.0),
           "CLL_mwat" = ifelse(mo %in% c(4:12), 18.3, 9.0),
           "CLL_dm" = ifelse(mo %in% c(4:12), 23.8, 13.0)
    )
}
    
# Roaring fork standards, from rf_state_wq_classification.pdf
# often simply calls the TVS function
roaringforkstandards <- function(measure, type = NA,  segment = NA, hard = NA, ph = NA, temp = NA){
    if(!is.na(type)){
        measure_type <- paste(measure, type, sep = "_")
    } else {
        measure_type <- measure
    }
    switch(measure_type,
           "DO" = 6.0, # mg/l
           "DOsp" = 7.0, # mg/l, sp = spawning
           "pHmin" = 6.5, 
           "pHmax" = 9.0,
           "Aluminum_Dissolved_acute" = tvs("Aluminum", "acute", hard, ph, temp),
           "Aluminum_Dissolved_chronic" = tvs("Aluminum", "chronic", hard, ph, temp),
           "Cadmium_Dissolved_acute" = tvs("Cadmium", "acute", hard, ph, temp),
           "Cadmium_Dissolved_chronic" = tvs("Cadmium", "chronic", hard, ph, temp),
           "Iron_Dissolved" = 300, # ug/l dissolved
           "Iron_Total" = 1000,
           "Manganese_Dissolved_acute" = tvs("Manganese", "acute", hard, ph, temp),
           "Manganese_Dissolved_chronic" = tvs("Manganese", "chronic", hard, ph, temp),
           "Manganese_Dissolved_chronic_dis" = 50,
           "Selenium_Dissolved_acute" = tvs("Selenium", "acute", hard, ph, temp),
           "Selenium_Dissolved_chronic" = tvs("Selenium", "chronic", hard, ph, temp),
           "Arsenic_Dissolved_acute" = 340,
           "Arsenic_Dissolved_chronic" = NA,
           "Arsenic_Total_acute" = NA,
           "Arsenic_Total_chronic" = ifelse(segment == "3B", "0.02 to 10", 0.02), # Trec for chronic
           "Cadmium_Dissolved_acute" = tvs("CadmiumTrout", "acute", hard, ph, temp),
           "Cadmium_Dissolved_chronic" = tvs("Cadmium", "chronic", hard, ph, temp),
           "Copper_Dissolved_acute" = tvs("Copper", "acute", hard, ph, temp),
           "Copper_Dissolved_chronic" = tvs("Copper", "chronic", hard, ph, temp),
           "Lead_Dissolved_acute" = tvs("Lead", "acute", hard, ph, temp),
           "Lead_Dissolved_chronic" = tvs("Lead", "chronic", hard, ph, temp),
           "Zinc_Dissolved_acute" = tvs("Zinc", "acute", hard, ph, temp),
           "Zinc_Dissolved_chronic" = ifelse((segment %in% c("02", "05", "06", "10")) & (hard < 102),
                                   tvs("ZincSculpin", "chronic", hard, ph, temp),
                                   tvs("Zinc", "chronic", hard, ph, temp)),
           "Phosphorus_Total" = ifelse(segment %in% c("11", "12"), 25, 110), # ug/l total
           "Chloride_Total" = 250,
           "Sulfate_Total" = 250 #mg/l
           )
}



load("data/binaryDat.RData")
stations <- read.csv("data/processed/stations.csv")
# analytelist <- read.csv("data/standards/analytesOfInterest.csv")
stations$segment <- substr(stations$WaterBodyID,7,10)

# take a look at measured values
qplot(date, Result.Value, data = small_dat) + facet_wrap(~analyte, scales = "free_y")

# rearrange small_dat to wide format
small_dat <- data.table(small_dat)
mydat <- small_dat[,list(date, Monitoring.Location.ID,
                         Characteristic.Name, Sample.Fraction, Result.Unit,
                         analyte, Result.Value)]
# there are some cases of multiple measurements on the same day -- take averages 
datwide <- dcast(mydat, date + Monitoring.Location.ID ~ analyte, value.var = "Result.Value", fun.aggregate = mean)

# add segment info from stations dataframe
datwide <- merge(datwide, stations[,c("Station", "segment")], by.x = "Monitoring.Location.ID", by.y = "Station", all.x = T)

# get hardness mins
tmp <- datwide[,c("Monitoring.Location.ID", "Hardness, Ca, Mg")]
setnames(tmp,names(tmp),c("Monitoring.Location.ID", "hardness"))
tmp <- data.table(tmp)
minhardness <- tmp[,.(minhardness = min(hardness, na.rm=T)), by = Monitoring.Location.ID]
datwide <- data.table(datwide)
# add to dataframe
datwide <- merge(datwide, minhardness, by = "Monitoring.Location.ID", all.x = T)
datwide <- data.frame(datwide)

### add standards to datwide

# analytes for which standards are specifed for dissolved measurements (as opposed to total)
# and which have chronic and acute standards
dissolved.meas <- c("Aluminum", "Cadmium", "Copper", 
                    "Lead", "Manganese", "Selenium", "Zinc")

# add standards for dissolved measures (separate chronic and acute standards)
for (i in dissolved.meas){
    datwide[, paste(i,"Dissolved_Standard_chronic", sep = "_")] <- mapply(roaringforkstandards, measure = paste(i,"Dissolved",sep="_"), 
                                                                type = "chronic", 
                                                                segment = datwide[,"segment"], 
                                                                hard = datwide[,"minhardness"], 
                                                                ph = datwide[,"pH"], 
                                                                temp = datwide[,"Temperature..sample"])
    datwide[, paste(i,"Dissolved_Standard_acute", sep = "_")] <- mapply(roaringforkstandards, measure =  paste(i,"Dissolved",sep="_"), 
                                                              type = "acute", 
                                                              segment = datwide[,"segment"], 
                                                              hard = datwide[,"minhardness"], 
                                                              ph = datwide[,"pH"], 
                                                              temp = datwide[,"Temperature..sample"])
}

### add other standards (no chronic/acute designation for most of these)

# Arsenic_chronic is on total, not dissolved
datwide[,"Arsenic_Total_Standard_chronic"] <- mapply(roaringforkstandards, measure = "Arsenic_Total_chronic", 
                                                     type = NA, 
                                                     segment = datwide[,"segment"], 
                                                     hard = datwide[,"minhardness"], 
                                                     ph = datwide[,"pH"], 
                                                     temp = datwide[,"Temperature..sample"])

# others
other.meas <- c("DO", "DOsp", "pHmin", "pHmax", "Iron_Dissolved", "Iron_Total", "Phosphorus_Total", "Chloride_Total", "Sulfate_Total")
for (i in other.meas){
    datwide[, paste(i,"Standard", sep = "_")] <- mapply(roaringforkstandards, measure = i, 
                                                        type = NA, 
                                                        segment = datwide[,"segment"], 
                                                        hard = datwide[,"minhardness"], 
                                                        ph = datwide[,"pH"], 
                                                        temp = datwide[,"Temperature..sample"])
}

# temperature
datwide[,"Temperature_DM_Standard"] <- mapply(tvstemp, segment = datwide[,"segment"], date = datwide[,"date"])

# how best to plot?
datwide$loghard <- log(datwide[,c("Hardness..Ca..Mg")])

all.meas <- c(paste(dissolved.meas,"Dissolved",sep = "_"), "Arsenic_Total", other.meas)

for (i in dissolved.meas){
    datwide[,paste(i,"fail",sep="_")] <- datwide[,paste(i,"Dissolved",sep="_")] > datwide[,paste(i,"Dissolved", "Standard","chronic",sep="_")]
    print(ggplot(aes_string("date", paste(i,"Dissolved",sep="_"), col = paste(i,"fail",sep="_")), data = datwide) +
              geom_point() + 
              xlab("Date") + ylab(paste(i, "Dissolved", sep = "_")) )
    print(ggplot(aes_string("loghard", paste(i,"Dissolved",sep="_"),
                            color = paste(i,"fail",sep="_")), 
                 data = datwide) + geom_point() +
              xlab("Ln(hardness)") + ylab(paste(i, "Dissolved", sep = "_")))
    
}



### how often are standards not met?

## standards on dissolved
for (i in dissolved.meas){
    print(i)
    x <- sum(datwide[,paste(i,"Dissolved",sep="_")] > datwide[,paste(i, "Dissolved_Standard_chronic", sep="_")], na.rm = T)
    print(c(x, x/sum(!is.na(datwide[,paste(i,"Dissolved",sep="_")]))))
}

## standards on totals
# Phosphorus
# Chloride
# Sulfate
for (i in c("Phosphorus", "Chloride", "Sulfate")){
    print(i)
    x <- sum(datwide[,paste(i, "Total", sep = "_")] > datwide[,paste(i,"Total", "Standard", sep="_")], na.rm = T)
    print(c(x, x/sum(!is.na(datwide[,paste(i,"Total", sep="_")]))))
}

## oddballs

# Arsenic acute
x <- sum(datwide[,"Arsenic_Dissolved"] > datwide[,"Arsenic_Dissolved_Standard_acute"], na.rm = T)
print(c(x, x/sum(!is.na(datwide[,"Arsenic_Total"]))))

# Arsenic Total
x <- sum(datwide[,"Arsenic_Total"] > datwide[,"Arsenic_Total_Standard_chronic"], na.rm = T)
print(c(x, x/sum(!is.na(datwide[,"Arsenic_Total"]))))
         
# Iron Total
x <- sum(datwide[,"Iron_Total"] > datwide[,"Iron_Total_Standard"], na.rm = T)
print(c(x, x/sum(!is.na(datwide[,"Iron_Total"]))))

# Iron Dissolved
x <- sum(datwide[,"Iron_Dissolved"] > datwide[,"Iron_Dissolved_Standard"], na.rm = T)
print(c(x, x/sum(!is.na(datwide[,"Iron_Dissolved"]))))

# pH
x <- sum((datwide[,"pH"] < datwide[,"pHmin_Standard"]) | (datwide[,"pH"] > datwide[,"pHmax_Standard"]), na.rm = T)
print(c(x, x/sum(!is.na(datwide[,"pH"]))))

# DO
x <- sum(datwide[,"Dissolved.oxygen..DO."] < datwide[,"DO_Standard"], na.rm = T)
print(c(x, x/sum(!is.na(datwide[,"Dissolved.oxygen..DO."]))))

# DOsp
x <- sum(datwide[,"Dissolved.oxygen..DO."] < datwide[,"DOsp_Standard"], na.rm = T)
print(c(x, x/sum(!is.na(datwide[,"Dissolved.oxygen..DO."]))))

# Temperature (not correct!  standard is for daily maximum, but this should give a sense)
x <- sum(datwide[,"Temperature..sample"] > datwide[, "Temperature_DM_Standard"], na.rm = T)
print(c(x, x/sum(!is.na(datwide[,"Temperature..sample"]))))




qplot(date, Hardness..Ca..Mg, data = datwide) + facet_wrap(~ Monitoring.Location.ID)
