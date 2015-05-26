#
# Functions to spit out standards for Roaring Fork river segments
#


load("data/binaryDat.RData")
# analytelist <- read.csv("data/standards/analytesOfInterest.csv")

# Table Value Standards for analytes that have an acute and chronic standard
# Analyte should be from analytelist below
# type is chronic or acute
# hardness, pH, and temperature (Celsius) are other inputs needed for some analytes (not all)
# from CO_water_quality_standards.pdf
tvs <- function(analyte, type = "chronic", hardness = NA, pH = NA, temperature = NA){
    analyte_type <- paste(analyte,type,sep = "_")
    analytelist <- c("Aluminum", "Ammonia", "Cadmium", "CadmiumTrout", "Copper", "Lead", "Manganese", "Selenium", "Zinc", "ZincSculpin")
    if (!(analyte %in% analytelist)){
        stop(paste('Analyte must be one of:',paste(analytelist, collapse = ", ")))
    }
    if (analyte_type == "Ammonia_chronic" & is.na(T)){
        warning('Must supply value for T (Temperature, in Celsius) as an argument')
        return(NA)
    } else if (analyte_type %in% c("Aluminum_chronic", "Ammonia_acute", "Ammonia_chronic") & is.na(pH)){
        warning('Must supply value for pH')
        return(NA)
    } else if (!(analyte == "Selenium") & is.na(hardness)){
        warning('Must supply value for hardness')
        return(NA)
    } else switch(analyte_type,
           Aluminum_acute = ifelse(is.na(hardness), NA, exp(1.3695*log(hardness)+1.8308)),
           Aluminum_chronic = ifelse(is.na(hardness) | is.na(pH), NA, exp(1.3695*log(hardness)-0.1158)*(pH >= 7) +
               min(87, exp(1.3695*log(hardness)-0.1158))*(pH < 7)),
           Ammonia_acute = ifelse(is.na(pH), NA, 0.275/(1 + 10^(7.204 - pH)) + 39.0/(1 + 10^(pH-7.204))), # cold water value
           Ammonia_chronic = ifelse(is.na(temperature) | is.na(pH), NA, 
                                    (0.0577/(1+10^(7.688-pH)) + 2.487/(1+10^(pH-7.688)))*
                                        min(2.85,1.45*10^(0.28*(25-temperature)))), # cold water value
           Cadmium_acute = ifelse(is.na(hardness), NA, (1.136672-0.041838*log(hardness))*exp(0.9151*log(hardness)-3.1485)),
           CadmiumTrout_acute = ifelse(is.na(hardness), NA, (1.136672-0.041838*log(hardness))*exp(0.9151*log(hardness)-3.6236)),
           Cadmium_chronic = ifelse(is.na(hardness), NA, (1.101672-0.041838*log(hardness))*exp(0.7998*log(hardness)-4.4451)),
           Copper_acute = ifelse(is.na(hardness), NA, exp(0.9422*log(hardness)-1.7408)),
           Copper_chronic = ifelse(is.na(hardness), NA, exp(0.8545*log(hardness)-1.7428)),
           Lead_acute = ifelse(is.na(hardness), NA, (1.46203-0.145712*log(hardness))*exp(1.237*log(hardness)-1.46)),
           Lead_chronic = ifelse(is.na(hardness), NA, (1.46203-0.145712*log(hardness))*exp(1.237*log(hardness)-4.705)),
           Manganese_acute = ifelse(is.na(hardness), NA, exp(0.3331*log(hardness)+6.4676)),
           Manganese_chronic = ifelse(is.na(hardness), NA, exp(0.3331*log(hardness)+5.8743)),
           Selenium_acute = 18.4,
           Selenium_chronic = 4.6,
           Zinc_acute = ifelse(is.na(hardness), NA, 0.978*exp(0.9094*log(hardness)+0.9095)),
           Zinc_chronic = ifelse(is.na(hardness), NA, 0.986*exp(0.9094*log(hardness)+0.6235)),
           ZincSculpin_chronic = exp(2.140*log(hardness)-5.084) # if hardness < 102 mg/l CaCO3 (do we use this??? ch or ac???)
           )
}

# Table value standards for temperature use different data
# need to specify segment (1-12, possibly with a letter attached)
# dm stands for "Daily Maximum", if dm is false, measurement is
# taken to be mwat, which is "Maximum Weekly Average Temperature")
# [It's not clear to me if this is water temp or air temp]
tvstemp <- function(segment, date, dm = T){
    require(lubridate)
    ifelse(dm == T, "dm", "mwat")
    if ((segment %in% c("1", "2", "3a", "3d", "4", "5", "6", "7", "8", "9", "10a", "10b")) | (segment %in% c(1,2,4:10))){
        tier <- "CS-I"
    } else if (segment %in% c("3b", "3c")){
        tier <- "CS-II"
    } else if ((segment %in% c("11", "12")) | (segment %in% c(11,12))){
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
# note that temp isn't an option with this function -- use tvstemp to get tmp standards
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
           "Aluminum_acute" = tvs("Aluminum", "acute", hard, ph, temp),
           "Aluminum_chronic" = tvs("Aluminum", "chronic", hard, ph, temp),
           "Cadmium_acute" = tvs("Cadmium", "acute", hard, ph, temp),
           "Cadmium_chronic" = tvs("Cadmium", "chronic", hard, ph, temp),
           "Iron" = 300, # ug/l dissolved
           "Iron_Trec" = 1000,
           "Manganese_acute" = tvs("Manganese", "acute", hard, ph, temp),
           "Manganese_chronic" = tvs("Manganese", "chronic", hard, ph, temp),
           "Manganese_chronic_dis" = 50,
           "Selenium_acute" = tvs("Selenium", "acute", hard, ph, temp),
           "Selenium_chronic" = tvs("Selenium", "chronic", hard, ph, temp),
           "Arsenic_acute" = 340,
           "Arsenic_chronic" = ifelse(segment == "3b", "0.02 to 10", 0.02), # Trec for chronic
           "Cadmium_acute" = tvs("CadmiumTrout", "acute", hard, ph, temp),
           "Cadmium_chronic" = tvs("Cadmium", "chronic", hard, ph, temp),
           "Copper_acute" = tvs("Copper", "acute", hard, ph, temp),
           "Copper_chronic" = tvs("Copper", "chronic", hard, ph, temp),
           "Lead_acute" = tvs("Lead", "acute", hard, ph, temp),
           "Lead_chronic" = tvs("Lead", "chronic", hard, ph, temp),
           "Zinc_acute" = tvs("Zinc", "acute", hard, ph, temp),
           "Zinc_chronic" = ifelse((segment %in% c(2, 5, 6, 10)) | (segment %in% c("2", "5", "6", "10")),
                                   tvs("ZincSculpin", "chronic", hard, ph, temp),
                                   tvs("Zinc", "chronic", hard, ph, temp)),
           "Phosphorus" = ifelse((segment %in% c(11, 12)) | (segment %in% c("11", "12")), 25, 110), # ug/l total
           "Chloride" = 250,
           "Sulfate" = 250 #mg/l
           )
}



# take a look at measured values
qplot(date, Result.Value, data = mydat) + facet_wrap(~analyte, scales = "free_y")

# rearrange small_dat to wide format
library(data.table)
small_dat <- data.table(small_dat)
mydat <- small_dat[,list(date, Monitoring.Location.ID,
                         Characteristic.Name, Sample.Fraction, Result.Unit,
                         analyte, Result.Value)]
# there are some cases of multiple measurements on the same day -- take averages 
datwide <- dcast(mydat, date + Monitoring.Location.ID ~ analyte, value.var = "Result.Value", fun.aggregate = mean)

### add standards to datwide

# analytes for which standards are specifed for dissolved measurements (as opposed to total)
# don't do Arsenic yet because need stream segment info
dissolved.meas <- c("Aluminum", "Cadmium", "Copper", "Iron", "Lead", "Manganese", "Selenium", "Zinc")

# add standards for dissolved measures
for (i in dissolved.meas){
    datwide[, paste(i,"Standard_chronic", sep = "_")] <- mapply(roaringforkstandards, measure = i, type = "chronic", segment = NA, hard = datwide[,"Hardness, Ca, Mg"], ph = datwide[,"pH"], temp = datwide[,"Temperature, sample"])
    datwide[, paste(i,"Standard_acute", sep = "_")] <- mapply(roaringforkstandards, measure = i, type = "acute", segment = NA, hard = datwide[,"Hardness, Ca, Mg"], ph = datwide[,"pH"], temp = datwide[,"Temperature, sample"])
}

### add other standards
# others
other.meas <- c("DO", "pHmin", "pHmax", "Iron", "Iron_Trec", "Phosphorus", "Chloride", "Sulfate")
for (i in other.meas){
    datwide[, paste(i,"Standard", sep = "_")] <- mapply(roaringforkstandards, measure = i, type = NA, segment = NA, hard = datwide[,"Hardness, Ca, Mg"], ph = datwide[,"pH"], temp = datwide[,"Temperature, sample"])
}

# temperature (need segment classifications before this will work)
# datwide[,"Temperature_Standard"] <- mapply(tvstemp, segment = NA, date = datwide[,"date"])

# how best to plot?
datwide$loghard <- log(datwide[,c("Hardness, Ca, Mg")])
qplot(date, Zinc_Dissolved, data = datwide, color = factor(Zinc_Dissolved > Zinc_Standard_chronic)) 
qplot(loghard, Zinc_Dissolved, data = datwide, color = factor(Zinc_Dissolved > Zinc_Standard_chronic)) 
