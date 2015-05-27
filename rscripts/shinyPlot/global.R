
library(ggplot2)
library(dplyr)

### clunky data processing which is only run when the site is first accessed.

load(file = "binaryDat.Rdata")


### replace 0 with DL

### identify values without a detection limit
small_dat$DL_substitution <- FALSE

i <- is.na(small_dat$Detection.Quantitation.Limit.Value1) & small_dat$Result.Value == 0

### set min alkalinity to 2 mg/L
ii<- i & small_dat$Characteristic.Name == "Alkalinity, Phenolphthalein (total hydroxide+1/2 carbonate)"
small_dat$Result.Value[ii] <- 2
small_dat$DL_substitution[ii] <- TRUE

## set min temp to 0.1, 
ii<- small_dat$Result.Value<=0  & small_dat$Characteristic.Name == "Temperature, sample"
small_dat$Result.Value[ii] <- 0.1
small_dat$DL_substitution[ii] <- TRUE


ii<- i & small_dat$Characteristic.Name == "Dissolved oxygen saturation"
small_dat$Result.Value[ii] <- 1
small_dat$DL_substitution[ii] <- TRUE

i <- small_dat$Result.Value == 0
small_dat$Result.Value[i] <- small_dat$Detection.Quantitation.Limit.Value1[i]
small_dat$DL_substitution[i] <- TRUE

small_dat <- as.data.frame(small_dat)
small_dat <- as.tbl(small_dat)


analytes <- filter(small_dat, date >= as.Date("1/1/2000", format = "%m/%d/%Y") ) %>% 
    group_by(Characteristic.Name, analyte) %>%
    summarise(n = n(), min = min(Result.Value), max = max(Result.Value), n_zero = sum(Result.Value == 0 ), n_detects =  sum(Result.Value > 0 ) ) %>% 
    arrange(desc(n))
