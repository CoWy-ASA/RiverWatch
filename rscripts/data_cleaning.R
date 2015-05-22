### outlier detection

## zero's were there shouldn't be one. eg. Calcium.

## all zeros

## values less than detection

load("../data/binaryDat.Rdata")
library(tidy)
library(reshape2)
library(XLConnect)


filter(small_dat, Result.Value < Detection.Quantitation.Limit.Value1 & Result.Value > 0 )

# 2,423 values - clarify the meaning of Detection.Quantification.Limit.  Is the procedure for reporting values between 0 and DL consistent?

filter( small_dat, Sample.Fraction != "Dissolved") %>% ggplot( aes(x = analyte, y = Result.Value)) + facet_wrap(~analyte, scales = "free") + geom_boxplot()

## ph range

wide_data <- dcast( small_dat, formula = Monitoring.Location.Name + date ~ analyte)  ## there are multiple results with  no identifable id making the distinct.  Some appear to be a wide spread.

filter(small_dat, Monitoring.Location.Name == "7-11 Br"  & Activity.ID == "72.120M") %>% select( analyte, Result.Value) %>% arrange(desc(analyte) ) -> x

View(x)
    
    
small_dat %>% group_by(Activity.ID, Monitoring.Location.ID, Activity.Start.Date, Activity.Start.Time, analyte, date) %>% summarise(n = length(Result.Value), min = min(Result.Value), max = max(Result.Value))    %>% filter(n > 1) -> yy


grp_cols <- names(small_dat)[! names(small_dat) %in% c("Result.Value", "Result.UID")]


dots <- lapply(grp_cols, as.symbol)

# Perform frequency counts
 

small_dat %>% group_by_(.dots=dots) %>% summarise(n = length(Result.Value), min = min(Result.Value), max = max(Result.Value), dl = max(Detection.Quantitation.Limit.Value1))  -> new_dat  ## note the underscore in group_by
    
new_dat$flag <- NA
new_dat$flag[new_dat$n > 1] <- 777

new_dat$Result.Value <- new_dat$max

select()

dups <- filter(new_dat, n >1)
zz <- left_join(dups, small_dat)

wb <- loadWorkbook("../data/censored/multipleSamples.xls",create = TRUE)

createSheet(wb, name = "dup_events")
writeWorksheet(wb, data = as.data.frame(dups), sheet = "dup_events")

createSheet(wb, name = "raw_events")
writeWorksheet(wb, as.data.frame(zz), sheet = "raw_events")

saveWorkbook(wb)



View(small_dat %>% filter( Activity.ID == "292.005M" & analyte == "Aluminum_Dissolved") )

View(small_dat %>% filter( Activity.ID == "292.005M" & analyte == "Iron_Total")  )


(apply(zz, 2, function(x) {length(unique(x)) != 1} ) ) -> ii
zz[,ii]

