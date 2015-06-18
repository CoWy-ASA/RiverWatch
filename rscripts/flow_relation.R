### flow - relations

### using the subset of gages - we would like to explore presenting the relation of flow and hardness

rm(list = ls() )
load("data//binaryDat.Rdata")
source("rscripts/usgsDataRetreival.R")
## chose common analytes
analytes <- analytes[analytes$Result.Value > 150,   ]

names(flow_wide)[1] <- "date"
sub_dat <- left_join(small_dat, flow_wide, by = "date",copy = TRUE )  ## adds flow columns

####

sub_dat2 <- filter(sub_dat, Characteristic.Name == "Hardness, Ca, Mg")

pdf("test.pdf", width = 9, height = 9)
par(mfrow = c( 6, 6), mar = c(1,1,1,1) )

sites <- unique(sub_dat$Monitoring.Location.ID)

for (i in 1:20){
    
x <- subset(sub_dat2, Monitoring.Location.ID == sites[i] )    
y <- log(x[, c(25, 42:47)] )
 indx <- cor(y)[-1,1 ]
ii <- which.min(unlist(indx) )

plot(x[,c(42, 25) ],log = "xy" )
mtext(text = round(indx[1], 2), line = -2 )

plot(x[,c(43, 25) ],log = "xy" )
mtext(text = round(indx[2], 2), line = -2 )

plot(x[,c(44,25) ],log = "xy" )
mtext(text = round(indx[3], 2), line = -2 )

plot(x[,c(45,25) ],log = "xy" )
mtext(text = round(indx[4], 2), line = -2 )

plot(x[,c(46,25) ],log = "xy" )
mtext(text = round(indx[5], 2), line = -2 )

plot(x[,c(47,25) ],log = "xy" )
mtext(text = round(indx[6], 2), line = -2 )

}

dev.off()


filter(sub_dat, Characteristic.Name == "Hardness, Ca, Mg") %>% 
    ggplot( aes(x = ecdf, y = Result.Value)) + 
    facet_wrap(~Monitoring.Location.Name, scales = "free_y") + 
    geom_point()


filter(sub_dat, Characteristic.Name == "Hardness, Ca, Mg") %>% 
    ggplot( aes(x = log(glenwood), y = Result.Value)) + 
    facet_wrap(~Monitoring.Location.Name, scales = "free_y") + 
    geom_point()  + scale_x_log10() + scale_y_log10() #+ coord_trans(xtrans = "log")

filter(sub_dat, Characteristic.Name == "Calcium") %>% 
    ggplot( aes(x = log(glenwood), y = Result.Value)) + 
    facet_wrap(~Monitoring.Location.Name, scales = "free_y") + 
    geom_point() #+ coord_trans(xtrans = "log")


filter(sub_dat, analyte == "Ammonia_Total" ) %>% 
    ggplot( aes(x = log(glenwood), y = log(Result.Value) ) ) + 
    facet_wrap(~Monitoring.Location.Name, scales = "free_y") + 
    geom_point() +  scale_x_log10() 
