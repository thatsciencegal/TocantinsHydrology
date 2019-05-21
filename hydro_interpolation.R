#######################################
####### Data Processing Script  #######
####### by Christine Swanson    #######
#######################################

library(dplyr)
library(rgdal)
library(sp)

ana.fluvio <- list.files("E:/Dissertation/Aim 1/Data/WaterLevel/Clean")

fluvio <- lapply(ana.fluvio, function(i) read.csv(paste0("E:/Dissertation/Aim 1/Data/WaterLevel/Clean/",i)))

all.fluvio <- bind_rows(fluvio)

meta <- read.csv("../hidroweb_metadata(3).csv")
meta.sm <- select(meta, c("station","lon","lat"))

fluvio.gis <- merge(all.fluvio, meta.sm)

df_list <- split(fluvio.gis, as.factor(fluvio.gis$date_measured))
list2env(df_list,envir=.GlobalEnv)






xy <- fluvio.gis[,c("lon","lat")]
fluvio.sp <- SpatialPointsDataFrame(coords=xy, data=fluvio.gis, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

fluvio.sp<-fluvio.sp[order("date_measured")]
