#######################################
####### Data Processing Script  #######
####### by Christine Swanson    #######
#######################################

library(dplyr)
library(rgdal)
library(sp)
library(gstat)

ana.fluvio <- list.files("E:/Dissertation/Aim 1/Data/WaterLevel/Clean")

fluvio <- lapply(ana.fluvio, function(i) read.csv(paste0("E:/Dissertation/Aim 1/Data/WaterLevel/Clean/",i)))
write.csv(fluvio, "E:/Dissertation/Aim 1")

all.fluvio <- bind_rows(fluvio)

meta <- read.csv("../hidroweb_metadata(3).csv")
meta.sm <- select(meta, c("station","lon","lat"))

fluvio.gis <- merge(all.fluvio, meta.sm)
fluvio.gis <- fluvio.gis[,-3]
fluvio.uni <- fluvio.gis %>% group_by(date_measured) %>% distinct(station,.keep_all = TRUE) %>% ungroup()

site.ext <- readOGR(dsn="../Data", layer = "hybas_lv7_clp_dsv")
site.crs <- proj4string(site.ext)
xy <- fluvio.uni[,c("lon","lat")]
fluvio.sp <- SpatialPointsDataFrame(coords=xy, data=fluvio.uni, 
                                    proj4string = crs(site.crs))
fluvio.small <- fluvio.sp[site.ext,]

fluvio.small.df <- as.data.frame(fluvio.small)
fluvio.small.df <- fluvio.small.df[,-c(8,9)]
fluvio.small.df <- na.omit(fluvio.small.df)
fluvio.small.df<-fluvio.small.df[duplicated(fluvio.small.df$date_measured) | duplicated(fluvio.small.df$date_measured, fromLast=TRUE),]
fluvio.small.df <- arrange(fluvio.small.df,desc(date_measured))


df_list <- split(fluvio.small.df, as.factor(fluvio.small.df$date_measured))

sp_df_list <- lapply(df_list, function(x){
  SpatialPointsDataFrame(coords = xy, data = df_list, proj4string = crs(site.crs))
})

daily_int <- lapply(df_list, function(x) {
  out <- with(x,gstat(formula = mean~1, locations=x, set=list(idp = 0)))
  return(out)
})

length(df_list)

list2env(df_list,envir=.GlobalEnv)






xy <- fluvio.gis[,c("lon","lat")]
fluvio.sp <- SpatialPointsDataFrame(coords=xy, data=fluvio.gis, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

fluvio.sp<-fluvio.sp[order("date_measured")]
