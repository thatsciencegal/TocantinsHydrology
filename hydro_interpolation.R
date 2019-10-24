#######################################
####### Data Processing Script  #######
####### by Christine Swanson    #######
#######################################

##Likely irrelevant script as of 10/24/19
library(dplyr)
library(rgdal)
library(sp)
library(gstat)

ana.fluvio <- list.files("E:/Dissertation/Aim 1/Data/WaterLevel/Clean")

write.csv(fluvio, "E:/Dissertation/Aim 1/Data/fluvio_clean.csv")
fluvio <- lapply(ana.fluvio, function(i) read.csv(paste0("E:/Dissertation/Aim 1/Data/WaterLevel/Clean/",i)))

all.fluvio <- bind_rows(fluvio)

meta <- read.csv("../hidroweb_metadata(3).csv")
meta.sm <- select(meta, c("station","lon","lat"))

fluvio.gis <- merge(all.fluvio, meta.sm)
fluvio.uni <- fluvio.gis %>% group_by(Date) %>% distinct(station,.keep_all = TRUE) %>% ungroup()

site.ext <- readOGR(dsn="../Data", layer = "hybas_study_area_dsv")
site.crs <- proj4string(site.ext)
xy <- fluvio.uni[,c("lon","lat")]
fluvio.sp <- SpatialPointsDataFrame(coords=xy, data=fluvio.uni, 
                                    proj4string = CRS(site.crs))
fluvio.small <- fluvio.sp[site.ext,]
tocantins <- readOGR(dsn="C:/Users/starg/Dropbox/dados_tocantins", layer="drenagem_tocantins")
tocantins.trans <- spTransform(tocantins, CRS(site.crs))

tocantins.trans.sm<-tocantins.trans[site.ext,]

fluvio.small.df <- as.data.frame(fluvio.small)
fluvio.small.df <- fluvio.small.df[,-c(6,7)]
fluvio.small.df <- na.omit(fluvio.small.df)
fluvio.small.df<-fluvio.small.df[duplicated(fluvio.small.df$Date) | duplicated(fluvio.small.df$Date, fromLast=TRUE),]
fluvio.small.df <- arrange(fluvio.small.df,desc(Date))

station_list <- split(fluvio.small.df, as.factor(fluvio.small.df$station))

myplots<-lapply(station_list,function(x)
  p<-ggplot(x,aes(x=Date,y=level)) + geom_point()
)

ggplot(fluvio.small.df, aes(x=Date,y=level))+
  geom_point()+
  facet_wrap(~station)
df_list <- split(fluvio.small.df, as.factor(fluvio.small.df$Date))

sp_df_list <- lapply(df_list, function(x){
 coordinates(x) <- ~lon+lat
 proj4string(x) <- CRS(site.crs)
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
