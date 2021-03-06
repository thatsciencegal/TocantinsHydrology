library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(stringr)

#Read in the dem
to.dem <- raster("C:/Users/starg/Dropbox/Dissertation/Chapter1/Data/GIS/DEMs/to_dem.tif")

foo <- function(station, xmin=1000, xmax=1000) {
  #Function to create a raster of water level minus elevation
  
  #Read in the data file and filter out points before 1994
  #in_file <- paste0("C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/Cotas/Cotas_", station, ".csv")
  #lev<-read.csv(in_file) %>% filter(year(mdy(Date))>=1990)
  
  lev <- wat.lev3
  #Turn file into a spatial points dataframe
  nr<-nrow(lev)
  if(nr==0){
    print(paste0("Station ",station," has no points after 1994"))
  }else{
  xy<-lev[,c(4,3)]
  lev.spdf<-SpatialPointsDataFrame(coords=xy,data=lev,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #Extract the elevground elevation at the xy location of the water level sensor
  topo <- raster::extract(to.dem,xy)
  
  #Add ground elevation to water level and convert water level to meters
  lev.spdf@data$water_level<-topo[1]+(0.01*lev.spdf@data$Level)
  
  #Make a raster of water elevation for each day in the dataset
  ext<-vector("list",nr)
  for(i in 1:nr){
    ext[[i]]<-raster(nrows=8,ncol=xmin+xmax,xmn=(xy[1,1]-((xmin)*0.0002777778)),xmx=(xy[1,1]+(xmax*0.0002777778)),
                     ymn=(xy[1,2]-(8*0.0002777778)),ymx=(xy[1,2]+(8*0.0002777778)), resolution=c(0.0002777778,0.0002777778),
                     crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                     val=lev.spdf@data[i,]$water_level)
  }
  
  #Crop the DEM to the water level raster extent
  to.sm <- crop(to.dem,ext[[1]],snap="near")    
  extent(to.sm)=extent(ext[[1]])
 
  #Rectify the two extents so you can perform raster math
  if(dim(to.sm)[2]>dim(ext[[1]])[2]){
    for(i in 1:nr){
      ext[[i]]<-raster(nrows=16,ncol=xmin+xmax,xmn=(xy[1,1]-((xmin+1)*0.0002777778)),xmx=(xy[1,1]+(xmax*0.0002777778)),
                       ymn=(xy[1,2]-(8*0.0002777778)),ymx=(xy[1,2]+(8*0.0002777778)), resolution=c(0.0002777778,0.0002777778),
                       crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                       val=lev.spdf@data[i,]$water_level)
    }
  extent(to.sm)=extent(ext[[1]])
  }
  
  #Calculate the water level over the landscape by subtracting elevation from water level
  lev.list<-vector("list",nr)
  for(i in 1:length(ext)){
    lev.list[[i]]<-ext[[i]]-to.sm
  }
  
  rm(list=c("ext"))
  
  #Reclassify the landscape water level raster so <=0 is dry and >0 is inundated
  m<-c(-1000000000,0,0,0.0001,1000000000,1)
  rclmat<-matrix(m,ncol=3,byrow = TRUE)
  
  lev.rcl<-vector("list",nr)
  for(i in 1:length(lev.list)){
    lev.rcl[[i]]<-reclassify(lev.list[[i]], rclmat)
  }
  rm(list=c("lev.list"))
  
  #Separate each year into its own raster
  d <- ymd(lev.spdf@data$Date)
  
  list_of_id <- split(1:nr, year(d))
  
  #Total the number of days inundated per year
  for (i in 1:length(list_of_id)) {
    lev.thisyr <- sum(stack(lev.rcl[list_of_id[[i]]]), na.rm = T)
    
    #Write out yearly days inundated raster files
    out_file <- paste0("C:/Users/starg/Dropbox/Dissertation/Chapter1/Data/GIS/WaterLevelRasters/ADNdata/lev_", station, "_", names(list_of_id)[i], ".tif")
    writeRaster(lev.thisyr, out_file, overwrite=TRUE)
  }
} }

#Choose the relevant stations and x limits
stats <- c(20699000, 20899000, 20950000, 22100000, 22050001, 22220000,
           22190000, 22250000, 23100000, 23150000, 23600000, 23650000, 
           23700000)
xlow <- c(2830, 4307, 4693, 2377, 2851, 4738, 7126, 2183, 1893, 3178, 1284, 2554, 1041)
xhigh <- c(1730, 2614, 1663, 1574, 6060, 3788, 806, 5985, 2740, 1448, 2629, 1656, 2057)

#Aggregate stations and x limits into a data frame
stat.df <- data.frame(station = stats, xmin = xlow, xmax = xhigh)

file.list <- list.files("C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/Cotas", full.names = TRUE)
stat <- c()
for(i in 1:length(file.list)){
  stat <- c(stat, as.numeric(str_extract(file.list[i],"[[:digit:]]+")))
}

#Run the days inundated raster function
for(i in 1:length(stat)){
  foo(stat[i],1000,1000)
  beepr::beep(sound=8)
}

#Change number of days inundated to percent of days inundated
rast.list <- list.files(path="../Data/GIS/WaterLevelRasters/ADNdata", pattern="lev_2205.*tif", full.names = TRUE)
for(i in 1:(length(rast.list)-1)){
  rast <- raster(rast.list[i])
  rast2 <- rast/rast@data@max
  writeRaster(rast2, paste0("../Data/GIS/WaterLevelRasters/ADNdata/PercentRasters/per_",str_extract(rast.list[i], "[[:digit:]]+_[[:digit:]]+"), ".tif"), overwrite = TRUE)
  print(paste0("Completed ", str_extract(rast.list[i], "[[:digit:]]+_[[:digit:]]+")))
}

#Read in percent inundated rasters before/after dam
per.list <- list.files(path="../Data/GIS/WaterLevelRasters/ADNdata/PercentRasters/", full.names=TRUE)

#Read in pre-dam rasteres
before.rast <- list()
for(i in 1:27){
  before.rast[[i]]<-raster(per.list[[i]])
}

#Stack pre-dam rasters and calculate the mean
before.stack <- stack(before.rast)
before.mean <- calc(before.stack, fun=mean)
writeRaster(before.mean, filename="../Data/GIS/WaterLevelRasters/ADNdata/MeanRasters/before_22050001.tif")

#Read in, stack, calculate mean of during construction rasters
during.rast <- list()
for(i in 28:30){
  during.rast[[i-27]]<-raster(per.list[[i]])
}
during.stack <- stack(during.rast)
during.mean <- calc(during.stack, fun=mean)
writeRaster(during.mean, filename="../Data/GIS/WaterLevelRasters/ADNdata/MeanRasters/during_22050001.tif")

#Read in, stack, calculate mean of post Serra da Mesa rasters
after.sdm.rast <- list()
for(i in 31:37){
  after.sdm.rast[[i-30]]<-raster(per.list[[i]])
}
after.sdm.stack <- stack(after.sdm.rast)
after.sdm.mean <- calc(after.sdm.stack, fun=mean)
writeRaster(after.sdm.mean, filename = "../Data/GIS/WaterLevelRasters/ADNdata/MeanRasters/after_sdm_22050001.tif")

#Read in, stack, calculate mean of post Peixe Angical rasters
after.pean.rast <- list()
for(i in 38:47){
  after.pean.rast[[i-37]]<-raster(per.list[[i]])
}
after.pean.stack <- stack(after.pean.rast)
after.pean.mean <- calc(after.pean.stack, fun=mean)
writeRaster(after.pean.mean, filename = "../Data/GIS/WaterLevelRasters/ADNdata/MeanRasters/after_pean_22050001.tif")

#Calculate the difference between pre- and post-damming floodplains
diff.sdm <- after.sdm.mean-before.mean
plot(diff.sdm)
writeRaster(diff.sdm, filename="../Data/GIS/WaterLevelRasters/ADNdata/MeanRasters/diff_sdm_22050001.tif", overwrite=TRUE)

diff.pean <- after.pean.mean-before.mean
plot(diff.pean)
writeRaster(diff.pean, filename="../Data/GIS/WaterLevelRasters/ADNdata/MeanRasters/diff_pean_22050001.tif", overwrite=TRUE)
