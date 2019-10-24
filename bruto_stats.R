##Calculate summary statistics for bruto data
library(tidyverse)

bruto<-list.files("../Data/WaterLevel1/", pattern="lev_*",full.names = TRUE)

for(i in 1:length(bruto)){
  #Get mean and standard deviation of bruto data for each date
  dat <- read.csv(bruto[i]) %>% 
    group_by(date) %>% 
    mutate(mean_lev = mean(discharge), sd_lev = sd(discharge)) %>% 
  write.csv(dat, paste0("../Data/WaterLevel1/mutated/",dat$station[1],".csv"))
}

brut2<-list.files("../Data/WaterLevel1/mutated/", full.names=TRUE)

for(i in 1:length(brut2)){
  dat2 <- read.csv(brut2[i]) %>% group_by(date)
  if(nrow(dat2)==0){
    print("Skipping this iteration")
    next
  }
  #Writes out dates in which the standard deviation is larger than the mean standard deviation
  #for the dataset
  dat3 <- as.data.frame(dat2[which(dat2$sd_lev>mean(dat2$sd_lev, na.rm=TRUE)),])
  write.csv(dat3, paste0("../Data/WaterLevel1/bruto_stats/",dat3$station[1],".csv"))
  png(filename=paste0(dat2$station[1],"_mean.png"), width=960)
  plot(dat2$mean_lev~dat2$date)
  lines(dat2$mean_lev~dat2$date)
  dev.off()
}

to.lev <- read.csv("../Data/WaterLevel1/tocantins_lev.csv")
site.crs<-proj4string(to)
xy <- to.lev[c("lon","lat")]

to.lev.sp <- SpatialPointsDataFrame(coords=xy, data=to.lev, proj4string = CRS(site.crs))
