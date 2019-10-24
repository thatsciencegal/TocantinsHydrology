library(stringr)
library(dplyr)

file.list <- list.files("./", full.names = TRUE)
meta <- read.csv("C:/Users/starg/Dropbox/Dissertation/Chapter1/hidroweb_metadata(3).csv")

#Add the lat and long coordinates for each file in the directory
for(i in seq_along(file.list)){
  stat <- as.numeric(str_extract(file.list[i],"[[:digit:]]+"))
  dat <- read.csv(file.list[i])
  dat$station <- stat
  lon <- meta %>% filter(station==stat) %>% select(lon)
  lat <- meta %>% filter(station==stat) %>% select(lat)
  dat$lon <- lon[1,1]
  dat$lat <- lat[1,1]
  write.csv(dat, paste0("C:/Users/starg/Dropbox/Dissertation/Chapter1/Data/WaterLevel/ANA_",stat,".csv"))
}
