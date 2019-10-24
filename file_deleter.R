#Delete directory files that have end date less than 2006
library(lubridate)
files <- list.files("C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/CleanANADownload/", full.names=TRUE)

for(i in seq_along(files)){
  dat <- read.csv(files[i])
  if(max(year(ymd(dat$Date)), na.rm=TRUE)<2006){
    unlink(files[i]) #Deletes directory file
  }
  stat <- as.numeric(str_extract(files[i],"[[:digit:]]+"))
  print(paste0(i," ",stat))
}
