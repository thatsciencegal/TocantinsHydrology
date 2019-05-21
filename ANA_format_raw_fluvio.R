####################################
### Format RAW ANA data to ADN  ####
### Pamela Senesi               ####
### 11/21/2018                  ####
####################################

install.packages("tidyr")
library(tidyr)
library(dplyr)
library(readxl)

ANA_raw_dir = "E:/Dissertation/Aim 1/Data/WaterLevel"
ANA_final_dir = "E:/Dissertation/Aim 1/Data/WaterLevel/Clean"
ana_fluvio = list.files(ANA_raw_dir)
ana_fluvio = ana_fluvio[-1]

for (i in 1:length(ana_fluvio))  {
setwd(ANA_raw_dir)
headers <- read.csv(ana_fluvio[i],header=F, skip = 13,nrows=1,as.is=T)
df=read.csv(ana_fluvio[i],skip=14,header=F)

#Merge all columns into one for all rows and then separate by semicolon
head = as.character(headers)
head = strsplit(head,";")
head = as.data.frame(head)
head = t(head)

clean_data <- df %>%  ##to extract ALL data
  unite(newandclean, everything()) %>%
  separate(newandclean, head, sep = ";")

cotas <- clean_data %>%  ##to extract just vazoes with date and station code
  select(EstacaoCodigo, Data, MediaDiaria, Maxima, Minima, Media)
  
colnames(cotas) <- c("station", "date_measured", "daily_mean", "max", "min", "mean")
cotas[cotas == ""]   <- NA
#
#cotas <- cotas %>%
  #gather(flow, key = "date_measured", na.rm = TRUE) 
  
  #Replace all underscores with periods.
 # cotas$flow <- gsub("_", ".", cotas$flow) 

cotas$date_measured <- strptime(as.character(cotas$date_measured), "%d/%m/%Y")
#cotas %>%
  #separate(date_measured, c("d", "month", "year"), sep="/") 

#cotas$d <- NULL
#cotas$station <- NULL

#cotas <- cotas %>%
  #unite(Date, c("d", "month", "year"), sep = "")

#setwd(ANA_final_dir)
station_code <- substr(ana_fluvio,9,16)
setwd(ANA_final_dir)
write.csv(cotas, file = paste0("ANA_",station_code[i], ".csv"), row.names = FALSE)
print(i)
}

