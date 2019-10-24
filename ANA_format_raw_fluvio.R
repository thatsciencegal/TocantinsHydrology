####################################
### Format RAW ANA data to ADN  ####
### Pamela Senesi               ####
### 11/21/2018                  ####
####################################

##Edited for water level data by Christine Swanson
##2018-05-22

#install.packages("tidyr")
library(tidyr)
library(dplyr)
library(readxl)

ANA_raw_dir = "E:/Dissertation/Aim 1/Data/WaterLevel"
ANA_final_dir = "C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/CleanANADownload"
ana_fluvio = list.files(ANA_raw_dir, full.names=TRUE)[-1]

for (i in 1:length(ana_fluvio))  {
headers <- read.csv(ana_fluvio[i],header=F, skip = 13,nrows=1,as.is=T)
df=read.csv(ana_fluvio[i],skip=14,header=F)

#df <- read.csv("C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/cotas_C_22050001/cotas_C_22050001.csv")
#Merge all columns into one for all rows and then separate by semicolon
head = as.character(headers)
head = strsplit(head,";")
head = as.data.frame(head)
head = t(head)

clean_data <- df %>%  ##to extract ALL data
  unite(newandclean, everything()) %>%
  separate(newandclean, head, sep = ";")

cotas <- clean_data %>%  ##to extract just vazoes with date and station code
  select(EstacaoCodigo, Data, Cota01:Cota31)
  
colnames(cotas) <- c("station", "date_measured", 1:31)
cotas[cotas == ""]   <- NA
#
cotas <- cotas %>%
  gather(level, key = "day", 3:33, na.rm = TRUE) %>% 
  distinct(.keep_all=TRUE)


cotas$date_measured <- strptime(as.character(cotas$date_measured), "%d/%m/%Y") 
cotas<-cotas %>%
  separate(date_measured, c("y", "m", "d"), sep="-") 

cotas$d <- NULL
#cotas$station <- NULL

cotas <- cotas %>%
  unite(Date, c("day", "m", "y"), sep = "-")

cotas$Date<-as.Date(cotas$Date, "%d-%m-%Y")
cotas <- arrange(cotas,Date)
#setwd(ANA_final_dir)
station_code <- substr(ana_fluvio,47,54)
write.csv(cotas, paste0("C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/CleanANADownload/lev_",station_code[i],".csv"))
#write.csv(cotas, file = paste0(ANA_final_dir,"/ANA_",station_code[i], ".csv"), row.names = FALSE)
print(i)
beepr::beep_on_error(9)
beepr::beep(1)
}
