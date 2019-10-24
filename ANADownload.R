##############################
## Script to download and 
## ProcesS ANA Data 
## Shar Siddiqui 5/1/2019
##############################

## Load Libraries
library(httr)
library(xml2)
library(reshape2)

## Import metadata
setwd("D:/Research/Projects/Catchment Classification/flow") #metadata file should be in here
meta <- read.csv("../hidroweb_metadata(3).csv")
meta <- meta[which(meta$class=="Fluviometric"),] #14656 total Q stations
meta <- meta[which(meta$basin=="RIO TOCANTINS"),] #1356 Q stations in the Amazon
meta <- meta[,c(1:14)]

##Access ANA Database
stationCodes = as.character(meta$station)
for (i in 1:length(stationCodes)) {
  codEstacao = stationCodes[i]
  bodyPOST = paste0("codEstacao=",codEstacao,"&dataInicio=01/01/1900&dataFim=&tipoDados=1&nivelConsistencia=2")
  b2 <- "http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica"
  xml = POST(b2, body = bodyPOST, content_type("application/x-www-form-urlencoded"))
  xml.doc = content(xml)
  
  ##Extract data from list object
  xml.list <- xml2::as_list(xml.doc) #WOWOWOWOW
  xml.data <- xml.list$DataTable$diffgram$`DocumentElement` #207 elements represent 207 months, from most recent to 2001 about 17 years this makes sense
  
  station.data <- matrix(ncol=length(xml.data),nrow = 31) #we want to make a data frame with 31 rows and length(xml.data columns)
  if (length(is.na(station.data))==31) {
    next
    print("Skipping this iteration")
  } #if no data for this station go onto next iteration
  for (j in 1:length(xml.data)) { #j for each record, rows 1:LOD
    month <- xml.data[[j]] #represents month of which we want to extract the 28-31 flows
    df <- as.data.frame(unlist(month[16:46])) #need to find a way of putting each value from 1 to 31 or NA
    if(length(df)==0) next
    station.data[1:length(df[,1]),j] <- as.numeric(as.character(df[,1])) 
  } 
  
  stationData <- melt(station.data)
  colnames(stationData) <- c("day","month","level")
  
  ## Add dates (get this info from the original list object)
  dates = list()
  for (k in 1:length(xml.data)) {
    date = xml.data[k]$SerieHistorica$DataHora
    date = as.character(date)
    dates[[k]] <- date
  } #now we have our YYYY-MM vectors to append to stationData!
  
  ##Add years, months and combine to final date
  stationData$year <- substr(dates,1,4)
  stationData$month <- substr(dates,6,7)
  stationData$date <- as.Date(with(stationData,paste(month,day,year,sep="/")),"%m/%d/%Y")
  
  ##note: all the NA dates that come out are days that don't exist (i.e. 2/31) Yet many of them have flow values
  stationData <- stationData[-c(which(is.na(stationData$date))),] ##for now assume, this data was extrapolated an remove
  ##sort by date and remove DMY cols
  stationData <- stationData[order(stationData$date),]
  stationData <- stationData[,-which(names(stationData) %in% c("day","month","year"))]
  stationData <- stationData[,c(2,1)]
  
  #write to .csv
  #setwd("../Data/WaterLevel/")
  outName = paste0("C:/Users/starg/Dropbox/Dissertation/Chapter1/Data/WaterLevel/ANA_",codEstacao,".csv")
  write.csv(stationData,outName)
  print(i)
}
