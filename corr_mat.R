##Individual station gap filler for < 30 days

library(stringr)
library(corrplot)
library(corrr)
library(tidyverse)
library(Hmisc)

#Make a list of files
lev.list <- list.files("C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/Cotas", full.names = TRUE)

#Extract station numbers from the list of files
station <- as.numeric(str_extract(lev.list,"[[:digit:]]+"))

#Read in the CSV files and store them in a list
lev <- lapply(lev.list, function(i) {read.csv(i)})

#Add station number to each water level data frame
lev <- mapply(cbind, lev, "Station" = station, SIMPLIFY = FALSE)

#Put all the water levels together in a large data frame
all.lev <- bind_rows(lev)

#Change dates to date format
all.lev$Date <- as.Date(all.lev$Date, "%m/%d/%Y")

#Create correlation matrix of water level and stations
all.lev.sm <- all.lev %>%  select(-Lat,-Long) %>% 
  spread(key = Station, value = Water.Level..cm.) %>% 
  select(-Date) %>% 
  correlate(use = "pairwise.complete.obs")

#Write out correlation matrix
write.csv(all.lev.sm, "../Data/WaterLevel/ADNDownload/Correlations/cor_mat.csv")

#Linear model to gap fill data for first location
#read in data file to be gap filled
names(all.lev.sm)
dat1 <- lev[[16]]
head(dat1)

dat2 <- lev[[27]]

#Join 2 files together (not necessary)
wat.lev <- inner_join(dat1,dat2, by="Date")

#Linear water level model
wat.lev.model <- lm(wat.lev$Water.Level..cm..x~wat.lev$Water.Level..cm..y)
summary(wat.lev.model)

#Separate water level of the station of interest
wat.lev3 <- wat.lev[,1:5]
names(wat.lev3) <- c("Date","Level","Lat","Long","Station")

#Make a data frame of the two variables
wat.lev.sm<-wat.lev %>% select(Water.Level..cm..x,Water.Level..cm..y)
wat.lev.pred<-predict(wat.lev.model,wat.lev.sm)

wat.lev3$Level[is.na(wat.lev3$Level)]<-wat.lev.pred[is.na(wat.lev3$Level)]

wat.lev3$Date<-as.Date(wat.lev3$Date, "%m/%d/%Y")

missing <- which(is.na(wat.lev$Water.Level..cm..x))

plot(wat.lev$Water.Level..cm..x~wat.lev$Date)
points(wat.lev3$Level[missing]~wat.lev3$Date[missing],col="red")
