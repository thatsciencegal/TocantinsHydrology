# library(tidyverse)
# library(reshape2)
# 
# #Put the fluvio data into long format
# fluvio <- fluvio.small.df %>% 
#   select(Date,level,station) %>% 
#   dcast(Date~station, value.var="level")
# 
# #Convert fluvio long format into a matrix and remove the date information
# fl.sm <- as.matrix(fluvio %>% select(-Date))
# 
# #Copy the fluvio data into a new matrix where we can add the missing data
# x <- log(fl.sm+28.1)
# 
# #Calculate the covariance between the stations, ignoring NAs
# fl.var <- cov(x, use="pairwise.complete.obs")
# #Where two stations do not overlap causing NAs, change the variance to 0
# fl.var[is.na(fl.var)]<-0
# #Calculate the mean value for each station
# mu <- colMeanxs(x,na.rm=TRUE)
# 
# for(i in 1:nrow(x)){
#   #Select all cells where there are missing values
#   mis.id <- which(is.na(x[i,]))
#   #Select all cells that have data
#   obs.id <- which(!is.na(x[i,]))
#   #Select the means for stations with missing data
#   mu1 <- mu[mis.id]
#   #Select the means for stations with no missing data
#   mu2 <- mu[obs.id]
#   #Variance between stations with missing observations and observed observations
#   sigma12 <- fl.var[mis.id,obs.id]
#   #Variance between stations with observed observations (inverse)
#   sigma22 <- solve(fl.var[obs.id,obs.id])
#   #Equation for conditional distribution, fills in missing observations
#   x[i,mis.id]<-mu1 + sigma12%*%sigma22%*%(fl.sm[i,obs.id]-mu2)
# }
# x.e <- exp(x)-28.1
# l <- vector("list",47)
# for(i in 1:nrow(fl.sm)){
#   #Select all cells where there are missing values
#   mis.id <- which(is.na(fl.sm[i,]))
#   #Select all cells that have data
#   obs.id <- which(!is.na(fl.sm[i,]))
#   #Select the means for stations with missing data
#   mu1 <- mu[mis.id]
#   #Select the means for stations with no missing data
#   mu2 <- mu[obs.id]
#   #Variance between stations with missing observations and observed observations
#   sigma12 <- fl.var[mis.id,obs.id]
#   #Variance between stations with observed observations (inverse)
#   sigma22 <- solve(fl.var[obs.id,obs.id])
#   #Equation for conditional distribution, fills in missing observations
#   #x[i,mis.id]<-mu1 + sigma12%*%sigma22%*%(fl.sm[i,obs.id]-mu2)
#   l[[i]]<-c(mis.id,obs.id,mu1,mu2,sigma12,sigma22)
# }
# 

library(tidyverse)
library(mvtnorm)
library(stringr)

dat <- read.csv("../Data/fluvio_clean.csv")
head(dat)

fl.sm <- as.matrix(dat %>% select(-X, -Date))
fl.var <- var(fl.sm, use="pairwise.complete.obs")
m <- is.na(fl.var)
colMeans(m)

df <- data.frame(x = rep(1:47, each = 47),
                 y = rep(1:47, 47),
                 m = as.vector(m))
ggplot(df, aes(x, y, fill = m)) +
  geom_tile()

ii <- c(2:5, 13, 16)
fl.sm <- fl.sm[,-ii]
fl.var <- var(fl.sm, use="pairwise.complete.obs")
m <- is.na(fl.var)
colMeans(m)

df <- data.frame(x = rep(1:41, each = 41),
                 y = rep(1:41, 41),
                 m = as.vector(m))
ggplot(df, aes(x, y, fill = m)) +
  geom_tile()

ii <- c(4, 25)
fl.sm <- fl.sm[,-ii]
fl.var <- var(fl.sm, use="pairwise.complete.obs")
m <- is.na(fl.var)
colMeans(m)

df <- data.frame(x = rep(1:39, each = 39),
                 y = rep(1:39, 39),
                 m = as.vector(m))
ggplot(df, aes(x, y, fill = m)) +
  geom_tile()

###
ori <- fl.sm
fl.sm <- ori
min(fl.sm, na.rm = T)
fl.sm <- fl.sm - min(fl.sm, na.rm = T) + 0.1
fl.sm <- log(fl.sm)

x <- fl.sm
fl.var <- var(fl.sm, use="comp")
ind <- (rowMeans(is.na(fl.sm)) == 0) %>% which
mu <- colMeans(fl.sm[ind,],na.rm=T)

diag(fl.var) %>% barplot # one very low variance, will be interesting.
mean(is.na(fl.var)) # 13.7% of the covariance matrix is missing.
eigen(fl.var)$val 

out <- c()
for(i in 1:nrow(fl.sm)){
  mis.id <- which(is.na(fl.sm[i,]))
  obs.id <- which(!is.na(fl.sm[i,]))
  mu1 <- mu[mis.id]
  mu2 <- mu[obs.id]
  sigma12 <- fl.var[mis.id,obs.id]
  sigma22 <- solve(fl.var[obs.id,obs.id])
  if(dmvnorm(fl.sm[i,obs.id], mu2, fl.var[obs.id,obs.id], log = T) - 
     dmvnorm(mu2, mu2, fl.var[obs.id,obs.id], log = T) < -10) {
    out <- c(out, i)
  } else {
    x[i,mis.id] <- mu1 + sigma12 %*% sigma22 %*% (fl.sm[i,obs.id] - mu2)
  }
}
statnames <- colnames(x)
statnames <- str_replace(statnames, "X", "")

kept.stat <- fluvio.small.df %>% filter(station %in% statnames)
rem.stat <- fluvio.small.df %>% filter(!station %in% statnames)

site.ext <- readOGR(dsn="../Data", layer = "hybas_study_area_dsv")
site.crs <- proj4string(site.ext)
xy <- kept.stat[,c("lon","lat")]
kept.stat.gis <- SpatialPointsDataFrame(coords=xy, data=kept.stat, 
                                    proj4string = CRS(site.crs))

xy2 <- rem.stat[,c("lon","lat")]
rem.stat.gis <- SpatialPointsDataFrame(coords = xy2, data = rem.stat, proj4string = CRS(site.crs))

kept.stat2 <- kept.stat %>%
  filter(station%in% statnames[iii]) %>%
  select(station, lon, lat) %>%
  distinct()
coordinates(kept.stat2) <- ~ lon + lat

plot(kept.stat2, add = T, col="red")
plot(site.ext)

writeOGR(kept.stat.gis, dsn = "C:/Users/starg/Dropbox/Dissertation/Chapter1/Data", layer = "comp_stations", driver="ESRI Shapefile" )
writeOGR(rem.stat.gis, dsn = "C:/Users/starg/Dropbox/Dissertation/Chapter1/Data", layer = "rem_stations", driver ="ESRI Shapefile")

stats <- colnames(dat)
stats <- str_replace(stats, "X", "")
