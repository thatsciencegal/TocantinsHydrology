write.na<-function(){
  ##Function to write the start and end dates for 
  in_file<-paste0("C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/Cotas/Cotas_", stat[i], ".csv")
  dat <- read.csv(in_file)
  dat$nas <- as.integer(as.logical(is.na(dat$Water.Level..cm.)))
  dat$nas_lead <- lead(dat$nas, n=1)
  dat$nas_diff <- dat$nas_lead-dat$nas
  na_start <- filter(dat, lag(nas_diff==1)) %>% select(Date)
  names(na_start) <- "StartDate"
  na_end<-(filter(dat, nas_diff==1)) %>% select(Date)
  names(na_end) <- "End Date"
  dat2 <- cbind(na_start,na_end)
  
  write.csv(dat2,file=paste0("./NA_Runs/na_run_",stat[i],".csv"))
}

#Get list of files that we're doing the NA operation on
file.list <- list.files("C:/Users/starg/Dropbox/Dissertation/Data/Hydrology/Cotas", full.names = TRUE)

#Empty vector for storing station names
stat <- c()

#Get just the station names from the larger string
for(i in 1:length(file.list)){
  stat <- c(stat, as.numeric(str_extract(file.list[i],"[[:digit:]]+")))
}

#Find the consecutive NA values for each dataset
for(i in 1:length(stat)){
  write.na()
}
