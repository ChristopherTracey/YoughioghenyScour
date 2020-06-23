library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)
library(exifr) # for getting exif information from the cameras
library(suncalc)
library(dataRetrieval)
library(jpeg)
library(grid)

# options for a run
sitename <- "ferncliff2"
latitude <- 39.86417
longitude <- -79.49722 

# check to see if an output folder exists and if not create one.
if (!dir.exists(here::here("data","sites",sitename,"output"))){
  dir.create(here::here("data","sites",sitename,"output"))
} else {
  print("Output directory already exists!")
}

#######################################################################################################################
# read exif information from the camera
files <- list.files(here::here("data","sites",sitename), recursive=TRUE, pattern="JPG", full.names=TRUE)
print(paste(length(files),"image files were found in the",sitename,"directory", sep=" "))
exifinfo <- read_exif(files)  # read the exif data from the camera.

# extract the image sizes for later use
imageWidth <- unique(exifinfo$ImageWidth)
imageHeight <- unique(exifinfo$ImageHeight)

if(length(imageHeight>1|imageWidth>1)){
  print("There are two different size images. We'll resize them later in the process.")
  resizeHeight <- min(imageHeight)
  resizeWidth <- min(imageWidth)
  exifinfo$flagResize <- ifelse(exifinfo$ImageWidth==resizeWidth&exifinfo$ImageHeight==resizeHeight,"no","yes")
  table(exifinfo$flagResize)
} else {
  print("All images are the same size. Good to go!")
}




exifinfo <- exifinfo[c("FileName","Directory","FileSize","FileModifyDate","GPSLatitude","GPSLongitude")] # drop the unneeded fields

# properly format the date
exifinfo$datetime <- ymd_hms(exifinfo$FileModifyDate) # convert to date format
exifinfo$datetime <- with_tz(exifinfo$datetime, "America/New_York") # convert to eastern time

#######################################################################################################################
# filter by various factors

# data range
date_start <- min(exifinfo$datetime)
date_end <- max(exifinfo$datetime)
 
totalhours <- interval(date_start, date_end) %>% as.numeric('hours')
estimatephotos <- ceiling(totalhours/4)
  
# get local sunrise/sunset time
daylight <- getSunlightTimes(date=as.Date(exifinfo$datetime), lat=latitude, lon=longitude, keep=c("sunrise", "sunset"), tz="America/New_York")
daylight <- unique(daylight) # get sunrise/sunset for each day
daylight <- daylight[c("date","sunrise","sunset")]

exifinfo$day <- date(exifinfo$datetime)

exifinfo <- merge(exifinfo, daylight, by.x="day", by.y="date")

exifinfo$daylight <-ifelse(exifinfo$datetime>=exifinfo$sunrise&exifinfo$datetime<=exifinfo$sunset, "yes", "no")

table(exifinfo$daylight)

# find the photos taken at 15minute intervals
exifinfo$intervalshot <- NA
exifinfo$intervalshot <- ifelse( minute(exifinfo$datetime) %in% c(0,15,30,45)   , "yes", "no")

table(exifinfo$intervalshot,exifinfo$daylight)

suitablePhotos <- exifinfo[which(exifinfo$intervalshot=="yes"&exifinfo$daylight=="yes"),]



# get the earliest dates

datetime_start <- min(suitablePhotos$datetime)
datetime_end <- max(suitablePhotos$datetime)
date_start <- date(datetime_start)            
date_end <- date(datetime_end)            









######################################################################################################################
# function to get gage data

siteNumber <- "03081500"
#parameterCd <- "00060"  # Discharge
parameterCd <- "00065" # gage height
startDate <- date_start 
endDate <- date_end 
dischargeUnit <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
dischargeUnit <- renameNWISColumns(dischargeUnit)

#subset discharge units by photo time period
dischargeUnit <- dischargeUnit[which(dischargeUnit$dateTime>=datetime_start&dischargeUnit$dateTime<=datetime_end),]


##########
# make graphs

df <- merge(dischargeUnit, suitablePhotos, by.x="dateTime", by.y="datetime") #, all.x=TRUE
# NOTE, probably should add something about all.x=true to properly format the graphs

# get the number of photos so we can properly pad the file names so things sort correctly...
padlength <- nchar(nrow(df)) # used below in ggsave


df <- df[order(df$dateTime),] 

df_gagemin <- min(floor(df$GH_Inst))
df_gagemax <- max(ceiling(df$GH_Inst))
df_datemin <- min(df$dateTime)
df_datemax <- max(df$dateTime)

save.image(file = "my_work_space.RData")
#load("my_work_space.RData")



for(i in 1:nrow(df)){
  df1 <- df[1:i,]
  img <- readJPEG(paste(df$Directory[i],df$FileName[i], sep="/")) 
  gpp <- rasterGrob(img, interpolate=TRUE) 
  # gpp$width <- unit(1, "in") 
  # gpp$height <- unit(1, "in")
  a <- ggplot(df1,aes(x=dateTime,y=GH_Inst)) + 
    xlab("Date") +
    ylab("Gage Height (feet)") +
    annotation_custom(gpp) +
    geom_line(color='red2', size=2) + 
    expand_limits(x=c(df_datemin,df_datemax),y=c(df_gagemin,df_gagemax)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    #theme(panel.border=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line=element_line(colour="black"))
  ggsave(filename = paste(here::here("data","sites",sitename,"output"), paste0("photo", str_pad(i, padlength, pad="0"),".jpg"),sep = "/"))
  print(paste("photo", i, "of", nrow(df), "saved"), sep=" ")
}

# make a movie
library(av)
outputfiles <- list.files(here::here("data", "sites", sitename, "output"), recursive=TRUE, pattern="jpg", full.names=TRUE)
av_encode_video(outputfiles, output=here::here("data", "sites", sitename, "output", paste(sitename, "_20200601.mp4", sep="")), framerate=12)

