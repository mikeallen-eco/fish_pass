# Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(wesanderson)
library(lubridate)
library(here)

# List of files to read
datadir <- here::here("data", "receiver_files", "2019")
files <- list.files(datadir)

# create empty list to contain formated data
file_data_list <- list()

# Loop through files and format data
for(i in 1:length(files)){
  # Read data
  filename <- files[i]
  print(paste(i, filename))
  filepath <- paste(datadir, filename, sep="/")
  data_all <- read.table(filepath, col.names=c("type", "date", "time",
                                               "unknown1", "unknown2", 
                                               "tag", "antenna", 
                                               "unknown3", "unknown4",
                                               paste("extra", 1:5, sep="")),
                         fill=T, as.is=T, na.strings="") %>%
    # Type: D (data) or E (error/info) or B (bad - only one file)
    filter(type == "D")
  
  # Only do the following if there is data
  if(nrow(data_all)>0){
    
    # Up or down file?
    data <- data_all %>%
      mutate(direction = case_when(grepl("up|Up|UP", 
                                         filename) ~ "up",
                                   grepl("down|Down|DOWN", 
                                         filename) ~ "down",
                                   TRUE ~ "direction unknown"),
             # flag antenna names that don't match expectations
             flag = case_when(direction == "up" & 
                                antenna %in% c("A1", "A2") ~ "mismatch",
                              direction == "down" & 
                                antenna %in% c("A3", "A4") ~ "mismatch",
                              TRUE ~ "OK"),
             file = filename)

    # Merge datasets
    file_data_list[[i]] <- data
    
  }
  
}

# set number of digits for seconds
options(digits.secs=2)
options(digits = 11)

# bind all data files together and format them
data2019 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna, tag, direction, flag) %>%
  # make shorter tag number
  mutate(tag_short = str_sub(tag, - 9, - 1)) %>%
  # format date and time
  mutate(date = as.Date(date),
         datetime = ymd_hms(paste(date, time), tz = "America/New_York"))

# check for antenna naming issues, etc.
unique(data2019$flag)
unique(data2019$tag)
unique(data2019$tag_short)
unique(data2019$date)
hist(yday(data2019$date))
table(data2019$direction)

# subset final columns
data2019 <- data2019 %>%
  select(file, date, time, datetime, direction, antenna, tag, tag_short) %>%
  arrange(datetime) %>%
  mutate(year = "2019")

rm(file_data_list, data, data_all, datadir, filename, filepath, files, i)