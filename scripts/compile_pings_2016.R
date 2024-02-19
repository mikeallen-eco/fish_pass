# Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(here)

# List of files to read
datadir <- here::here("data", "receiver_files", "2016")
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
                                antenna %in% c("A4", "A2") ~ "mismatch",
                              direction == "down" & 
                                antenna %in% c("A2", "A4") ~ "mismatch",
                              TRUE ~ "OK"),
             file = filename)

    # put dataset into list
    file_data_list[[i]] <- data
    
  }
  # make blank row if dataset contains no data
  if(nrow(data_all)==0){
    data <- data.frame(type = NA, date = "2016-01-01", 
                       time = "12:00:00.00", unknown1 = NA,
                       unknown2 = NA, tag = NA, antenna = NA, unknown3 = NA,
                       unknown4 = NA, extra1 = NA, extra2 = NA, extra3 = NA,
                       extra4 = NA, extra5 = NA, direction = NA, flag = "empty",
                       file = filename)
    # put dataset into list
    file_data_list[[i]] <- data
  }
}

# set number of digits for seconds
options(digits.secs=2)
options(digits = 11)

# bind all data files together and format them
data2016 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna, tag, direction, flag) %>%
  # make shorter tag number
  mutate(tag_short = str_sub(tag, - 9, - 1)) %>%
  # format date and time
  mutate(date = as.Date(date),
         datetime = ymd_hms(paste(date, time), tz = "America/New_York")) %>%
  # remove one empty data file: 07_01_16 down.TXT
  # and one ping from 2032: 06_01_16 up.TXT 2032-09-12 05:14:27.00
  # and one ping with no antenna identity: 05_20_16 up.TXT 2016-05-18 19:09:22.45
  filter(flag != "empty",
         date != "2032-09-12",
         antenna != "1") %>%
  # Fixed antenna naming re IFW Antenna Key.xlsx (see Issue #5 on github.com/mikeallen-eco/fish_pass)
  # 2016: A1= Down A1, A2= Down A3, A3=Up A3, A4= Up A1
  mutate(antenna = case_when(antenna == "A1" & direction == "down" ~ "A1",
                             antenna == "A3" & direction == "down" ~ "A2",
                             antenna == "A3" & direction == "up" ~ "A3",
                             antenna == "A1" & direction == "up" ~ "A4",
                             antenna == "A4" ~ "A4",
                             TRUE ~ "unknown"))

# check for antenna naming issues, etc.
unique(data2016$flag)
unique(data2016$tag)
unique(data2016$tag_short)
unique(data2016$date)
hist(yday(data2016$date))
table(data2016$direction)
table(year(data2016$date))

# subset final columns
data2016 <- data2016 %>%
  select(file, date, time, datetime, direction, antenna, tag, tag_short) %>%
  arrange(datetime) %>%
  mutate(year = "2016")

rm(file_data_list, data, data_all, datadir, filename, filepath, files, i)