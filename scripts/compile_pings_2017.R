# Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(here)

# List of files to read
datadir <- here::here("data", "receiver_files", "2017")
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
data2017 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna, tag, direction, flag) %>%
  # correct 2 reading errors manually from these three lines
  # 6_26_17 Down.TXT file at 2017-06-26 09:36:39.71
  # 6_26_17 Down.TXT file at 2017-06-25 21:26:28.43
  # 6_26_17 Down.TXT file at 2017-06-24 19:11:43.60
  mutate(tag = case_when(tag == "1" ~ "000000180843403", 
                         tag == "A1" ~ "0000_0000000180843403",
                         tag == "00šúÁ000000180843440" ~ 
                           "0000_0000000180843440",
                         TRUE ~ tag),
         antenna = case_when(antenna %in% c("2", "4440") ~ "A1", 
                             TRUE ~ antenna)) %>%
  # delete glitch row
  # 5_15_17 Down 2.TXT 2019-03-02 17:05:43.25
  filter(antenna != "0000") %>%
  # remove one weird ping from 2018-06-02
  filter(date != "2018-06-02") %>%  
  # make shorter tag number
  mutate(tag_short = str_sub(tag, - 9, - 1)) %>%
  # format date and time
  mutate(date = as.Date(date),
         datetime = ymd_hms(paste(date, time), tz = "America/New_York"))

# check for antenna naming issues, etc.
unique(data2017$flag)
unique(data2017$tag)
unique(data2017$tag_short)
unique(data2017$date)
hist(yday(data2017$date))
table(data2017$direction)
table(year(data2017$date))

# subset final columns
data2017 <- data2017 %>%
  select(file, date, time, datetime, direction, antenna, tag, tag_short) %>%
  arrange(datetime) %>%
  mutate(year = "2017")

rm(file_data_list, data, data_all, datadir, filename, filepath, files, i)
