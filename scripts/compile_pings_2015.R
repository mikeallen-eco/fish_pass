### NOTE: changed "fileEncoding = "437" in read.table below so it would work on new Mac
### based on this: https://stackoverflow.com/questions/4806823/how-to-detect-the-right-encoding-for-read-csv
### might need to change for it to work on another machine
### USED THIS TO DEBUG PROBLEM WITH ENCODING FOLLOWING THE WEBSITE ABOVE
# codepages <- setNames(iconvlist(), iconvlist())
# x <- lapply(codepages, function(enc) try(read.table(filepath,
#                                                     fileEncoding=enc,
#                                                     nrows=3, header=TRUE, sep="\t"))) # you get lots of errors/warning here

# Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(wesanderson)
library(lubridate)
library(here)

# List of files to read
datadir <- here::here("data", "receiver_files", "2015")
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
                         fill=T, as.is=T, na.strings="", fileEncoding = "437") %>%
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
    data <- data.frame(type = NA, date = "2015-01-01", 
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
data2015 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna, tag, direction, flag) %>%
  # make shorter tag number
  mutate(tag_short = str_sub(tag, - 9, - 1)) %>%
  # format date and time
  mutate(date = as.Date(date),
         datetime = ymd_hms(paste(date, time), tz = "America/New_York")) %>%
  # Fixed antenna naming re IFW Antenna Key.xlsx (see Issue #5 on github.com/mikeallen-eco/fish_pass)
  # 2015: A1= Down A3, A2= Down A1, A3=Up A3, A4= Up A1
  mutate(antenna = case_when(antenna == "A3" & direction == "down" ~ "A1",
                             antenna == "A1" & direction == "down" ~ "A2",
                             antenna == "A3" & direction == "up" ~ "A3",
                             antenna == "A1" & direction == "up" ~ "A4",
                             TRUE ~ "unknown"))

# check for antenna naming issues, etc.
sort(unique(data2015$antenna))
sort(unique(data2015$tag))
sort(unique(data2015$tag_short))
sort(unique(data2015$date))
hist(yday(data2015$date))
table(data2015$direction)
table(data2015$flag)
table(year(data2015$date))

# subset final columns
data2015 <- data2015 %>%
  select(file, date, time, datetime, direction, antenna, tag, tag_short) %>%
  arrange(datetime) %>%
  mutate(year = "2015")

rm(file_data_list, data, data_all, datadir, filename, filepath, files, i)
