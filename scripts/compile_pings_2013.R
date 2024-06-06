# Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(here)

# List of files to read
datadir <- here::here("data", "receiver_files", "2013")
files <- list.files(datadir)

# create empty list to contain formated data
file_data_list <- list()

# Loop through files and format data
for(i in 1:length(files)){
  # Read data
  filename <- files[i]
  print(paste(i, filename))
  filepath <- paste(datadir, filename, sep="/")
  data_all <- read.table(filepath, col.names=c("date", "time",
                                               "unknown1", "type", "unknown2", 
                                               "unknown3", "unknown4",
                                               "tag", "antenna", 
                                               paste("extra", 1:5, sep="")),
                         fill=T, as.is=T, na.strings="") #%>%
  # Type: D (data) or E (error/info) or B (bad - only one file)
  #filter(type == "D")
  
  # Only do the following if there is data
  if(nrow(data_all)>0){
    
    # Up or down file?
    data <- data_all %>%
      mutate(file = filename)
    
    # put dataset into list
    file_data_list[[i]] <- data
    
  }
  # make blank row if dataset contains no data
  if(nrow(data_all)==0){
    data <- data.frame(date = "2015-01-01", 
                       time = "12:00:00.00", unknown1 = NA, type = NA, 
                       unknown2 = NA, unknown3 = NA, unknown4 = NA, tag = NA, 
                       antenna = NA, extra1 = NA, extra2 = NA, extra3 = NA,
                       extra4 = NA, extra5 = NA,
                       file = filename)
    # put dataset into list
    file_data_list[[i]] <- data
  }
}

# set number of digits for seconds
options(digits.secs=2)
options(digits = 11)

# bind all data files together and format them
# First do the pings labeled "A"
data2013A <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna, tag, direction = type, flag = type) %>%
  # subset just pings labeled A
  filter(flag == "A") 

# Next do the pings labeled "R"
data2013R <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna = unknown3, tag = unknown2, 
         direction = type, flag = type) %>%
  # subset just pings labeled R
  filter(flag == "R" | flag == "W")

# combine A and R pings
data2013 <- data2013A %>%
  bind_rows(data2013R) %>%
  # make shorter tag number
  mutate(tag_short = str_sub(tag, - 9, - 1)) %>%
  # format date and time
  mutate(datetime = mdy_hms(paste(date, time), tz = "America/New_York"),
         date = mdy(date)) %>%
  # rename antennas as needed (see Issue #5 on github.com/mikeallen-eco/fish_pass)
  # 2013: A1=A2, A4=A1, this year only 1 receiver box was run, so no up/down files
  # there are A3 pings here, so I left them in for now
  mutate(
    antenna_original = antenna,
    antenna = case_when(antenna == "A2" ~ "A1",
                        antenna == "A1" ~ "A4",
                        antenna == "A3" ~ "A1", # changing A3->A1 per issue #17
                        TRUE ~ "unknown")) %>%
  # removing 3 pings with tag typos (but good data on either side)
  # 04_23_13.TXT 2013-04-03 16:08:25.46  
  # 06_17_13.TXT 2013-06-15 11:54:15.80
  # or with no data at all: 06_25_13.TXT 2013-06-25 08:28:11.35
  filter(antenna_original != "0") %>%
  filter(is.na(antenna_original) != TRUE)

# check for antenna naming issues, etc.
unique(data2013$antenna)
unique(data2013$antenna_original)
unique(data2013$tag)
unique(data2013$tag_short)
unique(data2013$date)
hist(yday(data2013$date))
table(data2013$direction)
table(year(data2013$date))

# subset final columns
data2013 <- data2013 %>%
  select(file, date, time, datetime, direction, antenna, tag, tag_short) %>%
  arrange(datetime) %>%
  mutate(year = "2013")

rm(file_data_list, data, data_all, datadir, filename, filepath, files, i)
rm(data2013A, data2013R)
