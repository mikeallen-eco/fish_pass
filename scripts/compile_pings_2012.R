# Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(here)

# List of files to read
datadir <- here::here("data", "receiver_files", "2012")
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
                                               paste("extra", 1:6, sep="")),
                         fill=T, as.is=T, na.strings="")
  
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
    data <- data.frame(date = "2012-01-01", 
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
data2012A1 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna, tag, direction = type, flag = type) %>%
  # subset just pings labeled A and exclude 6/25 file
  filter(flag == "A", 
         file != "06_25_12.TXT") 

# special one for file 06_25_12.TXT which had a weird format
data2012A2 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna = tag, tag = unknown2, 
         direction = type, flag = type) %>%
  # subset just pings labeled A
  filter(flag == "A", 
         file == "06_25_12.TXT") 

# Next do the pings labeled "R" (except 6/25 file which is different)
data2012R1 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna = unknown3, tag = unknown2, 
         direction = type, flag = type) %>%
  # subset just pings labeled R and exclude 6/25 file
  filter(flag == "R",
         file %notin% c("06_25_12.TXT", "06_14_12.TXT")) 

# Next do the pings labeled "R" just the 6/25 and 6/14 files which are different
data2012R2 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna = tag, unknown3, tag = unknown2, 
         direction = type, flag = type) %>%
  # subset just pings labeled R and include only the 6/25 file
  filter(flag == "R",
         file %in% c("06_25_12.TXT", "06_14_12.TXT")) %>%
  mutate(antenna = case_when(antenna %notin% c("A1", "A2") ~ unknown3,
                             TRUE ~ antenna)) %>%
  select(-unknown3)

# combine A and R pings
data2012 <- data2012A1 %>%
  bind_rows(data2012A2) %>%
  bind_rows(data2012R1) %>%
  bind_rows(data2012R2) %>%
  # make shorter tag number
  mutate(tag_short = str_sub(tag, - 9, - 1)) %>%
  # format date and time
  mutate(datetime = mdy_hms(paste(date, time), tz = "America/New_York"),
         date = mdy(date)) %>%
  # rename antennas as needed (see Issue #5 on github.com/mikeallen-eco/fish_pass)
  # 2012: A1=A2, A4=A1, this year only 1 receiver box was run, so no up/down files
  mutate(
    antenna_original = antenna,
    antenna = case_when(antenna == "A2" ~ "A1",
                        antenna == "A1" ~ "A4",
                        TRUE ~ "unknown")) %>%
  # fix date on 2 pings from the year 2030
  # 04_05_12.TXT 11/10/2030 22:47:55.00
  # 04_18_12.TXT 11/10/2030 22:47:55.00
  mutate(datetime = case_when(
                year(datetime) == 2030 ~ mdy_hms("04/17/2012 22:47:55.00",
                                     tz = "America/New_York"),
                TRUE ~ datetime),
         date = case_when(year(date) == 2030 ~ ymd("2012-04-17"),
                          TRUE ~ date)) %>%
  filter(year(datetime) != 2011)

# check for antenna naming issues, etc.
unique(data2012$antenna)
unique(data2012$antenna_original)
unique(data2012$tag)
unique(data2012$tag_short)
sort(unique(data2012$date))
hist(yday(data2012$date))
table(data2012$direction)
table(year(data2012$date))

# subset final columns
data2012 <- data2012 %>%
  select(file, date, time, datetime, direction, antenna, tag, tag_short) %>%
  arrange(datetime) %>%
  mutate(year = "2012") %>%
  # remove one fish that was never actually detected; only test pings (see issue #28)
  filter(tag_short != "166994725")

rm(file_data_list, data, data_all, datadir, filename, filepath, files, i,
   data2012A1, data2012A2, data2012R1, data2012R2)