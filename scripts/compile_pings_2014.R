# Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(here)

# List of files to read
datadir <- here::here("data", "receiver_files", "2014")
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
data2014 <- do.call(rbind, file_data_list) %>%
  select(file, date, time, antenna, tag, direction, flag) %>%
  # make shorter tag number
  mutate(tag_short = str_sub(tag, - 9, - 1)) %>%
  # remove weird record from 05_12_14.TXT 2019-01-08 V8:43:79.39
  filter(time != "V8:43:79.39") %>%
  # format date and time
  mutate(date = as.Date(date),
         datetime = ymd_hms(paste(date, time), tz = "America/New_York")) %>%
  # rename antennas as needed (see Issue #5 on github.com/mikeallen-eco/fish_pass)
  # 2014: A1= A2, A4=A1, this year we used one receiver box for the most part, so let's assume A1= A2, A4=A1 for now, but then it switched to two: if latter file name has A1, assume it's the receiver for A1 (update to A4), if file name has A2, assume it's updated antenna A1.
  # for now: just changing A2->A1 and A1->A4
  mutate(
    antenna_original = antenna,
    antenna = case_when(antenna == "A2" ~ "A1",
                        antenna == "A1" ~ "A4",
                        TRUE ~ "unknown")) %>%
  # to make list of problematic tag numbers do this here
  # bad_index <- is.na(as.numeric(gsub(data2014$tag, pattern = "_", replacement = "")))
  # data2014$tag[bad_index]
  #  removing 65 pings with antenna_original == "0" due to unreadable tag #s  
  # 04_18_14.TXT (1 ping), 04_21_14.TXT (6), 05_12_14.TXT (11), 05_13_14.TXT (1)
  # 05_15_14.TXT (15), 05_19_14.TXT (2), 05_21_14.TXT (2), 05_26_14.TXT (4),
  # 05_8_14.TXT (21)
  filter(antenna_original != "0") %>%
  # removing 5 pings with tag typos (but good data on either side)
  # 05_15_14.TXT (1 ping), 05_8_14.TXT (4)
  filter(antenna_original != "00000") %>%
  # removing 3 pings with tag typos (but good data on either side)
  # 05_8_14.TXT 2014-05-02 10:27:29.24
  # 05_8_14.TXT 2014-05-02 10:28:28.89
  # 05_8_14.TXT 2014-05-02 10:28:38.85
  # 05_9_14.TXT 2014-05-05 18:02:24.22
  filter(antenna_original != "0000000067143866",
         antenna_original != "00000000000067143866",
         antenna_original != "L1") %>%
  # removing 2 data files with no usable data but that caused reading errors
  # 4/16 has no data; 6/5/14 has no D/E/B codes
  filter(file != "04_16_14.TXT",
         file != "06_05_14.TXT") %>%
  # fix 2 pings with tag number missing an underscore
  # and manually assigned antenna A1 to file "05_15_14 DownStream.TXT" (see issue #13)
  mutate(antenna = case_when(antenna_original == "0000000178695688" ~ "A4",
                             file == "05_15_14 DownStream.TXT" ~ "A1",
                             TRUE ~ antenna),
         tag = case_when(antenna_original == "0000000178695688" 
                         ~ "0000_0000000178695688",
                         TRUE ~ tag),
         tag_short = case_when(antenna_original == "0000000178695688" 
                               ~ "178695688",
                               TRUE ~ tag_short),
         antenna_original = case_when(antenna_original == "0000000178695688" 
                                      ~ "A1",
                                      TRUE ~ antenna_original)) %>%
  # filtering out 20 corrupt tag numbers - ultimately should be reviewed
  # this is issue # 14 on GitHub. They all contain \'s and an L
  filter(grepl(iconv(tag_short, from = "", to = "UTF-8"), pattern = "L") == F) %>%
  # remove a single record from 2017 (tag 0000_0000000180843403)
  filter(year(date) != 2017) %>%
  # fix antenna names based on IFW Antenna Key.xlsx file info
  # all* records in files named with _A2 should be A1  
  mutate(antenna = case_when(grepl(file, pattern = "_A2") ~ "A1",
                             TRUE ~ antenna)) %>%
  # *except pings from 05_29_14_A2.TXT which should be A4
  mutate(antenna = case_when(file == "05_29_14_A2.TXT" ~ "A4",
                             TRUE ~ antenna)) %>%
  # all records in files named with _A1 should be A4  
  mutate(antenna = case_when(grepl(file, pattern = "_A1") ~ "A4",
                             TRUE ~ antenna))


# check for antenna naming issues, etc.
unique(data2014$antenna)
unique(data2014$antenna_original)
unique(data2014$tag)
unique(data2014$tag_short)
unique(data2014$date)
hist(yday(data2014$date))
table(data2014$direction)
table(year(data2014$date))

# subset final columns
data2014 <- data2014 %>%
  select(file, date, time, datetime, direction, antenna, tag, tag_short) %>%
  arrange(datetime) %>%
  mutate(year = "2014")

rm(file_data_list, data, data_all, datadir, filename, filepath, files, i)
