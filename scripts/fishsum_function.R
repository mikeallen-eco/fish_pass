# function to summarize data by antenna and fish
fishsum <- function(ping_data,
                    return_pings_predate_tagging = 0){

temp1a <- 
ping_data %>%
  filter(tag_short %in% tags$tag_short) %>%
  select(tag_short, datetime, antenna) %>%
  arrange(tag_short, datetime) %>%
  # add in tagging time and common name to filter out 
  # pings that predate tagging
  left_join(select(tags, tag_short, tag_time, Common.Name), 
            by = c("tag_short")) %>%
  mutate(pings_predate_tagging = ifelse(datetime < tag_time, 1, 0))

# save list of tags for which pings predate tagging
pings_predate_tagging <- temp1a %>%
  filter(pings_predate_tagging == 1) %>%
  select(-pings_predate_tagging) %>%
  rename(ping_time = datetime)

# finish formatting initial ping data
temp1b <- temp1a %>%
  filter(datetime >= tag_time) %>%
  # remove tagging info again to summarize pings
  select(tag_short, datetime, antenna) %>%
  group_by(tag_short, antenna) %>%
  summarize(
    first_ping = min(datetime),
    last_ping = max(datetime),
    n = length(datetime),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = antenna,
                     values_from = c(first_ping, last_ping, n)) %>%
  # add in tagging time and common name
  left_join(tags, by = "tag_short") %>%
  select(tag_short, Common.Name, tag_time, 
         starts_with("first_ping"),
         starts_with("last_ping"), 
         starts_with("n_")) 

# add antenna columns if they are missing
if(sum(grepl(colnames(temp1b), pattern = "_A1"))==0){
  temp1b <- temp1b %>%
    mutate(first_ping_A1 = NA,
           last_ping_A1 = NA,
           n_A1 = NA)
}

if(sum(grepl(colnames(temp1b), pattern = "_A4"))==0){
  temp1b <- temp1b %>%
    mutate(first_ping_A4 = NA,
           last_ping_A4 = NA,
           n_A4 = NA)
}

if(sum(grepl(colnames(temp1b), pattern = "_A2"))==0){
  temp1b <- temp1b %>%
    mutate(first_ping_A2 = NA,
           last_ping_A2 = NA,
           n_A2 = NA)
}

if(sum(grepl(colnames(temp1b), pattern = "_A3"))==0){
  temp1b <- temp1b %>%
    mutate(first_ping_A3 = NA,
           last_ping_A3 = NA,
           n_A3 = NA)
}

# calculate durations
  # attraction time (tag_time -> min A1)
  # staging time (min A1 -> max A1)
  # transit time (max A1 -> min A4)
  # passage time (sum of other 3)
temp2 <- temp1b %>%
  mutate(
    attraction.days = as.numeric(as.duration(
      interval(start = tag_time,
               end = first_ping_A1)
    )) / (3600 * 24),
    staging.days =
      as.numeric(as.duration(
        interval(start = first_ping_A1,
                 end = last_ping_A1)
      )) / (3600 * 24),
    transit.days = as.numeric(as.duration(
      interval(start = last_ping_A1,
               end = first_ping_A4)
    )) / (3600 * 24),
    passage.days = as.numeric(as.duration(
      interval(start = tag_time,
               end = first_ping_A4)
    )) / (3600 * 24)
  )

# find detection year
if(sum(is.na(temp2$first_ping_A1)==F) > 0){
  detect_year <- year(min(temp2$first_ping_A1, na.rm = T))
}

if(sum(is.na(temp2$first_ping_A2)==F) > 0){
  detect_year <- year(min(temp2$first_ping_A2, na.rm = T))
}

if(sum(is.na(temp2$first_ping_A3)==F) > 0){
  detect_year <- year(min(temp2$first_ping_A3, na.rm = T))
}

if(sum(is.na(temp2$first_ping_A4)==F) > 0){
  detect_year <- year(min(temp2$first_ping_A4, na.rm = T))
}

# format final output
temp3 <- temp2 %>%
  mutate(detect_year = detect_year) %>%
  select(detect_year, tag_short, species = Common.Name, tag_time, 
         first_ping_A1, last_ping_A1,
         first_ping_A2, last_ping_A2, first_ping_A3,
         last_ping_A3, first_ping_A4, last_ping_A4,
         n_A1, n_A2, n_A3, n_A4, 
         attraction.days, staging.days,
         transit.days, passage.days
         ) %>%
  arrange(species, tag_time)

# if return_pings_predate_tagging == T
if(return_pings_predate_tagging == 1){

if(nrow(pings_predate_tagging)==0){
  pings_predate_tagging <- data.frame(
    tag_short = NA,
    ping_time = NA,
    antenna = NA,
    tag_time = NA,
    Common.Name = NA
  )
}
  
return(pings_predate_tagging)
}

# if return_pings_predate_tagging == F
if(return_pings_predate_tagging == 0){
return(temp3)
}

} # end function 
