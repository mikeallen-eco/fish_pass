# function to summarize data by antenna and fish
fishsum <- function(ping_data){

ping_data %>%
  filter(tag_short %in% tags$tag_short) %>%
  arrange(tag_short, datetime) %>%
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
  left_join(tags) %>%
  select(tag_short, Common.Name, tag_time, 2:13) %>%
  # calculate durations
  # attraction time (tag_time -> min A1)
  # staging time (min A1 -> max A1)
  # transit time (max A1 -> max A4)
  # passage time (sum of other 3)
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
               end = last_ping_A4)
    )) / (3600 * 24),
    passage.days = as.numeric(as.duration(
      interval(start = tag_time,
               end = last_ping_A4)
    )) / (3600 * 24)
  )
}  
