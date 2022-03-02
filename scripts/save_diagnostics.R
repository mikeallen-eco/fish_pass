### Saves clean data files and diagnostic figures for inspection

# Load library
library(here)

# Export cleaned, compiled data for inspection
write.csv(data2012, 
          here("output", "2012_clean_detection_data.csv"), row.names=F)
write.csv(data2013, 
          here("output", "2013_clean_detection_data.csv"), row.names=F)
write.csv(data2014, 
          here("output", "2014_clean_detection_data.csv"), row.names=F)
write.csv(data2015, 
          here("output", "2015_clean_detection_data.csv"), row.names=F)
write.csv(data2016, 
          here("output", "2016_clean_detection_data.csv"), row.names=F)
write.csv(data2017, 
          here("output", "2017_clean_detection_data.csv"), row.names=F)
write.csv(data2018, 
          here("output", "2018_clean_detection_data.csv"), row.names=F)
write.csv(data2019, 
          here("output", "2019_clean_detection_data.csv"), row.names=F)

# write fish summary csv files for review
write.csv(fish_sum, "output/fish_sum.csv", row.names = F)

# Function to plot pings by tag to identify which belong to fish
plot_by_tag <- function(ping_data){
  the_plot <- ggplot(ping_data) +
    geom_point(aes(x = datetime, y = as.factor(tag_short), color = antenna),
               alpha = .8, pch = 1) +
    scale_color_manual(values = c(wes_palettes$FantasticFox1[1:3],
                                  "firebrick")) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(text = element_text(size = 15))
  
  ggsave(here("figures", paste0(stringr::str_sub(year(ping_data$date[1])), 
                     "_tag_check.png")), height = 7, width = 10, dpi = 400)
  
  return(the_plot)
}

# Plot pings by tag to identify which belong to fish
plot_by_tag(data2012)
plot_by_tag(data2013)
plot_by_tag(data2014)
plot_by_tag(data2015)
plot_by_tag(data2016)
plot_by_tag(data2017)
plot_by_tag(data2018)
plot_by_tag(data2019)

# Function to plot pings by antenna to check antenna naming, etc.
# make a function to plot pings by antenna for a given year
plot_by_antenna <- function(ping_data){
  antenna_plot <- ggplot(ping_data) +
    geom_point(aes(x = datetime, y = as.factor(antenna)),
               alpha = .5, pch = 1) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(text = element_text(size = 15))
  
  ggsave(here("figures", paste0(stringr::str_sub(year(ping_data$date[1])), 
                                "_antenna_check.png")), height = 7, width = 10, dpi = 400)
  
  return(antenna_plot)
}

# Plot pings by antenna to check antenna naming, etc.
plot_by_antenna(data2012)
plot_by_antenna(data2013)
plot_by_antenna(data2014)
plot_by_antenna(data2015)
plot_by_antenna(data2016)
plot_by_antenna(data2017)
plot_by_antenna(data2018)
plot_by_antenna(data2019)