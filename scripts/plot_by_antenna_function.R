# make a function to plot pings by antenna for a given year
plot_by_antenna <- function(ping_data){
  antenna_plot <- ggplot(ping_data) +
    geom_point(aes(x = datetime, y = as.factor(antenna)),
               alpha = .5, pch = 1) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(text = element_text(size = 15))
  
  ggsave(paste0("figures/", 
                stringr::str_sub(year(ping_data$date[1])), 
                "_antenna_check.png"), height = 7, width = 10, dpi = 400)
  
  return(antenna_plot)
}