# make a function to plot pings by tag number for a given year
plot_by_tag <- function(ping_data){
  the_plot <- ggplot(ping_data) +
    geom_point(aes(x = datetime, y = as.factor(tag_short), color = antenna),
               alpha = .8, pch = 1) +
    scale_color_manual(values = c(wes_palettes$FantasticFox1[1:3],
                                  "firebrick")) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(text = element_text(size = 15))
  
  ggsave(paste0("figures/", 
                stringr::str_sub(year(ping_data$date[1])), 
                "_tag_check.png"), height = 7, width = 10, dpi = 400)
  
  return(the_plot)
}