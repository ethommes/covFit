
plot_current_vs_projected_incidence_non_US_countries <- function(data_table, projection_date, pathname) {
  data_table_proj <- forecast_from_US_states_or_counties_frame(data_table, as.Date("2020-06-30"), projection_date)
  current_date <- data_table_proj$last_date[1]
  window_for_current <- data_table_proj$window_for_current[1]
  
  plt1 <- ggplot(data_table_proj,aes(x=incidence_current/pop*1e5, y = forecast_incidence_2_from_recent/pop*1e5,color="red")) + 
    geom_point() +
    geom_text_repel(aes(label=countries),hjust=0.2,vjust=-0.5,size=3,color="darkgray") +
    geom_point(aes(x=incidence_current/pop*1e5, y=forecast_incidence_2_from_post_turnover/pop*1e5,color="black")) +
    labs(title=paste0("State daily incidence, current (", current_date, ") \nand ", projection_date, " projection\n
                      (above line = increase, below line = decrease)"), x="current cases per day per 100k",
         y="projected cases per day per 100k") + 
    # xlim(0,75) + ylim(0,160) +
    geom_abline(slope=1,intercept=0) +
    scale_color_manual(name = "legend", labels=c("projected \nusing all \npost-turnover\n ", 
                                                 paste0(" \nprojected \nusing past\n",window_for_current, " days\n")), values = c("red"="red","black"="black"))
  
  plt2 <- plt1 + scale_x_log10() + scale_y_log10()
  
  plt3 <- plt2 + geom_errorbar(aes(ymin = forecast_incidence_2_from_post_turnover/pop*1e5,
                                   ymax=forecast_incidence_2_from_recent/pop*1e5),color="black")
  print(plt3)
  
  ggsave(path = pathname, filename = paste0("current_vs_projected_incidence_US_states_",projection_date,".png"))
}
