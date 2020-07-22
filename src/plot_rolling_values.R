
plot_rolling_values <- function(rolling_list, inputs) {
  
  # pop <- rolling_list$rolling_values$pop[1]
  pop <- inputs$pop
  
  # Find y limit.  Make upper limit = 1e5 (i.e. entire population infected on 1 day!)
  # z <- c(rolling_list$incidence$cases/pop*1e5, exp(rolling_list$forecast$log_cases_max)/pop*1e5) 
  z <- c(rolling_list$incidence$cases/pop*1e5, exp(rolling_list$forecast$log_cases_max)/pop*1e5) 
  
  z <- z[(is.finite(z) & z > 0)]
  y_max <- min(max(z), 1.05e4)
  y_min <- min(z)

  plot_base <- ggplot(data=rolling_list$incidence, aes(x=dates, y=cases/pop*1e5)) + geom_point(size=1) +
    geom_point(data=rolling_list$rolling_values_for_forecast, aes(x=dates, y=cases/pop*1e5), color="green") +
  geom_line(data=rolling_list$rolling_values, aes(x=dates, y = exp(log_cases_roll)/pop*1e5)) +
    xlim(min(rolling_list$incidence$dates), max(rolling_list$forecast$dates)) +
    geom_line(data=rolling_list$rolling_values_for_forecast, aes(x=dates, y=exp(log_cases_roll)/pop*1e5),color="green") +
    # geom_line(data=rolling_list$forecast, aes(dates,exp(log_cases_mid)/pop*1e5), color="green") +
    # geom_line(data=rolling_list$forecast, aes(dates,exp(log_cases_min)/pop*1e5), color="green", linetype=3) +
    # geom_line(data=rolling_list$forecast, aes(dates,exp(log_cases_max)/pop*1e5), color="green", linetype=3)
    geom_line(data=rolling_list$forecast, aes(x=dates, y=exp(log_cases_mid)/pop*1e5), color="green") +
    geom_ribbon(data=rolling_list$forecast, aes(x=dates, y=exp(log_cases_mid)/pop*1e5, 
                                                  ymin=exp(log_cases_min)/pop*1e5, 
                                                  ymax=exp(log_cases_max)/pop*1e5), fill="green", alpha=0.2, color="black") +
    labs(x="date", y="cases per day per 100k") +
    theme(plot.title = element_text(margin = margin(t = 10, b = -20), size=11, hjust=0.5)) +
    scale_x_date(date_breaks = "months", date_labels = "%b")
    
    
    
  plot1 <- plot_base + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
    theme(plot.margin = margin(5.5,5.5,0,5.5,"pt")) +
    # ylim(0,y_max) +
    coord_cartesian(ylim=c(0,y_max)) +
    # labs(title="linear plot")
    ggtitle("linear plot") 
    # labs(title="linear plot", x="date", y="cases per day per 100k") +
    
  plot2 <- plot_base + 
    # scale_y_log10(limits=c(y_min,y_max)) + 
    scale_y_log10() +
    coord_cartesian(ylim=c(y_min,y_max)) +
    theme(plot.margin = margin(0,5.5,5.5,5.5,"pt")) +
    ggtitle("log plot")
  
  # print(plot1)
  plot3 <- ggarrange(plot1, plot2, nrow=2, ncol=1, align="v")
  print(plot3)
  filename <- paste0(inputs$Country.Region,"_",inputs$Province.State,"_",inputs$Admin2,"_exponential_fit_alternate.",inputs$filetype)
  # ggsave(filename=filename, path=inputs$pathname_figs, plot3)
  ggsave(filename=filename, path=inputs$pathname_figs, plot3)
}