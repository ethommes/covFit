
plot_rolling_values <- function(rolling_list, title, CFR_text, inputs) {
  pop <- inputs$pop
  
  # Find y limit.  Make upper limit = 1e5 (i.e. entire population infected on 1 day!)
  # z <- c(rolling_list$incidence$cases/pop*1e5, exp(rolling_list$forecast$log_cases_max)/pop*1e5) 
  z <- c(rolling_list$incidence$cases/pop*1e5, exp(rolling_list$forecast$log_cases_max)/pop*1e5) 
  
  z <- z[(is.finite(z) & z > 0)]
  y_max <- min(max(z), 1.05e4)
  y_min <- min(z)
  x_min <- min(rolling_list$incidence$dates)
  x_max <- max(rolling_list$forecast$dates)

  latest_date <- rolling_list$rolling_values_for_forecast$dates[nrow(rolling_list$rolling_values_for_forecast)]
  cases_total <- rolling_list$rolling_values_for_forecast$cumu_cases[nrow(rolling_list$rolling_values_for_forecast)]
  
  text_plot_1 <- paste0(CFR_text,
                        "Total cases up to ",latest_date,":\n",
                        cases_total," (",round(cases_total*1e5/inputs$pop,0)," per 100,000)\n \n")
  # text_plot_2 <- paste0("Pre-turnover phase:\n",
  #                       "   rho = ", round(rolling_list$rho_0,2),"\n",
  #                       "   doubling time = ",round(rolling_list$doubling_time_0,2)," days (negative value = halving time)\n",
  #                       "   R = ",round(rolling_list$R0,2), "\n",
  #                       "   turnover date = ", rolling_list$turnover_date,"\n")
  
  text_plot_2 <- paste0("Pre-turnover phase:\n",
                        "   R0 = ",round(rolling_list$R0,2), "\n",
                        "Turnover date = ", rolling_list$turnover_date,"\n",
                        "Post-turnover:\n",
                        "   R_eff = ", round(rolling_list$R_mid,2), "(", round(rolling_list$R_min,2), ", ", round(rolling_list$R_max,2),")\n"
  )
  
  plot_base <- ggplot(data=rolling_list$incidence, aes(x=dates, y=cases/pop*1e5)) + geom_point(size=1) +
    geom_point(data=rolling_list$rolling_values_for_forecast, aes(x=dates, y=cases/pop*1e5), color="lightgreen", size=1) +
  geom_line(data=rolling_list$rolling_values, aes(x=dates, y = exp(log_cases_roll)/pop*1e5)) +
    # xlim(min(rolling_list$incidence$dates), max(rolling_list$forecast$dates)) +
    # xlim(x_min, x_max) +
    geom_line(data=rolling_list$rolling_values_for_forecast, aes(x=dates, y=exp(log_cases_roll)/pop*1e5),color="green") +
    geom_line(data=rolling_list$forecast, aes(x=dates, y=exp(log_cases_mid)/pop*1e5), color="green") +
    geom_ribbon(data=rolling_list$forecast, aes(x=dates, y=exp(log_cases_mid)/pop*1e5, 
                                                  ymin=exp(log_cases_min)/pop*1e5, 
                                                  ymax=exp(log_cases_max)/pop*1e5), fill="green", alpha=0.2, color="green", linetype=3) +
    labs(x="date", y="daily cases \nper 100k") +
    theme(plot.title = element_text(margin = margin(t = 10, b = -20), size=11, hjust=0.5)) +
    scale_x_date(date_breaks = "months", date_labels = "%b") +
    geom_vline(xintercept = rolling_list$turnover_date, linetype=3)
    
    
    
  plot1 <- plot_base + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
    theme(plot.margin = margin(5.5,5.5,0,5.5,"pt")) +
    # ylim(0,y_max) +
    coord_cartesian(xlim=c(x_min, x_max), ylim=c(0,y_max)) +
    # ggtitle("linear plot") +
    annotate_textp(x=0.02,y=1.0, label=text_plot_1, size=7.5) +
    annotate_textp(x=0.95, y=1.0, label="LINEAR PLOT", size=8.5)

  plot2 <- plot_base + 
    # scale_y_log10(limits=c(y_min,y_max)) + 
    scale_y_log10() +
    coord_cartesian(ylim=c(y_min,y_max)) +
    # theme(plot.margin = margin(0,5.5,5.5,5.5,"pt")) +
    theme(plot.margin = margin(0,5.5,0,5.5,"pt")) + # NEW
    
    theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + # New
    annotate_textp(x=0.95, y=1.0, label="LOG PLOT", size=8.5)
  
  
  plot_R <- ggplot(data=rolling_list$rolling_values, aes(x=dates, y=R_roll)) + geom_line() +
    geom_hline(yintercept = 1, linetype=3) + 
    labs(x="date", y="effective R") + 
    xlim(x_min, x_max) +
    scale_x_date(date_breaks = "months", date_labels = "%b") +
    geom_vline(xintercept = rolling_list$turnover_date, linetype=3) +
    coord_cartesian(xlim=c(x_min, x_max)) +
    annotate_textp(x=1.0, y=1.0, label=text_plot_2, size=7.5)
  
  
  # plot3 <- ggarrange(plot1, plot2, nrow=2, ncol=1, align="v")
  plot3 <- ggarrange(plot1, plot2, plot_R, nrow=3, ncol=1, align="v")
  plot3 <- annotate_figure(plot3, top=title)
  
  if (inputs$plot_to_screen_TF) {print(plot3)}
  
  if (inputs$plot_TF) {
    filename <- paste0(inputs$Country.Region,"_",inputs$Province.State,"_",inputs$Admin2, inputs$predict_from_date, "_exp_fit.",inputs$filetype)
    # ggsave(filename=filename, path=inputs$pathname_figs, plot3)
    ggsave(filename=filename, path=inputs$pathname_figs, plot3)
  }
  return(plot3)
}