
plot_rolling_values <- function(rolling_list, title, CFR_text, inputs) {
  if (!is.na(rolling_list$forecast)) {
    pop <- inputs$pop
    
    # Scale factor for incidence, depending on whether we want to plot cases per day, 
    # cases per day per 1k, etc.:
    if (inputs$incidencePer == "raw") {
      incScale <- 1
      y_label <- "daily cases"
    } else if (inputs$incidencePer == "per1k") {
      incScale <- 1e3/pop
      y_label <- "daily cases \nper 1000"
    } else if (inputs$incidencePer == "per10k") {
      incScale <- 1e4/pop
      y_label <- "daily cases \nper 10,000"
    } else {
      incScale <- 1e5/pop
      y_label <- "daily cases \nper 100,000"
    }
    

    # Find y limit.  Make upper limit = 1e5 (i.e. entire population infected on 1 day!)
    # z <- c(rolling_list$incidence$cases/pop*1e5, exp(rolling_list$forecast$log_cases_max)/pop*1e5) 
    z <- c(rolling_list$incidence$cases*incScale, exp(rolling_list$forecast$log_cases_max)*incScale) 
    z2 <- c(rolling_list$incidence$cases*incScale, 2*exp(rolling_list$forecast$log_cases_mid)*incScale) 
    
    
    z <- z[(is.finite(z) & z > 0)]
    z2 <- z2[(is.finite(z2) & z2 > 0)]
    if (is.na(inputs$manual_max_incidence)) {
      y_max <- max(z)
      y_max_linear <- max(z2)
      # y_max <- min(max(z), 1.05e4)
      # y_max_linear <- min(max(z2), 1.05e4)
    } else {
      y_max <- inputs$manual_max_incidence
      y_max_linear <- inputs$manual_max_incidence
    }
    
    
    
    y_min <- min(z)
    x_min <- min(rolling_list$incidence$dates)
    x_max <- max(rolling_list$forecast$dates)
    
    latest_date <- rolling_list$rolling_values_for_forecast$dates[nrow(rolling_list$rolling_values_for_forecast)]
    cases_total <- rolling_list$rolling_values_for_forecast$cumu_cases[nrow(rolling_list$rolling_values_for_forecast)]
    
    text_plot_1 <- paste0(CFR_text,
                          "Total cases up to ",latest_date,":\n",
                          cases_total," (",round(cases_total*1e5/pop,1)," per 100,000)\n \n")
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
    
    plot_base <- ggplot(data=rolling_list$incidence, aes(x=dates, y=cases*incScale)) + geom_point(size=1) +
      geom_point(data=rolling_list$rolling_values_for_forecast, aes(x=dates, y=cases*incScale), color="lightgreen", size=1) +
      geom_line(data=rolling_list$rolling_values, aes(x=dates, y = exp(log_cases_roll)*incScale)) +
      # xlim(min(rolling_list$incidence$dates), max(rolling_list$forecast$dates)) +
      # xlim(x_min, x_max) +
      geom_line(data=rolling_list$rolling_values_for_forecast, aes(x=dates, y=exp(log_cases_roll)*incScale),color="green") +
      geom_line(data=rolling_list$forecast, aes(x=dates, y=exp(log_cases_mid)*incScale), color="green") +
      geom_ribbon(data=rolling_list$forecast, aes(x=dates, y=exp(log_cases_mid)*incScale, 
                                                  ymin=exp(log_cases_min)*incScale, 
                                                  ymax=exp(log_cases_max)*incScale), fill="green", alpha=0.2, color="green", linetype=3) +
      labs(x="date", y=y_label) +
      theme(plot.title = element_text(margin = margin(t = 10, b = -20), size=11, hjust=0.5)) +
      scale_x_date(date_breaks = "months", date_labels = "%b") +
      geom_vline(xintercept = rolling_list$turnover_date, linetype=3)
    
    
    plot1 <- plot_base + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
      theme(plot.margin = margin(5.5,5.5,0,5.5,"pt")) +
      # ylim(0,y_max) +
      coord_cartesian(xlim=c(x_min, x_max), ylim=c(0,y_max_linear)) +
      # ggtitle("linear plot") +
      annotate_textp(x=0.02,y=1.0, label=text_plot_1, size=7.5) +
      annotate_textp(x=0.95, y=1.0, label="LINEAR PLOT", size=8.5)
    
    plot2 <- plot_base + 
      # scale_y_log10() +
      coord_cartesian(xlim=c(x_min, x_max), ylim=c(y_min,y_max)) +
      scale_y_log10() +
      # theme(plot.margin = margin(0,5.5,5.5,5.5,"pt")) +
      theme(plot.margin = margin(0,5.5,0,5.5,"pt")) + # NEW
      
      theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + # New
      annotate_textp(x=0.95, y=1.0, label="LOG PLOT", size=8.5)
    
    
    plot_R <- ggplot(data=rolling_list$rolling_values, aes(x=dates, y=R_roll)) + geom_line() +
      geom_hline(yintercept = 1, linetype=3) + 
      labs(x="date", y="effective R") + 
      # xlim(x_min, x_max) +
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
  } else {
    plot3 <- NA
  }
  
  return(plot3)
}