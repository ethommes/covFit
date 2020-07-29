
analyze_R_and_mobility_v2 <- function(incidence, google_mobi, initial_R, turnover_date, inputs) {
                                   # google_mobi,
                                   # mobility_country, 
                                   # mobility_region, 
                                   # mobility_subregion,
                                   # mobility_window_size,
                                   # R_window_size,
                                   # sigma_SEIR,
                                   # gamma_SEIR) {
  with(inputs, {
    # cases_roll <- frollmean(incidence$cases,R_window_size)
    # log_cases_roll <- log(cases_roll)

    incidence_aggr <- aggregate_incidence(incidence,R_window_size)
    mean_cases <- incidence_aggr$cases/R_window_size
    log_mean_cases <- log(mean_cases)
    mean_rho <- c(NA,diff(log_mean_cases))/R_window_size
    mean_R <- R0_SEIR(mean_rho,sigma_SEIR,gamma_SEIR)
    
    # TEST-----
    df <- smoothed_incidence_and_rho(incidence, R_window_size)
    plot(df$dates, df$log_cases, type="l")
    lines(df$dates, df$log_cases_roll, col="blue", lwd=2)
    browser()
    
    plot(df$dates, df$rho_roll, type="l", col="green", lwd=2)
    browser()
    
    # END TEST-----
    # We'll calculate relative change in R wrt. the first finite R value:
    # R0 <- mean_R[min(which(is.finite(mean_R)))] 
    # delta_R <- 100*(mean_R - R0)/R0
    delta_R <- 100*(mean_R - initial_R)/initial_R
    
    # Transform residential percent change: We will assume that on average people spend only half a day (12 hours)
    # outside the home even under normal circumstances. Thus, we want to apply the change in mobility inferred from the
    # change in residential to only 12 hours out of a day:
    residential_roll_transformed <- google_mobi$df$residential_roll*(24/12)
    
    # Plot:
    inds <- is.finite(delta_R)

    df_plot_delta_R <- data.frame("date"=incidence_aggr$dates, "delta_R"=delta_R, "R" = mean_R)[inds,]
    df_plot_residential <- data.frame("date" = google_mobi$df$date, "residential_roll_transformed" = residential_roll_transformed)
    x_min <- min(df_plot_delta_R$date[1], df_plot_residential$date[1])
    x_max <- max(df_plot_delta_R$date[nrow(df_plot_delta_R)], df_plot_residential$date[nrow(df_plot_residential)])
    
    plot1 <- ggplot(data=df_plot_delta_R,aes(x=date, y=delta_R, color="red")) + geom_line() +
      geom_line(data=df_plot_residential, aes(x=date, y=-residential_roll_transformed, color="black")) + 
      geom_vline(xintercept = google_mobi$date_residential_mid, linetype=2) +
      geom_vline(xintercept = turnover_date, color="red", linetype=2) +
      ylim(-100, 25) +
      labs(x="date", y="percent change from baseline") +
      theme(axis.title.x = element_blank()) +
      scale_color_manual(name = "legend", labels=c("mobility", "R"), values = c("red"="red","black"="black"))
      xlim(x_min, x_max)
    # plot1 <- ggplot(data=df_plot_delta_R,aes(x=date, y=delta_R, color="red")) + geom_line() +
    #   geom_line(data=df_plot_residential, aes(x=date, y=-residential_roll_transformed, color="black")) + 
    #   geom_vline(xintercept = google_mobi$date_residential_mid, linetype=2) +
    #   geom_vline(xintercept = turnover_date, color="red", linetype=2) +
    #   ylim(-100, 25) +
    #   labs(x="date", y="percent change from baseline") +
    #   theme(axis.title.x = element_blank()) +
    #   xlim(x_min, x_max)
    # print(plot1)
    # filename = paste0(Admin2, "_", Province.State, "_", Country.Region,"_R_and_mobility.png")
    # ggsave(path=pathname_figs, filename=filename, plot1)    
    
    plot2 <- ggplot(data=df_plot_delta_R,aes(x=date, y=R)) + geom_line(aes(color="red")) +
      labs(x="date", y=paste0("R, last ", R_window_size, " days")) +
      geom_hline(yintercept = 1, linetype=3) +
      geom_vline(xintercept = google_mobi$date_residential_mid, linetype=2) +
      geom_vline(xintercept = turnover_date, color="red", linetype=2) +
      xlim(x_min,x_max) +
      scale_color_manual(name = "legend", labels=c("R", "mobility"), values = c("red"="red","black"="black"))

    # plot2 <- ggplot(data=df_plot_delta_R,aes(x=date, y=R, color="red")) + geom_line() +
    #   labs(x="date", y=paste0("R, last ", R_window_size, " days")) +
    #   geom_hline(yintercept = 1, linetype=3) +
    #   geom_vline(xintercept = google_mobi$date_residential_mid, linetype=2) +
    #   geom_vline(xintercept = turnover_date, color="red", linetype=2) +
    #   xlim(x_min,x_max) 
    # +
    #   scale_color_manual(name = "legend", labels=c("mobility", "R"), values = c("black"="black","red"="red"))
    
    gA <- ggplotGrob(plot1)
    gB <- ggplotGrob(plot2)
    grid.newpage()
    twopanel_plot <- arrangeGrob(rbind(gA, gB), top=paste0("R and mobility change, ", Admin2," ",Province.State," ", Country.Region))
    filename = paste0(Country.Region, "_", Province.State, "_", Admin2,"_R_and_mobility.png")
    ggsave(path=pathname_figs, filename=filename, twopanel_plot) 

    # plot(incidence_aggr$dates, delta_R, type="l", col="red",ylim=c(-100,50))
    # lines(google_mobi$df$date, -residential_roll_transformed, col="black")
  })
}