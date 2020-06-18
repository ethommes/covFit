
analyze_R_and_mobility <- function(incidence, google_mobi, initial_R, inputs) {
                                   # google_mobi,
                                   # mobility_country, 
                                   # mobility_region, 
                                   # mobility_subregion,
                                   # mobility_window_size,
                                   # R_window_size,
                                   # sigma_SEIR,
                                   # gamma_SEIR) {
  with(inputs, {
    browser()
    # cases_roll <- frollmean(incidence$cases,R_window_size)
    # log_cases_roll <- log(cases_roll)

    incidence_aggr <- aggregate_incidence(incidence,R_window_size)
    mean_cases <- incidence_aggr$cases/R_window_size
    log_mean_cases <- log(mean_cases)
    mean_rho <- c(NA,diff(log_mean_cases))/R_window_size
    mean_R <- R0_SEIR(mean_rho,sigma_SEIR,gamma_SEIR)
    
    
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
    df_plot_delta_R <- data.frame("date"=incidence_aggr$dates, "delta_R"=delta_R)[inds,]
    df_plot_residential <- data.frame("date" = google_mobi$df$date, "residential_roll_transformed" = residential_roll_transformed)
    plot1 <- ggplot(data=df_plot_delta_R,aes(x=date, y=delta_R)) + geom_line(color="red") +
      geom_line(data=df_plot_residential, aes(x=date, y=-residential_roll_transformed),color="black") + 
      ylim(-100, 25) +
      labs(x="date", y="percent change from baseline")
    # print(plot1)
    filename = paste0(Country.Region, "_", Province.State, "_", Admin2,"_R_and_mobility.png")
    # filename = paste0(Admin2, "_", Province.State, "_", Country.Region,"_R_and_mobility.png")
    ggsave(path=pathname_figs, filename=filename, plot1)    
    
    # plot(incidence_aggr$dates, delta_R, type="l", col="red",ylim=c(-100,50))
    # lines(google_mobi$df$date, -residential_roll_transformed, col="black")
    browser()
  })
}