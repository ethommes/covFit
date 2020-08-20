
# _v4 adds the Google data
# _v5 drops the Apple data
# _v6 June 3 2020
#     -output also the output from find_exponential_portion_v3e()
mobility_and_incidence_v6 <- function(incidence, 
                                   google_mobility,
                                   pop,
                                   cumu_case_fraction_threshold, # cumulative number of cases as fraction of the population at which we deem outbreak to have started AT LATEST
                                   sigma_SEIR,
                                   gamma_SEIR,
                                   window_for_current_R,
                                   plot_current_R_TF, # Do we plot (in green) the fit for the current R?
                                   US_TF, # logical: Are we looking at the US?
                                   mobility_region,
                                   mobility_subregion,
                                   mobility_country,
                                   onset_date_override,
                                   turnover_date_override,
                                   mobility_window_half_width,
                                   incidence_window_half_width,
                                   diagnostic_mode_TF,
                                   title,
                                   xlim,
                                   pathname) {
  
  # Notes: 
  # - incidence_window_half_width is a misnomer since for day j we are actually 
  #   performing our regression fit over a window 
  #   (j - incidence_window_half+width, j+incidence_window_half_width)
  # - when diagnostic_mode_TF == T, then for each regression window,
  #   the function plots the regression for log10(cases) vs time and then pauses 
  
  
  # Google mobility data:
  
  google_mobi <- read_google_mobility(google_mobility,
                                      mobility_country, 
                                      mobility_region, 
                                      mobility_subregion,
                                      mobility_window_half_width)


  # Find and analyze the initial period of exponential growth:
  initial_exp_growth_result <- find_exponential_portion_v3f(incidence,
                                                            pop,
                                                            N_days_to_aggregate = 1,
                                                            daily_case_threshold = 1,
                                                            cumu_case_threshold = cumu_case_fraction_threshold*pop, # CAUTION, hardwired
                                                            max_starting_index = 1,
                                                            onset_date_override,
                                                            turnover_date_override,
                                                            sigma_SEIR,
                                                            gamma_SEIR,
                                                            window_for_current_R = window_for_current_R,
                                                            plot_current_R_TF = plot_current_R_TF,
                                                            plot_TF = T, 
                                                            county = "",
                                                            state = title,
                                                            pathname = pathname
                                                            # pathname = "D:/Users/ethom/Dropbox/new_research_current/COVID-19/pandemicSQEIRL/figures/mobility_test"
                                                            )

  # Compute rho (= slope) across moving window:
  incidence <- data.frame(incidence, 
                          "log_cases" = log(incidence$cases), 
                          "slope" = NA,
                          "intercept" = NA,
                          "doubling_time" = NA,
                          "R" = NA)
  n_days_incidence <- nrow(incidence)
  i_start <- incidence_window_half_width + 1
  i_end <- n_days_incidence - incidence_window_half_width - 1
  incidence_window <- incidence_window_half_width*2 + 1

  # TEST: pre-process the incidence data by applying rolling average [use same window as mobility]
  cases_roll <- frollmean(incidence$cases, incidence_window, align="center")
  log_cases_roll <- log(cases_roll)
  incidence <- data.frame(incidence,"cases_roll"=cases_roll,"log_cases_roll"=log_cases_roll)
  plot1 <- ggplot(data=incidence,aes(x=dates, y=cases_roll)) + geom_line(color="blue") +
    geom_point(aes(x=dates,y=cases)) +
    labs(x="date", y="daily cases",title=paste0("Daily cases, raw and rolling mean (",incidence_window,") days"))
  print(plot1)
  # plot(incidence$dates, cases_roll, type="l",main=)
  # points(incidence$dates, incidence$cases, col="blue")
  
  
  
  # incidence <- data.frame(incidence,"cases_roll" = cases_roll, "log_cases_roll" = log_cases_roll)
  incidence <- data.frame(incidence,"cases" = cases_roll, "log_cases" = log_cases_roll)
  for (i in i_start:i_end) {
    incidence_window_raw <- incidence[(i-incidence_window_half_width):(i+incidence_window_half_width),]
    # Get rid of days with zero incidence:
    # indices <- (incidence_window_raw$cases_roll > 0 & !is.na(incidence_window_raw$cases_roll))
    indices <- (incidence_window_raw$cases > 0 & !is.na(incidence_window_raw$cases))
    if (sum(indices) >= 3) { # Only compute regression fit if we have 3 or more days with nonzero incidence in the current window
      incidence_window <- incidence_window_raw[indices,]
      # fit <- lm(log_cases_roll~dates, data=incidence_window)
      fit <- lm(log_cases~dates, data=incidence_window)
      fit_summary <- summary(fit) 
      intercept <- fit_summary$coefficients["(Intercept)","Estimate"] 
      slope <- fit_summary$coefficients["dates","Estimate"]
      doubling_time <- log(2)/slope
      incidence$slope[i] <- slope
      incidence$intercept[i] <- intercept
      incidence$doubling_time[i] <- log(2)/slope
      incidence$R[i] <- R0_SEIR(slope,sigma_SEIR,gamma_SEIR)
      # FOR TESTING:
      if (diagnostic_mode_TF) {
        plot1 <- ggplot(data=incidence,aes(x=dates,y=log_cases)) +
        geom_point() +
        geom_point(data=incidence_window,aes(x=dates,y=log_cases),color="red") + 
        geom_abline(slope=slope,intercept=intercept,color="red") +
        labs(title = paste("rho =",slope))
        print(plot1) 
        browser()
      }
    }
  }
  # Compute % change in R relative to R0 for the initial exponential portion as calculated
  # by find_exponential_portion_v3f()
  
  # R_initial_index <- which(!is.na(incidence$R))[3]; R_initial <- incidence$R[R_initial_index]
  # R_over_R_initial <- incidence$R/R_initial*100
  R_over_R_initial <- incidence$R/initial_exp_growth_result$R*100
  incidence <- data.frame(incidence,"R_over_R_initial" = R_over_R_initial)

  
  # hack to get legend without having to reshape data frames:
  if(nrow(google_mobi$df) > 0) {
    colors <- c("retail + recr" = "blue",
                "grocery + pharm" = "red",
                "parks" = "green",
                "transit" = "darkblue",
                "workplaces" = "orange",
                "residential" = "darkblue",
                "R (reproduction number)" = "purple",
                "mean recr/grocery/trans/work"="cyan")
    # "driving (A)" = "black",
    # "walking (A)" = "darkgreen",
    # "transit (A)" = "cyan")
    plot5 <- ggplot(data = google_mobi$df,aes(date, recreation_roll, color = "retail + recr")) + geom_line() +
      geom_line(aes(x=date,y=grocery_roll, color="grocery + pharm")) +
      geom_line(aes(x=date,y=parks_roll, color="parks")) +
      geom_line(aes(x=date,y=transit_roll, color="transit")) +
      geom_line(aes(x=date,y=workplaces_roll, color="workplaces")) +
      geom_line(aes(x=date,y=residential_roll, color="residential")) +
      geom_line(aes(x=date,y=mean_rec_groc_trans_work, color="mean recr/grocery/trans/work"),size=1) +
      geom_line(data=incidence,aes(x=dates,y=R_over_R_initial, color = "R (reproduction number)"),size=1) +
      geom_vline(xintercept = google_mobi$date_mean_rec_groc_trans_work_mid, color="cyan", size=1) +
      geom_vline(xintercept = initial_exp_growth_result$turnover_date, color="purple", size=1) +
      ylim(0,150) +
      labs(x= "date", y="percent baseline", color="Legend", title = paste0(mobility_country,": % change in reproduction number (R) and mobility (Google)")) +
      scale_color_manual(values=colors) 
    print(plot5)
    ggsave(path = pathname, filename = paste0(mobility_country,"_",mobility_region,"_",mobility_subregion,"_mobi_and_R.png"))
  }
  
  return(list("incidence"=incidence,"google_mobility"=google_mobi, "initial_exp_growth"=initial_exp_growth_result))

}