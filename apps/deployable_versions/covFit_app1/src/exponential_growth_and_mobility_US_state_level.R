# Takes a data frame df(states,counties,populations,densities_kmsq) and output the same frame
# with turnover point, rho, doubling time, R added
# _v2: Try to speed up by only doing mobility stuff for counties which satisfy threshold


exponential_growth_and_mobility_US_state_level <- function(
                                          covid_data,
                                          covid_death_data,
                                          # apple_mobility,
                                          google_mobility,
                                          mobility_window_half_width,
                                          start_date,
                                               N_days_to_aggregate,
                                               daily_case_threshold_frac, # as fraction of total population
                                          cumu_case_threshold, 
                                          max_onset_delay, # After this number of days of nonzero incidence at the latest, outbreak is deemed to have begun
                                          override_table,
                                          interval_type,
                                          window_for_current_R,
                                          predict_date,
                                          sigma_SEIR,
                                               gamma_SEIR,
                                          total_case_threshold,
                                          plot_TF,
                                          pause_TF,
                                          pathname,
                                          fips_and_land_area_table) {
  
  # left-join tables covid19_data and fips_and_land_area_table:
  # master_table <- left_join(covid19_US, fips_and_land_area_table, by=c("FIPS" = "fips"))
  
  # Vector of all unique Province_State entries:
  states <- unique(covid19_US$Province_State)
  
  # Remove what needs removing:
  # states <- subset(states, states != "Diamond Princess" &
  #                    states != "Grand Princess") 
  ind <- states != "Diamond Princess" & states != "Grand Princess"
  states <- states[ind]
  override_table <- override_table[ind,]
  
  n_rows <- length(states)
  empty_date_vector <- rep(NA,n_rows); class(empty_date_vector) <- "Date"
  
  # set up (part of) master data frame:
  df <- data.frame(
    "states" = states,
    "state_abbr" = setNames(state.abb, state.name)[states],
    "pop" = rep(NA,n_rows),
    "total_cases" = rep(NA,n_rows),
    "cases_per_100k" = rep(NA,n_rows),
    "pop_density_per_sq_mi" = rep(NA,n_rows),
    "onset_date" = empty_date_vector,
    "onset_date_OVERRIDE" = override_table$onset_date_override,
    "turnover_date"=empty_date_vector,
    "turnover_date_OVERRIDE" = override_table$turnover_date_override,
    "incidence_at_turnover" = rep(NA,n_rows),
    "cumu_incidence_at_turnover" = rep(NA,n_rows),
    "rho"=rep(NA,n_rows),
    "doubling_time"=rep(NA,n_rows),
    "R" = rep(NA,n_rows),
    "rho_post_exp" = rep(NA,n_rows),
    "doubling_time_post_exp" = rep(NA,n_rows),
    "R_post_exp" = rep(NA,n_rows),
    "rho_current" = rep(NA,n_rows),
    "R_current" = rep(NA,n_rows),
    "recreation_mobi_midpoint_date" = empty_date_vector,
    "grocery_mobi_midpoint_date" = empty_date_vector,
    "parks_mobi_midpoint_date" = empty_date_vector,
    "transit_mobi_midpoint_date" = empty_date_vector,
    "workplaces_mobi_midpoint_date" = empty_date_vector,
    "residential_mobi_midpoint_date" = empty_date_vector,
    "mean_mobi_midpoint_date" = empty_date_vector,
    "mean_mobi_min" = rep(NA,n_rows)
  )

  # Loop over states:
  for (i in 1:length(states)) {
    state <- states[i]
    cat("\f", i,  "Doing ", state)
    flush.console()
    # Take subset of covid_data frame which includes only the counties within the current state, then left-join it 
    # to fips_and_land_area_table 
    covid_data_subset <- subset(covid_data, Province_State == states[i])
    covid_death_data_subset <- subset(covid_death_data, Province_State == states[i])
    # covid_death_data includes the population, so use that:  
    pop_state <- sum(covid_death_data_subset$Population)
    fips_vector <- covid_data_subset$FIPS
    dfTemp <- data.frame("fips"=fips_vector)
    dfTemp <- left_join(dfTemp,fips_and_land_area_table, by="fips")
    # Sum over the respective columns to get the total population and land area for the current state
    # Use 2012 estimate for population (newest in the file)
    pop_2 <- sum(subset(dfTemp$PST045212,is.finite(dfTemp$PST045212)))
    land_area <- sum(subset(dfTemp$LND110210,is.finite(dfTemp$LND110210)))
    pop_density <- pop_state/land_area
    df$pop_density_per_sq_mi[i] <- pop_density
    incidence_frame <- jhu_covid_get_region_incidence_US_v2(covid_data,
                                                           covid_death_data,
                                                           start_date,
                                                           state,
                                                           F,
                                                           "",
                                                           plot_TrueFalse = F,
                                                           npop=pop_state,
                                                           normalize_by_npop = F)
    df$pop[i] <- pop_state
    cumu_cases <- incidence_frame$cumu_cases[nrow(incidence_frame)]
    df$total_cases[i] <- cumu_cases
    df$cases_per_100k[i] <- cumu_cases/pop_state*1e5
    # Estimate current R using the last [window_for_current_R] days:
    current_rho_R <- compute_current_rho_and_R(incidence_frame,window_for_current_R,sigma_SEIR,gamma_SEIR)
    df$rho_current[i] <- current_rho_R$rho_current
    df$R_current[i] <- current_rho_R$R_current
    google_mobi <- read_google_mobility(google_mobility,
                                        mobility_country = "United States", 
                                        mobility_region = df$states[i], 
                                        mobility_subregion = "",
                                        mobility_window_half_width = mobility_window_half_width)
    df$recreation_mobi_midpoint_date[i] <- google_mobi$date_recreation_mid
    df$grocery_mobi_midpoint_date[i] <- google_mobi$date_grocery_mid
    df$parks_mobi_midpoint_date[i] <- google_mobi$date_parks_mid
    df$transit_mobi_midpoint_date[i] <- google_mobi$date_transit_mid
    df$workplaces_mobi_midpoint_date[i] <- google_mobi$date_workplaces_mid
    df$residential_mobi_midpoint_date[i] <- google_mobi$date_residential_mid
    df$mean_mobi_midpoint_date[i]  <- google_mobi$date_mean_rec_groc_trans_work_mid
    df$mean_mobi_min[i] <- google_mobi$min_of_mean_rec_groc_trans_work
    if (cumu_cases >= total_case_threshold) {
      v_exp_portion <- find_exponential_portion_v3f(incidence_list =  incidence_frame,
                                                    population = pop_state,
                                                    N_days_to_aggregate =  N_days_to_aggregate,
                                                    daily_case_threshold =  daily_case_threshold_frac,
                                                    cumu_case_threshold = cumu_case_threshold*pop_state,
                                                    max_starting_index = max_onset_delay,
                                                    df$onset_date_OVERRIDE[i],
                                                    df$turnover_date_OVERRIDE[i],
                                                    interval_type,
                                                    window_for_current_R = window_for_current_R,
                                                    predict_date = predict_date,
                                                    plot_current_R_TF = T,
                                                    # cumu_case_threshold = cumu_case_threshold,
                                                    # daily_case_threshold_frac*frame$pop[i],
                                                    sigma_SEIR,
                                                    gamma_SEIR,
                                                    plot_TF = plot_TF,
                                                    # title = paste(df$counties[i], df$states[i]),
                                                    title = state,
                                                    # county = "",
                                                    # state = state,
                                                    pathname = pathname)
      df$turnover_date[i] <- v_exp_portion$turnover_date
      df$incidence_at_turnover[i] <- incidence_frame$cases[incidence_frame$dates==v_exp_portion$turnover_date] # [TO IMPROVE]: Should really be averaging about this date!
      df$cumu_incidence_at_turnover[i] <- incidence_frame$cumu_cases[incidence_frame$dates==v_exp_portion$turnover_date]
      df$rho[i]  <- v_exp_portion$rho
      df$doubling_time[i] <- v_exp_portion$doubling_time
      df$R[i] <- v_exp_portion$R
      df$rho_post_exp[i]  <- v_exp_portion$rho_post_exp
      df$doubling_time_post_exp[i] <- v_exp_portion$doubling_time
      df$R_post_exp[i] <- v_exp_portion$R_post_exp
    }
    if (nrow(google_mobi$df) > 0 & sum(is.finite(google_mobi$df$mean_rec_groc_trans_work)) >0 & df$total_cases[i] >= total_case_threshold) {
      # print(paste("Plotting",df$counties[i],df$states[i]))
      
      # colors <- c("retail + recr" = "blue",
      #             "grocery + pharm" = "red",
      #             "parks" = "green",
      #             "transit" = "darkblue",
      #             "workplaces" = "orange",
      #             "residential" = "darkblue",
      #             "R (reproduction number)" = "purple",
      #             "mean recr/grocery/trans/work"="cyan")
      # # "driving (A)" = "black",
      # # "walking (A)" = "darkgreen",
      # # "transit (A)" = "cyan")
      # plot5 <- ggplot(data = google_mobi$df,aes(date, recreation_roll, color = "retail + recr")) + geom_line() +
      #   geom_line(aes(x=date,y=grocery_roll, color="grocery + pharm")) +
      #   geom_line(aes(x=date,y=parks_roll, color="parks")) +
      #   geom_line(aes(x=date,y=transit_roll, color="transit")) +
      #   geom_line(aes(x=date,y=workplaces_roll, color="workplaces")) +
      #   geom_line(aes(x=date,y=residential_roll, color="residential")) +
      #   geom_line(aes(x=date,y=mean_rec_groc_trans_work, color="mean recr/grocery/trans/work"),size=1) +
      #   geom_line(data=incidence,aes(x=dates,y=R_over_R_initial, color = "R (reproduction number)"),size=1) +
      #   geom_vline(xintercept = google_mobi$date_mean_rec_groc_trans_work_mid, color="cyan", size=1) +
      #   geom_vline(xintercept = initial_exp_growth_result$turnover_date, color="purple", size=1) +
      #   ylim(0,150) +
      #   labs(x= "date", y="percent baseline", color="Legend", title = paste0(mobility_country,": % change in reproduction number (R) and mobility (Google)")) +
      #   scale_color_manual(values=colors) 
      # print(plot5)      
      
      # ================
      
      colors <- c("retail + recr (G)" = "black",
                  "grocery + pharm (G)" = "brown",
                  "parks (G)" = "green",
                  "transit (G)" = "blue",
                  "workplaces (G)" = "orange",
                  "residential (G)" = "cyan",
                  "mean (G)" = "red")
      plot5 <- ggplot(data = google_mobi$df,aes(date, recreation_roll, color = "retail + recr (G)")) + geom_line() +
        geom_vline(xintercept = df$recreation_mobi_midpoint_date[i],color="black",size=1) +
        geom_line(aes(x=date,y=grocery_roll, color="grocery + pharm (G)")) +
        geom_vline(xintercept = df$grocery_mobi_midpoint_date[i],color="brown", size=1, linetype="dashed") +
        geom_line(aes(x=date,y=parks_roll, color="parks (G)")) +
        geom_vline(xintercept = df$parks_mobi_midpoint_date[i],color="green", size=1, linetype = "dotted") +
        geom_line(aes(x=date,y=transit_roll, color="transit (G)")) +
        geom_vline(xintercept = df$transit_mobi_midpoint_date[i],color="blue", size=0.75) +
        geom_line(aes(x=date,y=workplaces_roll, color="workplaces (G)")) +
        geom_vline(xintercept = df$workplaces_mobi_midpoint_date[i],color="orange", size=0.75, linetype="dashed") +
        geom_line(aes(x=date,y=residential_roll, color="residential (G)")) +
        geom_vline(xintercept = df$residential_mobi_midpoint_date[i],color="cyan", size=0.75, linetype="dotted") +
        geom_line(aes(x=date,y=mean_rec_groc_trans_work, color="mean (G)")) +
        geom_vline(xintercept = df$mean_mobi_midpoint_date[i],color="red", size=2, linetype="dotted") +
        # geom_line(data=apple_mobi$df,aes(x=date,y=driving_roll, color = "driving (A)"),linetype = 2) +
        # geom_line(data=apple_mobi$df,aes(x=date,y=walking_roll, color = "walking (A)"),linetype = 2) +
        # geom_line(data=apple_mobi$df,aes(x=date,y=transit_roll, color = "transit (A)"),linetype = 2) +
        labs(title = paste0("Google mobility, ",df$states[i]), x= "date", y="percent baseline", color="Legend") +
        scale_color_manual(values=colors) +
        ylim(0,200)
      filename <- paste0(df$states[i],"_mobility.png")
      ggsave(path=pathname,filename = filename,width=6,height=4)
    }
  }

  
  return(df)
}