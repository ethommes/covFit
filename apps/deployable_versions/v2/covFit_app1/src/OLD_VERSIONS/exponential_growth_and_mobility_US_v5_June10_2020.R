# Takes a data frame df(states,counties,populations,densities_kmsq) and output the same frame
# with turnover point, rho, doubling time, R added
# _v2: Try to speed up by only doing mobility stuff for counties which satisfy threshold


exponential_growth_and_mobility_US_v5 <- function(
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
                                          onset_date_override,
                                          turnover_date_override,
                                          window_for_current_R,
                                          sigma_SEIR,
                                               gamma_SEIR,
                                          total_case_threshold,
                                          plot_TF,
                                          pause_TF,
                                          pathname,
                                          fips_and_land_area_table) {
  
  frame <- data.frame(
    "states" = covid_data$Province_State,
    "counties" = covid_data$Admin2,
    "FIPS" = covid_data$FIPS,
    "pop" = covid_death_data$Population
    # "area_sq_miles" = NA,
    # "density_sq_miles" = NA
  )
  
  # This data file contains also the county name, allowing us to add this as a column:
  fips_raw <-
    read_csv(
      "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
      col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp")
    )
  # fips <- data.frame(fips_raw,"FIPS"=paste(fips_raw$statefp,fips_raw$countyfp,sep=""))
  fips <- data.frame("FIPS"=as.numeric(paste(fips_raw$statefp,fips_raw$countyfp,sep="")),"county_full_name" = fips_raw$county.name)
  frame <- left_join(frame,fips,by="FIPS")
  
  n_rows <- nrow(frame)
  empty_date_vector <- rep(NA,n_rows); class(empty_date_vector) <- "Date"
  df1 <- data.frame(frame,
                    "total_cases" = rep(NA,n_rows),
                    "cases_per_100k" = rep(NA,n_rows),
                    "turnover_date"=empty_date_vector,
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
                    "mean_mobi_min" = rep(NA,n_rows),
                    "County_and_State" = paste(frame$counties,frame$states)
  )
  # Join the land area column from fips_and_land_area_table to df:
  fips_pop_area_density <- data.frame(
    "FIPS" = fips_and_land_area_table$fips,
    # "land area_sq_meter" = fips_and_land_area_table$LND110210,
    "pop_2010" = fips_and_land_area_table$POP010210,
    "pop_density_per_sq_mi" = fips_and_land_area_table$POP060210
  )
  # fips_and_area_only <- data.frame("FIPS" = fips_and_land_area_table$fips, "land_area" = fips_and_land_area_table$LND110210)
  # df <- merge(df1,fips_and_area_only, by="FIPS")
  
  # df <- left_join(df1,fips_and_area_only,by="FIPS") # TEST: try left join rather than the default (inner?)
  df <- left_join(df1,fips_pop_area_density,by="FIPS")
  
  # Compute county density:
  # meters_per_mile <- 0.000621371
  # df$density_sq_miles <- df$pop/(df$land_area*meters_per_mile^2)
  
  for (i in 1:n_rows) {
  # for (i in 65:225) {
  
    # print(paste(i,"Doing",df$counties[i],df$states[i],""))
    cat("\f", i,  "Doing ", df$counties[i], df$states[i])
    flush.console()
    # if (df$counties[i] == "Kusilvak") {browser()}
    # if (i==87) {browser()}
    incidence_frame <- jhu_covid_get_region_incidence_US_v2(covid_data,
                                                            covid_death_data,
                                                            start_date,
                                                            df$states[i],
                                                            T,
                                                            df$counties[i],
                                                            plot_TrueFalse = F,
                                                            npop=frame$pop[i],
                                                            normalize_by_npop = T)
    cumu_cases <- incidence_frame$cumu_cases[nrow(incidence_frame)]
    df$total_cases[i] <- cumu_cases
    df$cases_per_100k[i] <- cumu_cases/df$pop[i]*1e5
    # Estimate current R using the last [window_for_current_R] days:
    current_rho_R <- compute_current_rho_and_R(incidence_frame,window_for_current_R,sigma_SEIR,gamma_SEIR)
    df$rho_current[i] <- current_rho_R$rho_current
    df$R_current[i] <- current_rho_R$R_current
    google_mobi <- read_google_mobility(google_mobility,
                                        mobility_country = "United States", 
                                        mobility_region = df$states[i], 
                                        mobility_subregion = df$county_full_name[i],
                                        mobility_window_half_width = mobility_window_half_width)
    df$recreation_mobi_midpoint_date[i] <- google_mobi$date_recreation_mid
    df$grocery_mobi_midpoint_date[i] <- google_mobi$date_grocery_mid
    df$parks_mobi_midpoint_date[i] <- google_mobi$date_parks_mid
    df$transit_mobi_midpoint_date[i] <- google_mobi$date_transit_mid
    df$workplaces_mobi_midpoint_date[i] <- google_mobi$date_workplaces_mid
    df$residential_mobi_midpoint_date[i] <- google_mobi$date_residential_mid
    df$mean_mobi_midpoint_date[i]  <- google_mobi$date_mean_rec_groc_trans_work_mid
    df$mean_mobi_min[i] <- google_mobi$min_of_mean_rec_groc_trans_work
    # 
    
    # If there are too few cases [by a metric to be determined], skip this county for rest of analysis 
    # and fill in NAs for everything else
    if (cumu_cases >= total_case_threshold) {
      if (pause_TF) {browser()} # optionally, pause on each county for closer inspection/debugging
      # if (df$counties[i] == "Fresno") {browser()}
      v_exp_portion <- find_exponential_portion_v3f(incidence_list =  incidence_frame,
                                                    population = frame$pop[i],
                                                    N_days_to_aggregate =  N_days_to_aggregate,
                                                    daily_case_threshold =  daily_case_threshold_frac,
                                                    cumu_case_threshold = cumu_case_threshold*frame$pop[i],
                                                    max_starting_index = max_onset_delay,
                                                    onset_date_override,
                                                    turnover_date_override,
                                                    window_for_current_R = window_for_current_R,
                                                    plot_current_R_TF = T,
                                                    # cumu_case_threshold = cumu_case_threshold,
                                                    # daily_case_threshold_frac*frame$pop[i],
                                                    sigma_SEIR,
                                                    gamma_SEIR,
                                                    plot_TF = plot_TF,
                                                    # title = paste(df$counties[i], df$states[i]),
                                                    county = df$counties[i],
                                                    state = df$states[i],
                                                    pathname = pathname)
      df$turnover_date[i] <- v_exp_portion$turnover_date
      df$rho[i]  <- v_exp_portion$rho
      df$doubling_time[i] <- v_exp_portion$doubling_time
      df$R[i] <- v_exp_portion$R
      df$rho_post_exp[i]  <- v_exp_portion$rho_post_exp
      df$doubling_time_post_exp[i] <- v_exp_portion$doubling_time
      df$R_post_exp[i] <- v_exp_portion$R_post_exp

      # NEW for _v5:
      # incidence <- data.frame(incidence_frame, 
      #                         "log_cases" = log(incidence_frame$cases), 
      #                         "slope" = NA,
      #                         "intercept" = NA,
      #                         "doubling_time" = NA,
      #                         "R" = NA)
      # n_days_incidence <- nrow(incidence)
      # i_start <- incidence_window_half_width + 1
      # i_end <- n_days_incidence - incidence_window_half_width - 1
      # incidence_window <- incidence_window_half_width*2 + 1
      # browser()
      
    } else {
      df$turnover_date[i] <- NA
      df$rho[i]  <- NA
      df$doubling_time[i] <- NA
      df$R[i] <- NA
      df$rho_post_exp[i]  <- NA
      df$doubling_time_post_exp[i] <- NA
      df$R_post_exp[i] <- NA
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
        labs(title = paste("Google mobility, ",df$County_and_State[i],sep=""), x= "date", y="percent baseline", color="Legend") +
        scale_color_manual(values=colors) +
        ylim(0,150)
      filename <- paste(df$counties[i],"_",df$states[i],".png",sep="")
      ggsave(path=pathname,filename = filename,width=6,height=4)
  }
    
    
    }
  
  return(df)
}