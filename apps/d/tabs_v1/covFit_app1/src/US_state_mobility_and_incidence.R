
US_state_mobility_and_incidence <- function(input) {
  with(input,{
    # left-join tables covid19_data and fips_and_land_area_table:
    # master_table <- left_join(covid19_US, fips_and_land_area_table, by=c("FIPS" = "fips"))
    
    # Vector of all unique Province_State entries:
    states <- unique(covid19_US$Province_State)
    
    # Remove what needs removing:
    # states <- subset(states, states != "Diamond Princess" &
    #                    states != "Grand Princess") 
    ind <- states != "Diamond Princess" & states != "Grand Princess" & states != "American Samoa"
    states <- states[ind]
    override_table <- override_table[ind,]
    
    n_rows <- length(states)
    empty_date_vector <- rep(NA,n_rows); class(empty_date_vector) <- "Date"
    
    # set up (part of) master data frame:
    df <- data.frame(
      "last_date" = empty_date_vector,
      "states" = states,
      "state_abbr" = setNames(state.abb, state.name)[states],
      "pop" = rep(NA,n_rows),
      "CFR_applied" = rep(NA,n_rows),
      "cfr_observed" = rep(NA,n_rows),
      "cfr_correction_factor" = rep(NA,n_rows),
      "cfr_lag" = rep(NA,n_rows),
      "cumu_cases_raw" = rep(NA,n_rows),
      "cumu_cases_corrected"= rep(NA,n_rows),
      "cumu_cases_per_100k_raw" = rep(NA,n_rows),
      "cumu_cases_per_100k_corrected" = rep(NA,n_rows),
      "cumu_deaths" = rep(NA,n_rows),
      "cumu_deaths_per_100k" = rep(NA,n_rows),
      "pop_density_per_sq_mi" = rep(NA,n_rows),
      "onset_date" = empty_date_vector,
      "onset_date_OVERRIDE" = override_table$onset_date_override,
      "turnover_date"=empty_date_vector,
      "turnover_date_OVERRIDE" = override_table$turnover_date_override,
      "cumu_cases_at_turnover_raw" = rep(NA,n_rows),
      "cumu_cases_at_turnover_corrected" = rep(NA,n_rows),
      "cumu_cases_at_turnover_per_100k_raw" = rep(NA,n_rows),
      "cumu_cases_at_turnover_per_100k_corrected" = rep(NA,n_rows),
      "rho"=rep(NA,n_rows),
      "doubling_time"=rep(NA,n_rows),
      "R" = rep(NA,n_rows),
      "rho_post_exp" = rep(NA,n_rows),
      "doubling_time_post_exp" = rep(NA,n_rows),
      "R_post_exp" = rep(NA,n_rows),
      "window_for_current" = rep(NA, n_rows),
      "rho_current" = rep(NA,n_rows),
      "doubling_time_current" = rep(NA,n_rows),
      "R_current" = rep(NA,n_rows),
      "incidence_current" = rep(NA,n_rows),
      # "recreation_mobi_midpoint_date" = empty_date_vector,
      # "grocery_mobi_midpoint_date" = empty_date_vector,
      # "parks_mobi_midpoint_date" = empty_date_vector,
      # "transit_mobi_midpoint_date" = empty_date_vector,
      # "workplaces_mobi_midpoint_date" = empty_date_vector,
      "residential_mobi_midpoint_date" = empty_date_vector,
      "residential_mobi_min" = rep(NA,n_rows),
      "residential_mobi_max" = rep(NA,n_rows),
      "mean_mobi_midpoint_date" = empty_date_vector,
      "mean_mobi_min" = rep(NA,n_rows)
    )
    # Loop over states:
    
    # Manually add abbreviations for DC, Puerto Rico, Guam etc
    df$state_abbr[1] <- "GU"
    df$state_abbr[2] <- "MP"
    df$state_abbr[3] <- "PR"
    df$state_abbr[4] <- "VI"
    df$state_abbr[13] <- "DC"
    
    for (i in 1:length(states)) {
    # for (i in 10:15) {
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
      df$pop[i] <- pop_state
      df$pop_density_per_sq_mi[i] <- pop_density
      
      input$onset_date_override <- df$onset_date_OVERRIDE[i]
      input$turnover_date_override <- df$turnover_date_OVERRIDE[i]
      input$Country.Region <- "US"
      input$Province.State <- states[i]
      input$Admin2 = ""
      input$mobility_country = "United States"
      input$mobility_region = states[i]
      input$mobility_subregion = ""
      input$lags_vector <- c(10,0) # PLACEHOLDER
      
      state_output <- region_mobility_and_incidence_v6(input)
      
      df$last_date[i] <- state_output$last_date
      df$onset_date[i] <- state_output$onset_date
      df$turnover_date[i] <- state_output$turnover_date
      df$cumu_cases_raw[i] <- state_output$cumu_cases_raw
      df$cumu_cases_corrected[i] <- state_output$cumu_cases_corrected
      df$cumu_cases_per_100k_raw[i] <- state_output$cumu_cases_per_100k_raw
      df$cumu_cases_per_100k_corrected[i] <- state_output$cumu_cases_per_100k_corrected
      df$cumu_deaths[i] <- state_output$cumu_deaths
      df$cumu_deaths_per_100k[i] <- state_output$cumu_deaths_per_100k
      df$CFR_applied[i] <- state_output$CFR_applied
      df$cfr_observed[i] <- state_output$cfr_observed
      df$cfr_lag[i] <- state_output$cfr_lag
      df$cfr_correction_factor[i] <- state_output$cfr_correction_factor
      df$cumu_cases_at_turnover_raw[i] <- state_output$cumu_cases_at_turnover_raw
      df$cumu_cases_at_turnover_corrected[i] <- state_output$cumu_cases_at_turnover_corrected
      df$cumu_cases_at_turnover_per_100k_raw[i] <- state_output$cumu_cases_at_turnover_per_100k_raw
      df$cumu_cases_at_turnover_per_100k_corrected[i] <- state_output$cumu_cases_at_turnover_per_100k_corrected
      df$rho[i] <- state_output$rho
      df$doubling_time[i] <- state_output$doubling_time
      df$R[i] <- state_output$R
      df$rho_post_exp[i] <- state_output$rho_post_exp
      df$doubling_time_post_exp[i] <- state_output$doubling_time_post_exp
      df$R_post_exp[i] <- state_output$R_post_exp
      df$window_for_current[i] <- state_output$window_for_current_R
      df$rho_current[i] <- state_output$rho_current
      df$doubling_time_current[i] <- state_output$doubling_time_current
      df$R_current[i] <- state_output$R_current
      df$incidence_current[i] <- state_output$incidence_current
      df$residential_mobi_max[i] <- state_output$residential_max
      df$residential_mobi_min[i] <- state_output$residential_min
      df$residential_mobi_midpoint_date[i] <- state_output$residential_mobi_midpoint
    }
    return(df)
    
  })
}