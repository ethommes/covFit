
process_US_trial_sites <- function(augmented_trial_sites_table, states_table, forecast_date) {
  
  dummy_date <- as.Date("2020-06-30")
  df <- forecast_from_US_states_or_counties_frame(augmented_trial_sites_table, dummy_date, forecast_date)
  
  browser()
  simple_sites_table <- data.frame("sitename" = df$sitename,
                                   "PI.Last.name" = df$PI.Last.name,
                                   "Institution" = df$Institution,
                                   "Address" = df$Address,
                                   "City" = df$City,
                                   "State" = df$State,
                                   "County" = df$counties.x,
                                   "FIPS" = df$FIPS,
                                   "Population" = df$pop,
                                   "post_turnover_R" = df$R_post_exp,
                                   "cumulative_cases_per_100k_corrected" = df$cumu_cases_per_100k_corrected,
                                   "current_daily_cases_per_100k" = df$incidence_current,
                                   "current_R" = df$R_current,
                                   "forecast_date" = forecast_date,
                                   "forecast_incidence_from_post_turnover" = df$forecast_incidence_2_from_post_turnover,
                                   "forecast_incidence_from_recent" = df$forecast_incidence_2_from_recent
                                   
  )
  browser()
    
  
  return(simple_sites_table)
}