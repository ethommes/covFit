
process_US_trial_sites <- function(augmented_trial_sites_table, states_table, forecast_date) {
  
  dummy_date <- as.Date("2020-06-30")
  df <- forecast_from_US_states_or_counties_frame(augmented_trial_sites_table, dummy_date, forecast_date)
  
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
                                   "cumulative_cases_per_100k" = df$cumu_cases_per_100k_corrected,
                                   "current_daily_cases_per_100k" = df$incidence_current/df$pop*1e5,
                                   "current_R" = df$R_current,
                                   "forecast_date" = forecast_date,
                                   "forecast_incidence_from_post_turnover" = df$forecast_incidence_2_from_post_turnover,
                                   "forecast_incidence_from_recent" = df$forecast_incidence_2_from_recent
                                   
  )
  df_state <- forecast_from_US_states_or_counties_frame(states_table, dummy_date, forecast_date)
  states_addon_frame <- data.frame(
    "State" = states_table$state_abbr, 
    "STATE_post_turnover_R" = states_table$R_post_exp,
    "STATE_current_daily_cases_per_100k" = states_table$incidence_current/states_table$pop*1e5,
    "STATE_current_R" = states_table$R_current
    )
  simple_sites_table <- left_join(simple_sites_table, states_addon_frame, by = "State")
  simple_sites_table <- data.frame(simple_sites_table,
                                   "COUNTY_under_control_since_turnover" = NA,
                                   "COUNTY_under_control_currently" = NA,
                                   "STATE_under_control_since_turnover" = NA,
                                   "STATE_under_control_currently" = NA)
  simple_sites_table$COUNTY_under_control_since_turnover <- (simple_sites_table$post_turnover_R < 1)
  simple_sites_table$COUNTY_under_control_currently <- (simple_sites_table$current_R < 1)
  simple_sites_table$STATE_under_control_since_turnover <- (simple_sites_table$STATE_post_turnover_R < 1)
  simple_sites_table$STATE_under_control_currently <- (simple_sites_table$STATE_current_R < 1)


  
  return(simple_sites_table)
}