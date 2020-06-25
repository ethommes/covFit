
forecast_from_US_states_or_counties_frame <- function(df, proj_date_1, proj_date_2) {
  last_date <- df$last_date
  delta_t_1 <- as.integer(proj_date_1 - last_date)
  delta_t_2 <- as.integer(proj_date_2 - last_date)
  
  forecast_incidence_1_from_recent <- df$incidence_current*exp(df$rho_current*delta_t_1)
  forecast_incidence_1_from_post_turnover <- df$incidence_current*exp(df$rho_post_exp*delta_t_1)
  forecast_incidence_2_from_recent <- df$incidence_current*exp(df$rho_current*delta_t_2)
  forecast_incidence_2_from_post_turnover <- df$incidence_current*exp(df$rho_post_exp*delta_t_2)

  df_output <- data.frame(df,
                          "forecast_date_1" = proj_date_1, 
                          "forecast_incidence_1_from_recent" = forecast_incidence_1_from_recent,
                          "forecast_incidence_1_from_post_turnover" = forecast_incidence_1_from_post_turnover,
                          "forecast_date_2" = proj_date_2,
                          "forecast_incidence_2_from_recent" = forecast_incidence_2_from_recent,
                          "forecast_incidence_2_from_post_turnover" = forecast_incidence_2_from_post_turnover)
  
  return(df_output)

  
}