
rolling_R_and_incidence <- function(incidence, predict_start_date, predict_end_date, predict_window, inputs) {

  with(inputs, {
    predict_start_date <- predict_from_date
    predict_end_date <- predict_date
    predict_window <- inputs$most_recent_R_window
    # Remove all dates before the first case
    first_case_index <- min(which(incidence$cumu_cases > 0))
    incidence <- incidence[first_case_index:nrow(incidence),]
    
    df_rho <- smoothed_incidence_and_rho(incidence, R_window_size, "right")
    R_roll <- R0_SEIR(df_rho$rho_roll, sigma_SEIR, gamma_SEIR)

    # Don't let the first date to predict be later than the most recent case data:
    if (predict_start_date > df_rho$dates[nrow(df_rho)]) {predict_start_date <- df_rho$dates[nrow(df_rho)]}
    
    # Create a date vector for the forecast:
    dates_forecast <- seq(predict_start_date, predict_end_date, by="day")
    
    # Create a subframe of df_rho from which to calculate the forecast:
    df_rho_sub <- df_rho[df_rho$dates <= predict_start_date & df_rho$dates >= (predict_start_date - predict_window),]
    log_cases_0 <- df_rho_sub$log_cases_roll[nrow(df_rho_sub)] 
    # rho_mid <- df_rho_sub$rho_roll[nrow(df_rho_sub)]
    rho_mid <- mean(df_rho_sub$rho_roll)
    rho_min <- min(df_rho_sub$rho_roll)
    rho_max <- max(df_rho_sub$rho_roll)
    
    # The forecasts:
    log_cases_forecast_mid <- as.numeric(log_cases_0 + rho_mid*(dates_forecast - predict_start_date))
    log_cases_forecast_min <- as.numeric(log_cases_0 + rho_min*(dates_forecast - predict_start_date))
    log_cases_forecast_max <- as.numeric(log_cases_0 + rho_max*(dates_forecast - predict_start_date))
    
    forecast_frame <- data.frame("dates" = dates_forecast,
                                 "log_cases_mid" = log_cases_forecast_mid,
                                 "log_cases_min" = log_cases_forecast_min,
                                 "log_cases_max" = log_cases_forecast_max)
    
    output_to_return <- list(
      "incidence" = incidence,
      "rolling_values" = df_rho,
      "rolling_values_for_forecast" = df_rho_sub,
      "forecast" = forecast_frame
    )
    
    
    return(output_to_return)
    
  })
}