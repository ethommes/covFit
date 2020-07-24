
rolling_R_and_incidence <- function(incidence, predict_start_date, predict_end_date, onset_date, turnover_date, inputs) {

  with(inputs, {
    predict_start_date <- predict_from_date
    predict_end_date <- predict_date
    # predict_window_start <- inputs$onset_date_override
    
    # Remove all dates before the first case
    # first_case_index <- min(which(incidence$cumu_cases > 0))
    # incidence <- incidence[first_case_index:nrow(incidence),]
    
    # ALT: Remove all dates before onset date (computed by find_exponential_portion_v7())
    incidence <- incidence[incidence$dates >= onset_date,]
    # Create a subframe of df_rho from which to calculate the forecast.
    incidence_sub <- incidence[incidence$dates <= predict_start_date & incidence$dates >= turnover_date,]

    df_rho <- smoothed_incidence_and_rho(incidence, R_window_size, rolling_mean_alignment)
    R_roll <- R0_SEIR(df_rho$rho_roll, sigma_SEIR, gamma_SEIR)
    df_rho <- data.frame(df_rho, "R_roll" = R_roll)
    
    df_rho_sub <- smoothed_incidence_and_rho(incidence_sub, R_window_size, rolling_mean_alignment)
    R_roll_sub <- R0_SEIR(df_rho_sub$rho_roll, sigma_SEIR, gamma_SEIR)
    df_rho_sub <- data.frame(df_rho_sub, "R_roll" = R_roll_sub)

    # Don't let the first date to predict be later than the most recent case data:
    if (predict_start_date > df_rho$dates[nrow(df_rho)]) {predict_start_date <- df_rho$dates[nrow(df_rho)]}
    
    # Create a date vector for the forecast:
    df_rho_sub_non_NA <- subset(df_rho_sub, !is.na(df_rho_sub$log_cases_roll)) # strip off the NAs produced by rolling mean
    predict_start_date_for_roll <- df_rho_sub_non_NA$dates[nrow(df_rho_sub_non_NA)] # forecast starts from last date after stripping off NAs
    dates_forecast <- seq(predict_start_date_for_roll, predict_end_date, by="day")
    log_cases_0 <- df_rho_sub_non_NA$log_cases_roll[nrow(df_rho_sub_non_NA)]
    
    # Create a subframe of df_rho from which to calculate the forecast.
    # df_rho_sub <- df_rho[df_rho$dates <= predict_start_date & df_rho$dates >= turnover_date,]
    # df_rho_sub <- df_rho[df_rho$dates <= predict_start_date & df_rho$dates >= (predict_start_date - predict_window),]
    
    
    # The rolling incidence at the end of the prediction window, i.e. the starting point for our predicted incidence:
    # log_cases_0 <- df_rho_sub$log_cases_roll[nrow(df_rho_sub)] 

    # avrg, min and max rho over the prediction window:
    # rho_roll_for_forecast <- subset(df_rho_sub$rho_roll, !is.na(df_rho_sub$rho_roll))
    rho_roll_for_forecast <- df_rho_sub_non_NA$rho_roll
    rho_mid <- mean(rho_roll_for_forecast)
    rho_min <- min(rho_roll_for_forecast)
    rho_max <- max(rho_roll_for_forecast)
    R_mid <- R0_SEIR(rho_mid, sigma_SEIR, gamma_SEIR)
    R_min <- R0_SEIR(rho_min, sigma_SEIR, gamma_SEIR)
    R_max <- R0_SEIR(rho_max, sigma_SEIR, gamma_SEIR)
    
    # The forecasts:
    # log_cases_forecast_mid <- as.numeric(log_cases_0 + rho_mid*(dates_forecast - predict_start_date))
    log_cases_forecast_mid <- as.numeric(log_cases_0 + rho_mid*(dates_forecast - predict_start_date_for_roll))
    log_cases_forecast_min <- as.numeric(log_cases_0 + rho_min*(dates_forecast - predict_start_date_for_roll))
    log_cases_forecast_max <- as.numeric(log_cases_0 + rho_max*(dates_forecast - predict_start_date_for_roll))
    
    forecast_frame <- data.frame("dates" = dates_forecast,
                                 "log_cases_mid" = log_cases_forecast_mid,
                                 "log_cases_min" = log_cases_forecast_min,
                                 "log_cases_max" = log_cases_forecast_max)
    
    # Estimate R0, as the maximum R between onset and turnover:
    df_rho_0 <- df_rho %>% subset(dates <= turnover_date)
    
    # incidence_init <- subset(incidence, incidence$dates <= turnover_date)
    # df_rho_0 <- smoothed_incidence_and_rho(incidence_init, R_window_size, rolling_mean_alignment)
    rho_0 <- df_rho_0$rho_roll %>% na.omit() %>% max()
    doubling_time_0 <- log(2)/rho_0
    # rho_0 <- max(df_rho_0$rho_roll)
    R0 <- R0_SEIR(rho_0, sigma_SEIR, gamma_SEIR)

    output_to_return <- list(
      "incidence" = incidence,
      "rolling_values" = df_rho,
      "rolling_values_for_forecast" = df_rho_sub,
      "forecast" = forecast_frame,
      "turnover_date" = turnover_date,
      "rho_0" = rho_0,
      "R0" = R0,
      "doubling_time_0" = doubling_time_0,
      "rho_mid" = rho_mid,
      "rho_min" = rho_min,
      "rho_max" = rho_max,
      "R_mid" = R_mid,
      "R_min" = R_min,
      "R_max" = R_max,
      "cases_0" = exp(log_cases_0),
      "forecast_start_date" = predict_start_date_for_roll
    )
    
    
    return(output_to_return)
    
  })
}