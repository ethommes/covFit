
rolling_R_and_incidence <- function(incidence, inputs) {

  with(inputs, {
    if (incidence$cumu_cases[nrow(incidence)]==0) {
      output_to_return <- ERROR_rolling_R_and_incidence(incidence)
      return(output_to_return)
    }
    predict_start_date <- predict_from_date
    predict_end_date <- predict_date

    # Onset date is the date on which cumulative case threshold is reached:
    # NOTE: Below we're multiplying onset_cumu_case_threshold by cfr_correction_factor so that the specified threshold is applied
    # to the UNCORRECTED incidence.  
    onset_cumu_case_threshold <- (cases_per_100k_threshold*cfr_correction_factor)*pop/1e5 
    onset_index <- min(which(incidence$cumu_cases >= onset_cumu_case_threshold))
    onset_date <- incidence$date[onset_index]
    
    # Turnover date:
    # turnover_date <- find_turnover_point_v2(incidence_frame = incidence,
    #                                         min_turnover_date = onset_date,
    #                                         max_turnover_date = onset_date+60,
    #                                         max_date_to_consider = onset_date+90)
    df <- incidence[onset_index:nrow(incidence),]
    turnover_date <- find_turnover_point_v2(incidence_frame = df,
                                            min_turnover_date = as.Date("2020-02-01"),
                                            max_turnover_date = as.Date("2020-05-01"),
                                            max_date_to_consider = as.Date("2020-06-01"))
    
    # Remove all dates before onset date 
    incidence <- incidence[incidence$dates >= onset_date,]
    # Create a subframe of df_rho from which to calculate the forecast.
    # If turnover_date = NA (i.e. we were unsuccessful in finding one), then
    # just set it to onset_date
    if (is.na(turnover_date)) {turnover_date <- onset_date}
    
    # If the start of the forecast window has been specified as earlier than the above, 
    # use the specified value instead:
    if (predict_data_start > turnover_date) {
      window_start <- predict_data_start
    } else {
      window_start <- turnover_date
    }
    
    incidence_sub <- incidence[incidence$dates <= predict_start_date & incidence$dates >= window_start,]

    df_rho <- smoothed_incidence_and_rho(incidence, R_window_size, rolling_mean_alignment)
    R_roll <- R0_SEIR(df_rho$rho_roll, sigma_SEIR, gamma_SEIR)
    df_rho <- data.frame(df_rho, "R_roll" = R_roll)
    
    # EXPERIMENTAL: try to construct an approximation of rho as a function of incidence
    # df_rho <- rho_from_incidence(df_rho, inputs)
    
    df_rho_sub <- smoothed_incidence_and_rho(incidence_sub, R_window_size, rolling_mean_alignment)
    R_roll_sub <- R0_SEIR(df_rho_sub$rho_roll, sigma_SEIR, gamma_SEIR)
    df_rho_sub <- data.frame(df_rho_sub, "R_roll" = R_roll_sub)
    
   
    
    
    

    # Don't let the first date to predict be later than the most recent case data:
    if (predict_start_date > df_rho$dates[nrow(df_rho)]) {predict_start_date <- df_rho$dates[nrow(df_rho)]}
    
    # Create a date vector for the forecast:
    
    df_rho_sub_non_NA <- subset(df_rho_sub, !is.na(df_rho_sub$log_cases_roll)) # strip off the NAs produced by rolling mean
    
   

    if (nrow(df_rho_sub_non_NA > 0)) {
      predict_start_date_for_roll <- df_rho_sub_non_NA$dates[nrow(df_rho_sub_non_NA)] # forecast starts from last date after stripping off NAs
      dates_forecast <- seq(predict_start_date_for_roll, predict_end_date, by="day")
      log_cases_0 <- df_rho_sub_non_NA$log_cases_roll[nrow(df_rho_sub_non_NA)]
      
      
      
      # Min, max incidence across df_rho_sub:
      min_incidence <- exp(min(df_rho_sub_non_NA$log_cases_roll))
      max_incidence <- exp(max(df_rho_sub_non_NA$log_cases_roll))

      # avrg, min and max rho over the prediction window:
      rho_roll_for_forecast <- df_rho_sub_non_NA$rho_roll
      rho_mid <- mean(rho_roll_for_forecast)
      rho_min <- min(rho_roll_for_forecast)
      rho_max <- max(rho_roll_for_forecast)
      rho_SD <- sd(rho_roll_for_forecast)
      
      # Mean and SD of rho across growth periods, and mean rho across decay periods
      rho_roll_positive <- rho_roll_for_forecast %>% subset(rho_roll_for_forecast > 0)
      rho_roll_negative <- rho_roll_for_forecast %>% subset(rho_roll_for_forecast < 0)
      mean_rho_growth <- mean(rho_roll_positive)
      mean_rho_decay <- mean(rho_roll_negative)
      last_rho <- rho_roll_for_forecast[length(rho_roll_for_forecast)]

      
      # mean_rho_growth <- df_rho_sub_non_NA %>% subset(rho_roll > 0) %>% pull(rho_roll) %>% mean
      # mean_rho_decay <- df_rho_sub_non_NA %>% subset(rho_roll < 0) %>% pull(rho_roll) %>% mean
      # SD_rho_growth <- 
      
      
      R_mid <- R0_SEIR(rho_mid, sigma_SEIR, gamma_SEIR)
      R_min <- R0_SEIR(rho_min, sigma_SEIR, gamma_SEIR)
      R_max <- R0_SEIR(rho_max, sigma_SEIR, gamma_SEIR)
      R_last <- R0_SEIR(last_rho, sigma_SEIR, gamma_SEIR)
      
      # The forecasts:
      # log_cases_forecast_mid <- as.numeric(log_cases_0 + rho_mid*(dates_forecast - predict_start_date))
      log_cases_forecast_mid <- as.numeric(log_cases_0 + rho_mid*(dates_forecast - predict_start_date_for_roll))
      log_cases_forecast_min <- as.numeric(log_cases_0 + rho_min*(dates_forecast - predict_start_date_for_roll))
      log_cases_forecast_max <- as.numeric(log_cases_0 + rho_max*(dates_forecast - predict_start_date_for_roll))
      log_cases_SD_lower <- as.numeric(log_cases_0 + (rho_mid - rho_SD)*(dates_forecast - predict_start_date_for_roll))
      log_cases_SD_upper <- as.numeric(log_cases_0 + (rho_mid + rho_SD)*(dates_forecast - predict_start_date_for_roll))
      log_cases_growth <- as.numeric(log_cases_0 + mean_rho_growth*(dates_forecast - predict_start_date_for_roll))
      log_cases_decay <- as.numeric(log_cases_0 + mean_rho_decay*(dates_forecast - predict_start_date_for_roll))
      log_cases_last_rho <- as.numeric(log_cases_0 + last_rho*(dates_forecast - predict_start_date_for_roll))
      
      
      forecast_frame <- data.frame("dates" = dates_forecast,
                                   "log_cases_mid" = log_cases_forecast_mid,
                                   "log_cases_min" = log_cases_forecast_min,
                                   "log_cases_max" = log_cases_forecast_max,
                                   "log_cases_SD_lower" = log_cases_SD_lower,
                                   "log_cases_SD_upper" = log_cases_SD_upper,
                                   "log_cases_growth" = log_cases_growth,
                                   "log_cases_decay" = log_cases_decay,
                                   "log_cases_last_rho" = log_cases_last_rho)
      
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
        "R_last" = R_last,
        "cases_0" = exp(log_cases_0),
        "forecast_start_date" = predict_start_date_for_roll,
        "min_incidence" = min_incidence,
        "max_incidence" = max_incidence)
      } else { # if no valid result (specifically, if df_rho_sub_non_NA has zero rows)
        output_to_return <- list(
          "incidence" = incidence,
          "rolling_values" = df_rho,
          "rolling_values_for_forecast" = df_rho_sub,
          "forecast" = NA,
          "turnover_date" = NA,
          "rho_0" = NA,
          "R0" = NA,
          "doubling_time_0" = NA,
          "rho_mid" = NA,
          "rho_min" = NA,
          "rho_max" = NA,
          "R_mid" = NA,
          "R_min" = NA,
          "R_max" = NA,
          "cases_0" = NA,
          "forecast_start_date" = NA,
          "min_incidence" = NA,
          "max_incidence" = NA)
        
      
    }
    
    return(output_to_return)
    
  })
}

# Return NAs for everything in the event that we can't compute sensible things in 
# rolling_R_and_incidence():

ERROR_rolling_R_and_incidence <- function(incidence) {
  output_to_return <- list(
    "incidence" = incidence,
    "rolling_values" = NA,
    "rolling_values_for_forecast" = NA,
    "forecast" = NA,
    "turnover_date" = NA,
    "rho_0" = NA,
    "R0" = NA,
    "doubling_time_0" = NA,
    "rho_mid" = NA,
    "rho_min" = NA,
    "rho_max" = NA,
    "R_mid" = NA,
    "R_min" = NA,
    "R_max" = NA,
    "cases_0" = NA,
    "forecast_start_date" = NA,
    "min_incidence" = NA,
    "max_incidence" = NA)
  return(output_to_return)
}