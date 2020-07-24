# _v5: adds option to fit post-exponential phase only up to the beginning of the 
#   window for current R and incidence.  Only difference is that it calls find_exponential_portion_v7()
#   rather than find_exponential_portion_v6()
#
# _v7: For use with Shiny (outputs plot object)
 
region_mobility_and_incidence_v7 <- function(inputs) {
  # 
  with(inputs,{
    if (Country.Region=="US" & Province.State != "") {US_TF <- T} else {US_TF <- F}
    if (US_TF) {
      incidence_frame <- jhu_covid_get_region_incidence_US_v3(covid_data,
                                                              covid_death_data,
                                                              start_date,
                                                              Province.State,
                                                              Admin2,
                                                              plot_TrueFalse = F,
                                                              normalize_by_npop = T)
      pop <- incidence_frame$pop[1]
      inputs$pop <- incidence_frame$pop[1]
      
    } else {
      if (Province.State == "") {use_Province.State <- F} else {use_Province.State <- T}
      incidence_frame <- jhu_covid_get_region_incidence_2(covid_data,
                                                          covid_death_data,
                                                          start_date,
                                                          Country.Region,
                                                          # use_Province.State = T,
                                                          use_Province.State, # Leave off while we're still at country level, otherwise e.g. Canada doesn't work
                                                          exclude_Province.State = F,
                                                          Province.State_input = Province.State,
                                                          plot_TrueFalse = F)
    }
    # Date of most recent observation:
    last_date <- incidence_frame$dates[nrow(incidence_frame)]
    incidence_frame_cfr_adj <- incidence_frame
    lagged_CFR_frame <- NA
    lag <- NA
    
    if (!is.na(CFR)) { # If CFR is not specified, don't do any adjusting
      if (!is.finite(cfr_correction_factor)) { # Only comput cfr_correction_factor if it hasn't been specified
        lagged_CFR_frame <- death_lags(incidence_frame,lags_vector,plot_cfr_lags_TF)
        lag <- lagged_CFR_frame$lag[1] # For now, just use the first element of lags_vector as the lag
        
        cfr_observed <- lagged_CFR_frame$cfr[1]
        
        if (is.finite(cfr_observed) & cfr_observed > 0) {
          cfr_correction_factor <- cfr_observed/CFR
          if (cfr_correction_factor < 1) {cfr_correction_factor <- 1} # Can't have fewer than the observed number of cases!
        } else {
          cfr_correction_factor <- 1
        }
        CFR_text <- paste0("CFR: target = ",CFR,", observed = ",round(cfr_observed,3), " using ", lag, "d lag ==> correction factor = ",round(cfr_correction_factor,3),"\n")
      } else {
        CFR_text <- paste0("CFR correction factor externally specified as ", round(cfr_correction_factor,3),"\n")
        lag <- NA
      }
      incidence_frame_cfr_adj$cumu_cases <- as.integer(incidence_frame_cfr_adj$cumu_cases*cfr_correction_factor)
      incidence_frame_cfr_adj$cases <- as.integer(incidence_frame$cases*cfr_correction_factor)
    } else {
      CFR_text <- "CFR correction not applied"
    }
    # input$cfr_correction_factor <- cfr_correction_factor
    
   # turnover_point_data <- find_turnover_point_v2(incidence_frame_cfr_adj,as.Date("2020-03-15"), as.Date("2020-04-15"), as.Date("2020-05-15"))
    # Apply find_exponential_portion_v3f() to the corrected incidence:
    if (incidence_frame_cfr_adj$cumu_cases[nrow(incidence_frame_cfr_adj)] > 0) {
      # df_exp_portion <- find_exponential_portion_v3i(
      df_exp_portion <- find_exponential_portion_v7(
        incidence_list = incidence_frame_cfr_adj,
        population = pop,
        N_days_to_aggregate = 1,
        daily_case_threshold = 1,
        total_case_threshold_for_turnover_calc = total_case_threshold_for_turnover_calc,
        onset_cumu_case_threshold = cases_per_100k_threshold*pop/1e5,
        max_starting_index = 1,
        onset_date_override = onset_date_override,
        turnover_date_override = turnover_date_override,
        interval_type = interval_type,
        window_for_current_R = most_recent_R_window,
        predict_date = predict_date,
        plot_current_R_TF = T,
        post_turnover_up_to_present_TF = post_turnover_up_to_present_TF,
        sigma_SEIR = sigma_SEIR,
        gamma_SEIR = gamma_SEIR,
        plot_TF = F, # NEW: hardwire to not print, since we're not using these plots anymore
        plot_only_linear_TF = plot_only_linear_TF,
        plot_to_screen_TF = plot_to_screen_TF,
        title = paste0(Admin2, " ", Province.State, " ", Country.Region),
        filename = paste0(Country.Region,"_",Province.State,"_",Admin2,"_exponential_fit.",filetype),
        CFR_text = CFR_text, 
        pathname = pathname_figs
      )
      cumu_cases_at_turnover_raw <- incidence_frame$cumu_cases[incidence_frame$dates==df_exp_portion$turnover_date]
      cumu_cases_at_turnover_corrected <- incidence_frame_cfr_adj$cumu_cases[incidence_frame_cfr_adj$dates==df_exp_portion$turnover_date]
      
    } else {
      df_exp_portion <- data.frame(
        "onset_date" = NA,
        "turnover_date" = NA,
        "exponential_portion_duration" = NA,
        "rho" = NA,
        "doubling_time" = NA,
        "R" = NA,
        "rho_post_exp" = NA,
        "doubling_time_post_exp" = NA,
        "R_post_exp" = NA,
        "rho_current" = NA,
        "doubling_time_current" = NA,
        "R_current" = NA,
        "incidence_current" = NA,
        "sigma_SEIR" = sigma_SEIR,
        "gamma_SEIR" = gamma_SEIR
      )
      
      cumu_cases_at_turnover_raw <- NA
      cumu_cases_at_turnover_corrected <- NA
      
    }


    # Get Google mobility info:
    google_mobi <- read_google_mobility_v2(google_mobility,
                                        mobility_country = mobility_country, 
                                        mobility_region = mobility_region, 
                                        mobility_subregion = mobility_subregion,
                                        mobility_window_size = mobility_window_half_width,
                                        alignment = rolling_mean_alignment)

    # New incidence + R + projection plot :
    browser()
    rolling_list <- rolling_R_and_incidence(incidence =  incidence_frame_cfr_adj, 
                                            predict_start_date = as.Date("2020-08-20"), 
                                            predict_end_date =inputs$predict_date, 
                                            onset_date = df_exp_portion$onset_date,
                                            turnover_date = df_exp_portion$turnover_date,
                                            inputs = inputs)
    plot_to_return <- plot_rolling_values(rolling_list = rolling_list, 
                        title = paste0(mobility_subregion, " ", Province.State, " ", Country.Region),
                        CFR_text = CFR_text,
                        inputs = inputs)

    # if (nrow(google_mobi$df) > 0) {
    # Only call analyze_R_and_mobility if there exists any residential data:
    any_residential_data <- max(is.finite(google_mobi$df$residential_percent_change_from_baseline))
    if (any_residential_data > 0 & plot_R_and_mobility_TF & incidence_frame_cfr_adj$cumu_cases[nrow(incidence_frame_cfr_adj)] > 0) {
      analyze_R_and_mobility_v2(incidence_frame, google_mobi, df_exp_portion$R, df_exp_portion$turnover_date, inputs)
    }

    n_days <- nrow(incidence_frame)
    output_to_return <- list("last_date" = last_date,
                                   "df_exp_portion"=df_exp_portion,
                                   # "onset_date" = df_exp_portion$onset_date, # BEGIN NEW
                                   # "turnover_date" = df_exp_portion$turnover_date,
                                   # "exponential_portion_duration" = df_exp_portion$exponential_portion_duration,
                                   # "rho" = rolling_list$rho_0,
                                   # "doubling_time" = rolling_list$doubling_time_0,
                                   # "R" = rolling_list$R0,
                                   # "R_" = rolling_list$
                                   "rho_0_rolling" = rolling_list$rho_0,
                                   "R0" = rolling_list$R0,
                                   "rho_mid" = rolling_list$rho_mid,
                                   "rho_min" = rolling_list$rho_min,
                                   "rho_max" = rolling_list$rho_max,
                                   "R_mid" = rolling_list$R_mid,
                                   "R_min" = rolling_list$R_min,
                                   "R_max" = rolling_list$R_max,
                                   "incidence_current" = rolling_list$cases_0,
                                   "CFR_applied" = CFR,
                                   "cfr_observed" = cfr_observed,
                                   "cfr_correction_factor" = cfr_correction_factor,
                                   "cfr_lag" = lag,
                                   "cumu_cases_raw" = incidence_frame$cumu_cases[n_days],
                                   "cumu_cases_corrected" = incidence_frame_cfr_adj$cumu_cases[n_days],
                                   "cumu_deaths" = incidence_frame$cumu_deaths[n_days],
                                   "cumu_cases_per_100k_raw" = incidence_frame$cumu_cases[n_days]/pop*1e5,
                                   "cumu_cases_per_100k_corrected" = incidence_frame_cfr_adj$cumu_cases[n_days]/pop*1e5,
                                   "cumu_deaths_per_100k" = incidence_frame$cumu_deaths[n_days]/pop*1e5, 
                                   "cumu_cases_at_turnover_raw" = cumu_cases_at_turnover_raw,
                                   "cumu_cases_at_turnover_corrected" = cumu_cases_at_turnover_corrected,
                                   "cumu_cases_at_turnover_per_100k_raw" = cumu_cases_at_turnover_raw/pop*1e5,
                                   "cumu_cases_at_turnover_per_100k_corrected" = cumu_cases_at_turnover_corrected/pop*1e5,
                                   "residential_mobi_midpoint" = google_mobi$date_workplaces_mid,
                                   "residential_min" = google_mobi$min_of_residential,
                                   "residential_max" = google_mobi$max_of_residential,
                                   "window_for_current_R" = most_recent_R_window,
                             "plot" = plot_to_return
                             
                                   )
    return(output_to_return)
# [LEFT OFF HERE; read_google_mobility returns empty frame]
    # incidence_frame_cfr_adj$cumu_
  })
}