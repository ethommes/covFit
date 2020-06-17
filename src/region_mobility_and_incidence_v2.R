
region_mobility_and_incidence_v2 <- function(inputs) {
  with(inputs,{
    if (Country.Region=="US") {US_TF <- T} else {US_TF <- F}
    if (US_TF) {
      incidence_frame <- jhu_covid_get_region_incidence_US_v3(covid_data,
                                                              covid_death_data,
                                                              start_date,
                                                              Province.State,
                                                              Admin2,
                                                              plot_TrueFalse = F,
                                                              normalize_by_npop = T)
      pop <- incidence_frame$pop[1]
      
    } else {
      incidence_frame <- jhu_covid_get_region_incidence_2(covid_data,
                                                          covid_death_data,
                                                          start_date,
                                                          Country.Region,
                                                          use_Province.State = T,
                                                          exclude_Province.State = F,
                                                          Province.State_input = Province.State,
                                                          plot_TrueFalse = F)
    }
    # Compute instantaneous effective R
    # R_eff <- compute_smoothed_R(incidence_frame, R_window_size)
    
    # Compute cfr = [cumulative deaths]/[cumulative cases] at different lags of the cumulative cases.
    # NOTE: the first element of lags_vector is the lag used to compute the overall cfr.
    # if (exists("lags_vector")) {
    
    # If CFR is not NA, apply CFR correction. Otherwise do not.
    # If cfr_observed is NA, compute cfr_observed for the current region and apply 
    # the CFR correction relative to this. If cfr_observed is specified (say, if we calculated 
    # it at the state level and now want to apply that at a county), then we use that instead.
    
    incidence_frame_cfr_adj <- incidence_frame
    if (!is.na(CFR)) {
      if (!is.finite(cfr_observed)) {
        lagged_CFR_frame <- death_lags(incidence_frame,lags_vector,plot_cfr_lags_TF)
        cfr_observed <- lagged_CFR_frame$cfr[1]
        lag <- lagged_CFR_frame$lag[1]
      }
      cfr_correction_factor <- cfr_observed/CFR
      if (cfr_correction_factor < 1) {cfr_correction_factor <- 1} # Can't have few than observed cases!
      incidence_frame_cfr_adj$cumu_cases <- as.integer(incidence_frame_cfr_adj$cumu_cases*cfr_correction_factor)
      incidence_frame_cfr_adj$cases <- as.integer(incidence_frame$cases*cfr_correction_factor)
      CFR_text <- paste0("CFR: target = ",CFR,", observed = ",round(cfr_observed,3), " using ", lag, "d lag ==> correction factor = ",round(cfr_correction_factor,3),"\n")
      
    }
    
    
  
    #     if (!is.finite(cfr_observed)) 
    # lagged_CFR_frame <- death_lags(incidence_frame,lags_vector,plot_cfr_lags_TF)
    # # }
    # cfr_observed <- lagged_CFR_frame$cfr[1]
    # lag <- lagged_CFR_frame$lag[1]
    # 
    # # Apply CFR correction to observed incidence:
    # incidence_frame_cfr_adj <- incidence_frame
    # if (!is.na(CFR)) {
    #   cfr_correction_factor <- cfr_observed/CFR
    #   incidence_frame_cfr_adj$cumu_cases <- as.integer(incidence_frame_cfr_adj$cumu_cases*cfr_correction_factor)
    #   incidence_frame_cfr_adj$cases <- as.integer(incidence_frame$cases*cfr_correction_factor)
    #   CFR_text <- paste0("CFR: target = ",CFR,", observed = ",round(cfr_observed,3), " using ", lag, "d lag ==> correction factor = ",round(cfr_correction_factor,3),"\n")
    # }
    
    # Apply find_exponential_portion_v3f() to the corrected incidence:
    df_exp_portion <- find_exponential_portion_v3g(
      incidence_list = incidence_frame_cfr_adj,
      population = pop,
      N_days_to_aggregate = 1,
      daily_case_threshold = 1,
      cumu_case_threshold = cases_per_100k_threshold*pop/1e5,
      max_starting_index = 1,
      onset_date_override = onset_date_override,
      turnover_date_override = turnover_date_override,
      interval_type = interval_type,
      window_for_current_R = most_recent_R_window,
      predict_date = predict_date,
      plot_current_R_TF = T,
      sigma_SEIR = sigma_SEIR,
      gamma_SEIR = gamma_SEIR,
      plot_TF = F,
      title = paste0(Admin2, " ", Province.State, " ", Country.Region),
      CFR_text = CFR_text, 
      pathname = pathname_figs
    )
    
    
    # Get Google mobility info:
    google_mobi <- read_google_mobility(google_mobility,
                                        mobility_country = mobility_country, 
                                        mobility_region = mobility_region, 
                                        mobility_subregion = mobility_subregion,
                                        mobility_window_half_width = mobility_window_half_width)

    cumu_cases_at_turnover_raw <- incidence_frame$cumu_cases[incidence_frame$dates==df_exp_portion$turnover_date]
    cumu_cases_at_turnover_corrected <- incidence_frame_cfr_adj$cumu_cases[incidence_frame_cfr_adj$dates==df_exp_portion$turnover_date]
    n_days <- nrow(incidence_frame)
    output_to_return <- data.frame(df_exp_portion,
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
                                   "cumu_cases_at_turnover_per_100k_corrected" = cumu_cases_at_turnover_corrected/pop*1e5
                                   
                                   )
    return(output_to_return)
# [LEFT OFF HERE; read_google_mobility returns empty frame]
    # incidence_frame_cfr_adj$cumu_
  })
}