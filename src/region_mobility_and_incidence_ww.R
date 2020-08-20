
# "Worldwide" region, which uses the JHU daily snapshots which combine all worldwide regions 
# (incl US counties) into one
region_mobility_and_incidence_ww <- function(inputs) {
  with(inputs,{
    incidence_frame <- inputs$incidence_frame
    
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
      cfr_correction_factor <- 1
    }
    inputs$cfr_correction_factor <- cfr_correction_factor
    
    if (plot_R_and_mobility_TF) {
      google_mobi <- read_google_mobility_v2(google_mobility,
                                             mobility_country = mobility_country, 
                                             mobility_region = mobility_region, 
                                             mobility_subregion = mobility_subregion,
                                             mobility_window_size = mobility_window_half_width,
                                             alignment = rolling_mean_alignment)
    } else {
      google_mobi <- data.frame("residential_mobi_midpoint"=NA,
                                "residential_min"=NA,
                                "residential_max"=NA)
    }
    
    
    # New incidence + R + projection plot :
    rolling_list <- rolling_R_and_incidence(incidence =  incidence_frame_cfr_adj, 
                                            inputs = inputs)
    cumu_cases_at_turnover_raw <- incidence_frame$cumu_cases[incidence_frame$dates==rolling_list$turnover_date]
    cumu_cases_at_turnover_corrected <- incidence_frame_cfr_adj$cumu_cases[incidence_frame_cfr_adj$dates==rolling_list$turnover_date]
    
    if (plot_TF | plot_to_screen_TF) {
      # Construct the plot title:
      if (!filter_to_trial_site_YN) {
        # If we're NOT restricting to only trial sites:
        if (Admin2 == "ALL") {
          title_subregion <- ""
        } else {
          title_subregion <- Admin2
        }
        if (Province_State == "ALL") {
          title_region <- ""
        } else {
          title_region <- Province_State
        }
        title <- paste(title_subregion, title_region, Country_Region)
      } else {
        # If we ARE restricting to only trial sites:
        title <- paste0("SITE: ", trial_site, " --- LOCATION: ", Admin2, " ", Province_State, " ", Country_Region)
        # title <- paste0(trial_site, ", ", " ", Admin2, " ", Province_State, " ", Country_Region)
      }
      
      
      
      plot_to_return <- plot_rolling_values(rolling_list = rolling_list, 
                                            # title = paste0(Admin2, " ", Province_State, " ", Country_Region),
                                            title = title,
                                            CFR_text = CFR_text,
                                            inputs = inputs)
    } else {
      plot_to_return <- plot.new()
    }
    
    
    
    # Only call analyze_R_and_mobility if there exists any residential data:
    if (plot_R_and_mobility_TF) {
      any_residential_data <- max(is.finite(google_mobi$df$residential_percent_change_from_baseline))
      if (any_residential_data > 0 & incidence_frame_cfr_adj$cumu_cases[nrow(incidence_frame_cfr_adj)] > 0) {
        analyze_R_and_mobility_v2(incidence_frame, google_mobi, rolling_list$R_max, rolling_list$turnover_date, inputs)
      }
    }
    
    
    n_days <- nrow(incidence_frame)
    output_to_return <- list("last_date" = last_date,
                             # "df_exp_portion"=df_exp_portion,
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
  })
}