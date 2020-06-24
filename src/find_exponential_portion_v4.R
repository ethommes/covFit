# _v3e adds calculation of latest R_eff, over the last N days, where N can be specified.
# _v3f: 
#   - outputs also onset date
#   - allows selecting whether or not to plot current R 
# _v3h: extrapolates also the total post-turnover fit (blue) to predict_date

find_exponential_portion_v4 <- function(incidence_list,
                                         population,
                                         CFR, # NA to not perform correction
                                     N_days_to_aggregate, 
                                     daily_case_threshold,
                                     total_case_threshold_for_turnover_calc,
                                     onset_cumu_case_threshold,
                                     max_starting_index,
                                     onset_date_override, # set to NA for no override
                                     turnover_date_override, # set to NA for no override
                                     interval_type,
                                     window_for_current_R,
                                     predict_date, # date which to extrapolate current R portion
                                     plot_current_R_TF, # Do we plot (in green) the fit for the current R?
                                     sigma_SEIR,
                                     gamma_SEIR, 
                                     plot_TF,
                                     title,
                                     filename,
                                     CFR_text,
                                     # county,
                                     # state,
                                     pathname) { 
  
  # Total number of cases and most recent date: 
  cases_total <- incidence_list$cumu_cases[nrow(incidence_list)]
  latest_date <- incidence_list$dates[nrow(incidence_list)]
  
  # Get rid of dates before cumulative case threshold is reached:
  onset_index <- min(which(incidence_list$cumu_cases >= onset_cumu_case_threshold))
  onset_date <- incidence_list$date[onset_index]
  incidence_list_cropped <- incidence_list[onset_index:nrow(incidence_list),]
  
  dates <- incidence_list_cropped$dates
  cases <- incidence_list_cropped$cases

  df_cases <- data.frame("dates"=dates,"cases"=cases)
  df_cases <- df_cases %>% subset(cases > 0) # get rid of days with zero cases
  
  fit <- lm(cases~dates, data=df_cases)
  # seg_fit <- segmented(fit, seg.Z = ~dates, psi=c(df_cases$dates[2]), control = seg.control(display = FALSE))
  seg_fit <- segmented(fit, seg.Z = ~dates, control = seg.control(display = FALSE))
  
  browser()
  
  
  
  return()
  
}