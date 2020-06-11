
modify_county_onset_turnover <- function(frame_to_modify, FIPS_to_modify, onset_date_OVERRIDE, turnover_date_OVERRIDE, inputs) {
  with(inputs, {
  
    
    covid19_US_line <- covid19_US %>% subset(FIPS==FIPS_to_modify)
    covid19_deaths_US_line <- covid19_deaths_US %>% subset(FIPS==FIPS_to_modify)
    
    # Make 1-line override "table" to give us the overrides we want:
    fips <- covid19_US_line$FIPS
    counties <- covid19_US_line$Admin2
    states <- covid19_US_line$Province_State
    n_rows <- 1
    override_table <- data.frame("FIPS"=fips, 
                                 "counties"=counties, 
                                 "states" = states,
                                 "onset_date_override" = onset_date_OVERRIDE,
                                 "turnover_date_override" = turnover_date_OVERRIDE)
    browser()
    
    # empty_date_vector <- rep(NA,n_rows); class(empty_date_vector) <- "Date"
    # override_table <- data.frame("FIPS" = fips, "counties" = counties, "states"=states,onset_date_override=empty_date_vector, turnover_date_override =empty_date_vector)
    # 
    # override_table <- 
    
    modified_row <- exponential_growth_and_mobility_US_v5(
      covid19_US_line,
      covid19_deaths_US_line,
      # apple_mobility,
      google_mobility,
      mobility_window_half_width,
      start_date,
      N_days_to_aggregate,
      daily_case_threshold_frac,
      cumu_case_threshold,
      max_onset_delay,
      override_table,
      interval_type,
      window_for_current_R,
      predict_date,
      sigma_SEIR,
      gamma_SEIR,
      total_case_threshold,
      plot_TF, 
      pause_TF, 
      pathname,
      fips_and_land_area_table)
    
    ind <- which(frame_to_modify$FIPS == FIPS_to_modify)
    frame_to_modify[ind,] <- modified_row
    
  })

 return(frame_to_modify)               
}