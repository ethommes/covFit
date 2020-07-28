
US_county_mobility_and_incidence <- function(input, US_states_frame) {
  with(input, {
    frame <- data.frame(
      "states" = covid_data$Province_State,
      "counties" = covid_data$Admin2,
      "FIPS" = covid_data$FIPS,
      "pop" = covid_death_data$Population
      # "area_sq_miles" = NA,
      # "density_sq_miles" = NA
    )
    # Remove entries with no population given:
    frame <- frame %>% subset(is.finite(pop) & pop > 0)
    
    # This data file contains also the county name, allowing us to add this as a column [comment out if you want to read from local file]
    # fips_raw <-
    #   read_csv(
    #     "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
    #     col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp")
    #   )
    # 
    # Alternatively, if you already have it, read from local file, uncomment below and comment out above:
    # fips_raw <- read_csv("../../US_Census_data/national_county.txt",col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp"))
    
    # fips <- data.frame("FIPS"=as.numeric(paste(fips_raw$statefp,fips_raw$countyfp,sep="")),"county_full_name" = fips_raw$county.name, "state_abbr" = fips_raw$state.abb)
    frame <- left_join(frame, fips, by="FIPS")
    
    # Remove the unincorporated territories:
    frame <- frame[6:nrow(frame),]
    
    override_table_sub <- override_table %>% select(FIPS, onset_date_override, turnover_date_override)
    frame_TEMP <- left_join(frame, override_table_sub, by="FIPS")
    
    # # Manually add abbreviations for unicorporated territories
    # frame$state_abbr[1] <- "AS"
    # frame$state_abbr[2] <- "GU"
    # frame$state_abbr[3] <- "MP"
    # frame$state_abbr[4] <- "PR"
    # frame$state_abbr[5] <- "VI"
    
    

    n_rows <- nrow(frame)
    empty_date_vector <- rep(NA,n_rows); class(empty_date_vector) <- "Date"
    
    # set up (part of) master data frame:
    df <- data.frame(
      frame,
      "last_date" = empty_date_vector,
      "CFR_applied" = rep(NA,n_rows),
      "cfr_observed" = rep(NA,n_rows),
      "cfr_correction_factor" = rep(NA,n_rows),
      "cfr_lag" = rep(NA,n_rows),
      "cumu_cases_raw" = rep(NA,n_rows),
      "cumu_cases_corrected"= rep(NA,n_rows),
      "cumu_cases_per_100k_raw" = rep(NA,n_rows),
      "cumu_cases_per_100k_corrected" = rep(NA,n_rows),
      "cumu_deaths" = rep(NA,n_rows),
      "cumu_deaths_per_100k" = rep(NA,n_rows),
      # "pop_density_per_sq_mi" = rep(NA,n_rows),
      "onset_date" = empty_date_vector,
      "onset_date_OVERRIDE" = frame_TEMP$onset_date_override,
      "turnover_date"=empty_date_vector,
      "turnover_date_OVERRIDE" = frame_TEMP$turnover_date_override,
      "cumu_cases_at_turnover_raw" = rep(NA,n_rows),
      "cumu_cases_at_turnover_corrected" = rep(NA,n_rows),
      "cumu_cases_at_turnover_per_100k_raw" = rep(NA,n_rows),
      "cumu_cases_at_turnover_per_100k_corrected" = rep(NA,n_rows),
      "rho"=rep(NA,n_rows),
      "doubling_time"=rep(NA,n_rows),
      "R" = rep(NA,n_rows),
      "rho_post_exp" = rep(NA,n_rows),
      "doubling_time_post_exp" = rep(NA,n_rows),
      "R_post_exp" = rep(NA,n_rows),
      "window_for_current" = rep(NA,n_rows),
      "rho_current" = rep(NA,n_rows),
      "doubling_time_current" = rep(NA,n_rows),
      "R_current" = rep(NA,n_rows),
      "incidence_current" = rep(NA,n_rows),
      # "recreation_mobi_midpoint_date" = empty_date_vector,
      # "grocery_mobi_midpoint_date" = empty_date_vector,
      # "parks_mobi_midpoint_date" = empty_date_vector,
      # "transit_mobi_midpoint_date" = empty_date_vector,
      # "workplaces_mobi_midpoint_date" = empty_date_vector,
      "residential_mobi_midpoint_date" = empty_date_vector,
      "residential_mobi_min" = rep(NA,n_rows),
      "residential_mobi_max" = rep(NA,n_rows),
      "mean_mobi_midpoint_date" = empty_date_vector,
      "mean_mobi_min" = rep(NA,n_rows)
    )
    # To each county, assign the CFR correction factor we previously found for its state:
    cfr_frame <- data.frame("state_abbr"=US_states_frame$state_abbr, "cfr_correction_factor"=US_states_frame$cfr_correction_factor)
    df_states_of_counties <- data.frame("state_abbr"=df$state_abbr)
    df_cfr_factors_for_counties <- left_join(df_states_of_counties, cfr_frame, by="state_abbr")
    df$cfr_correction_factor <- df_cfr_factors_for_counties$cfr_correction_factor
    
    # Join the land area column from fips_and_land_area_table to df:
    fips_pop_area_density <- data.frame(
      "FIPS" = fips_and_land_area_table$fips,
      "pop_2010" = fips_and_land_area_table$POP010210,
      "pop_density_per_sq_mi" = fips_and_land_area_table$POP060210
    )
    df <- left_join(df,fips_pop_area_density,by="FIPS")
    
    # Give option of over-riding range of counties to do:
    if (is.na(first_county_index)) {n_start <- 1} else {n_start <- first_county_index}
    if (is.na(last_county_index)) {n_end <- n_rows} else {n_end <- last_county_index}
        
    for (i in n_start:n_end) {
    # for (i in 67:n_rows) {
      cat("\f", i,  "Doing ", df$counties[i], df$states[i])
      flush.console()
      input$onset_date_override <- df$onset_date_OVERRIDE[i]
      input$turnover_date_override <- df$turnover_date_OVERRIDE[i]
      input$Country.Region <- "US"
      input$Province.State <- df$states[i]
      input$Admin2 <- df$counties[i]
      input$mobility_country = "United States"
      input$mobility_region = df$states[i]
      input$mobility_subregion = df$county_full_name[i]
      input$lags_vector <- c(10,0)
      input$cfr_correction_factor <- df$cfr_correction_factor[i]
      
      state_output <- region_mobility_and_incidence_v6(input)
      df$last_date[i] <- state_output$last_date
      df$onset_date[i] <- state_output$onset_date
      df$turnover_date[i] <- state_output$turnover_date
      df$cumu_cases_raw[i] <- state_output$cumu_cases_raw
      df$cumu_cases_corrected[i] <- state_output$cumu_cases_corrected
      df$cumu_cases_per_100k_raw[i] <- state_output$cumu_cases_per_100k_raw
      df$cumu_cases_per_100k_corrected[i] <- state_output$cumu_cases_per_100k_corrected
      df$cumu_deaths[i] <- state_output$cumu_deaths
      df$cumu_deaths_per_100k[i] <- state_output$cumu_deaths_per_100k
      df$CFR_applied[i] <- state_output$CFR_applied
      df$cfr_observed[i] <- state_output$cfr_observed
      df$cfr_lag[i] <- state_output$cfr_lag
      df$cfr_correction_factor[i] <- state_output$cfr_correction_factor
      df$cumu_cases_at_turnover_raw[i] <- state_output$cumu_cases_at_turnover_raw
      df$cumu_cases_at_turnover_corrected[i] <- state_output$cumu_cases_at_turnover_corrected
      df$cumu_cases_at_turnover_per_100k_raw[i] <- state_output$cumu_cases_at_turnover_per_100k_raw
      df$cumu_cases_at_turnover_per_100k_corrected[i] <- state_output$cumu_cases_at_turnover_per_100k_corrected
      df$rho[i] <- state_output$rho
      df$doubling_time[i] <- state_output$doubling_time
      df$R[i] <- state_output$R
      df$rho_post_exp[i] <- state_output$rho_post_exp
      df$doubling_time_post_exp[i] <- state_output$doubling_time_post_exp
      df$R_post_exp[i] <- state_output$R_post_exp
      df$rho_current[i] <- state_output$rho_current
      df$doubling_time_current[i] <- state_output$doubling_time_current
      df$R_current[i] <- state_output$R_current
      df$incidence_current[i] <- state_output$incidence_current
      df$residential_mobi_max[i] <- state_output$residential_max
      df$residential_mobi_min[i] <- state_output$residential_min
      df$residential_mobi_midpoint_date[i] <- state_output$residential_mobi_midpoint
      df$window_for_current[i] <- state_output$window_for_current_R
      
    }
    return(df)
  })
  
}