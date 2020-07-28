
world_mobility_and_incidence <- function(countries_frame, input) {
  with(input, {
    data("world_bank_pop")
    
    countries_vector <- countries_frame$country_names_JHU
    countries_vector_mobility <- countries_frame$country_names_mobility
    correction_factor <- as.numeric(countries_frame$correction_factor)
    n_rows <- length(countries_vector)
    empty_date_vector <- rep(NA,n_rows); class(empty_date_vector) <- "Date"
    
    # set up (part of) master data frame:
    df <- data.frame(
      "last_date" = empty_date_vector,
      "countries" = countries_vector,
      "pop" = rep(NA,n_rows),
      "CFR_applied" = rep(NA,n_rows),
      "cfr_observed" = rep(NA,n_rows),
      "cfr_correction_factor" = correction_factor,
      "cfr_lag" = rep(NA,n_rows),
      "cumu_cases_raw" = rep(NA,n_rows),
      "cumu_cases_corrected"= rep(NA,n_rows),
      "cumu_cases_per_100k_raw" = rep(NA,n_rows),
      "cumu_cases_per_100k_corrected" = rep(NA,n_rows),
      "cumu_deaths" = rep(NA,n_rows),
      "cumu_deaths_per_100k" = rep(NA,n_rows),
      "pop_density_per_sq_mi" = rep(NA,n_rows),
      "onset_date" = empty_date_vector,
      "onset_date_OVERRIDE" = override_table$onset_date_override,
      "turnover_date"=empty_date_vector,
      "turnover_date_OVERRIDE" = override_table$turnover_date_override,
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
      "window_for_current" = rep(NA, n_rows),
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
    # Loop over countries:
    for (i in 1:length(countries_vector)) {
      # for (i in 49:50) {
      country <- countries_vector[i]
      if (is.na(countries_vector_mobility[i])) {
        country_mobility <- country
      } else {
        country_mobility <- countries_vector_mobility[i]
      }
      cat("\f", i,  "Doing ", country)
      flush.console()
      # Take subset of covid_data frame which includes only the counties within the current state, then left-join it 
      # to fips_and_land_area_table 
      covid_data_subset <- subset(covid_data, Country.Region == country)
      covid_death_data_subset <- subset(covid_death_data, Country.Region == country)
      
      # Get population from World Bank data:  
      country_code <- countrycode(country, origin="country.name", destination="wb")
      pop_line <- world_bank_pop %>% subset(country==country_code) %>% subset(indicator=="SP.POP.TOTL")
      if (nrow(pop_line) == 0) {
        browser()
      }
      pop_country <- pop_line$`2017`
      
      land_area <- NA # Need to implement
      pop_density <- NA # Need to implement
      df$pop[i] <- pop_country
      df$pop_density_per_sq_mi[i] <- pop_density
      
      input$onset_date_override <- df$onset_date_OVERRIDE[i]
      input$turnover_date_override <- df$turnover_date_OVERRIDE[i]
      input$Country.Region <- country
      input$Province.State <- ""
      input$Admin2 = ""
      input$mobility_country = country_mobility # CAUTION: unless you specified correct override name, will not work if country is named differently in mobility data (e.g. "South Korea" vs. "Korea, South")
      input$mobility_region = ""
      input$mobility_subregion = ""
      input$lags_vector <- c(10,0) # PLACEHOLDER
      input$pop <- pop_country
      input$cfr_correction_factor <- df$cfr_correction_factor[i]
      
      country_output <- region_mobility_and_incidence_v5(input)
      
      df$last_date[i] <- country_output$last_date
      df$onset_date[i] <- country_output$onset_date
      df$turnover_date[i] <- country_output$turnover_date
      df$cumu_cases_raw[i] <- country_output$cumu_cases_raw
      df$cumu_cases_corrected[i] <- country_output$cumu_cases_corrected
      df$cumu_cases_per_100k_raw[i] <- country_output$cumu_cases_per_100k_raw
      df$cumu_cases_per_100k_corrected[i] <- country_output$cumu_cases_per_100k_corrected
      df$cumu_deaths[i] <- country_output$cumu_deaths
      df$cumu_deaths_per_100k[i] <- country_output$cumu_deaths_per_100k
      df$CFR_applied[i] <- country_output$CFR_applied
      df$cfr_observed[i] <- country_output$cfr_observed
      df$cfr_lag[i] <- country_output$cfr_lag
      df$cfr_correction_factor[i] <- country_output$cfr_correction_factor
      df$cumu_cases_at_turnover_raw[i] <- country_output$cumu_cases_at_turnover_raw
      df$cumu_cases_at_turnover_corrected[i] <- country_output$cumu_cases_at_turnover_corrected
      df$cumu_cases_at_turnover_per_100k_raw[i] <- country_output$cumu_cases_at_turnover_per_100k_raw
      df$cumu_cases_at_turnover_per_100k_corrected[i] <- country_output$cumu_cases_at_turnover_per_100k_corrected
      df$rho[i] <- country_output$rho
      df$doubling_time[i] <- country_output$doubling_time
      df$R[i] <- country_output$R
      df$rho_post_exp[i] <- country_output$rho_post_exp
      df$doubling_time_post_exp[i] <- country_output$doubling_time_post_exp
      df$R_post_exp[i] <- country_output$R_post_exp
      df$window_for_current[i] <- country_output$window_for_current_R
      df$rho_current[i] <- country_output$rho_current
      df$doubling_time_current[i] <- country_output$doubling_time_current
      df$R_current[i] <- country_output$R_current
      df$incidence_current[i] <- country_output$incidence_current
      df$residential_mobi_max[i] <- country_output$residential_max
      df$residential_mobi_min[i] <- country_output$residential_min
      df$residential_mobi_midpoint_date[i] <- country_output$residential_mobi_midpoint
    }
    return(df)
    
  })
}