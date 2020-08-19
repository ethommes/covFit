
make_incidence_frame_ww <- function(df, inp) {
  # 1) Filter to country:
  region_data <- df %>% subset(Country_Region == inp$Country_Region)
  
  # 2) Filter to province/state.  
  if (inp$Province_State != "ALL") {
    region_data <- region_data %>% subset(Province_State == inp$Province_State)
  }
  
  # 3) Filter to county (US only)
  if (inp$Admin2 != "ALL") {
    region_data <- region_data %>% subset(Admin2 == inp$Admin2)
  }

  # region_data <- df %>% subset(Country_Region == inp$Country_Region & 
  #                                Province_State == inp$Province_State &
  #                                Admin2 == inp$Admin2)
  
  # 4) Filter to trial site, if the option is selected:
  if(inp$filter_to_trial_site_YN) {
    region_data <- region_data %>% subset(`Site name` == inp$trial_site)  
    # Set country, region and subregion to the appropriate values.  Since we're looking only at 
    # a single trial site, these entries are the same all the way through and we can just
    # take the first one:
    inp$Country_Region <- region_data$`Site country`[1]
    inp$Province_State <- region_data$`Site State`[1]
    inp$Admin2 <- region_data$county[1]
  }
  
  # Build incidence_frame, turning any NAs to zeros...
  tmp1 <- data.frame(
    "dates" = region_data$Date, 
    "cumu_cases" = region_data$Confirmed,
    "cumu_deaths" = region_data$Deaths,
    "population" = region_data$Population
  )
  tmp1[is.na(tmp1)] <- 0
  
  # ...if we combined multiple regions--say, all Canadian provinces--we will have multiple entries
  # per date.  What we want is the sum across regions for each date.  Calculate this...
  tmp1 <- tmp1 %>% group_by(dates) %>% summarise_all(sum)
  
  incidence_frame <- data.frame(
    tmp1[,1:3], "cases" = c(0, diff(tmp1$cumu_cases)), 
    "deaths" = c(0, diff(tmp1$cumu_deaths))
    )
  inp$incidence_frame <- incidence_frame

  # Fill in other inputs.  NOTE: Take population from most recent date, since
  # with different reporting formats at earlier times, our population back-calculation 
  # does not necessarily work
  # inp$pop <- region_data$Population[1]
  inp$pop <- tmp1$population[nrow(tmp1)]
  return(inp)
}