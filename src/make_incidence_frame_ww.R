
make_incidence_frame_ww <- function(df, inp) {
  browser()
  region_data <- df %>% subset(Country_Region == inp$Country_Region & 
                                 Province_State == inp$Province_State &
                                 Admin2 == inp$Admin2)
  incidence_frame <- data.frame(
    "dates" = region_data$Date, 
    "cumu_cases" = region_data$Confirmed,
    "cumu_deaths" = region_data$Deaths,
    "cases" = c(0, diff(region_data$Confirmed)),
    "deaths" = c(0, diff(region_data$Deaths))
  )
  inp$incidence_frame <- incidence_frame
  
  # Fill in other inputs:
  inp$pop <- region_data$Population[1]
  return(inp)
}