
jhu_covid_get_region_incidence_2 <- function(covid_data, 
         covid_death_data,
         start_date,
         Country.Region_input, 
         use_Province.State, # T or F, depending on whether or not we want to impose a condition on Province.State
         exclude_Province.State,
         Province.State_input,
         plot_TrueFalse) {
  
  # dates:
  n_days <- ncol(covid_data) - 4
  dates <- seq.Date(from = as.Date(start_date), length.out = n_days,by="day")
  region_cases_raw <- subset(covid_data,
                             Country.Region == Country.Region_input)
  region_deaths_raw <- subset(covid_death_data,
                              covid_death_data$Country.Region == Country.Region_input)
  if (use_Province.State) {
    if (exclude_Province.State) {
      region_cases_raw <- subset(region_cases_raw, Province.State == !Province.State_input)
      region_deaths_raw <- subset(region_deaths_raw,
                                  Province.State == !Province.State_input)
    } else {
      region_cases_raw <- subset(region_cases_raw, Province.State == Province.State_input)
      region_deaths_raw <- subset(region_deaths_raw,
                                  Province.State == Province.State_input)
    }
  }

  # region_deaths_raw <- subset(covid_death_data,
  #                             covid_death_data$Country.Region == Country.Region_input)
  # if (use_Province.State) {
  #   if (exclude_Province.State{
  #     
  #   }
  #   region_deaths_raw <- subset(region_deaths_raw, covid_death_data$Province.State == Province.State_input)
  # }

  cumu_cases <- as.numeric(colSums(region_cases_raw[,5:ncol(covid_data)]))
  cumu_deaths <- as.numeric(colSums(region_deaths_raw[,5:ncol(covid_data)]))
  
  cases <- c(0,diff(cumu_cases))
  deaths <- c(0,diff(cumu_deaths))
  
  # result <- list(
  #   "dates" = dates,
  #   "cumu_cases" = cumu_cases,
  #   "cumu_deaths" = cumu_deaths,
  #   "cases" = cases,
  #   "deaths" = deaths
  # )
  # result <- data.frame(
  #   "dates" = as.numeric(dates),
  #   "cumu_cases" = as.numeric(cumu_cases),
  #   "cumu_deaths" = as.numeric(cumu_deaths),
  #   "cases" = as.numeric(cases),
  #   "deaths" = as.numeric(deaths)
  # )
  result <- data.frame(
    "dates" = dates,
    "cumu_cases" = cumu_cases,
    "cumu_deaths" = cumu_deaths,
    "cases" = cases,
    "deaths" = deaths
  )

  if (plot_TrueFalse) {
    dev.off
    par(mfrow=c(1,1))
    plot(dates,cumu_cases,main = paste(Country.Region_input,Province.State_input,"cumulative cases"))
    plot(dates,cumu_deaths,main = paste(Country.Region_input,Province.State_input,"cumulative deaths"))
    plot(dates,cases,main=paste(Country.Region_input,Province.State_input,"daily cases"))
    plot(dates,cases,main=paste(Country.Region_input,Province.State_input,"log daily cases"),log="y")
    plot(dates,deaths,main=paste(Country.Region_input,Province.State_input,"daily deaths"))
    plot(dates,deaths,main=paste(Country.Region_input,Province.State_input,"log daily deaths"),log="y")
  }
  
  return(result)
  #   
  #   # cumu_cases_US <- colSums(covid19_US[,5:ncol(covid19)])
  #   # cases_US <- c(0,diff(cumu_cases_US))
  #   # 
  #   # # cases_US_padded <- c(cases_US, rep(0,number_to_pad))
  #   # 
  #   # cumu_deaths_US <- colSums(covid19_US_deaths[,5:ncol(covid19)])
  #   # deaths_US <- c(0,diff(cumu_deaths_US))
  #   # browser()
}