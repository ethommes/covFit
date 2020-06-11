
jhu_covid_get_region_incidence_US_v2 <- function(covid_data, 
         covid_death_data,
         start_date,
         Province_State_input, 
         use_Admin2, # T or F, depending on whether or not we want to impose a condition on Province.State
         Admin2_input,
         plot_TrueFalse,
         npop,
         normalize_by_npop) { 
  # dates:
  n_days <- ncol(covid_data) - 11
  dates <- seq.Date(from = as.Date(start_date), length.out = n_days,by="day")
  region_cases_raw <- subset(covid_data,
                             Province_State == as.character(Province_State_input))
  region_deaths_raw <- subset(covid_death_data,
                              covid_death_data$Province_State == as.character(Province_State_input))
  if (use_Admin2) {
    region_cases_raw <- subset(region_cases_raw, Admin2 == as.character(Admin2_input))
    region_deaths_raw <- subset(covid_death_data,
                                  covid_death_data$Admin2 == as.character(Admin2_input))
  }

  # region_deaths_raw <- subset(covid_death_data,
  #                             covid_death_data$Country.Region == Country.Region_input)
  # if (use_Province.State) {
  #   if (exclude_Province.State{
  #     
  #   }
  #   region_deaths_raw <- subset(region_deaths_raw, covid_death_data$Province.State == Province.State_input)
  # }

  cumu_cases <- as.numeric(colSums(region_cases_raw[,12:ncol(covid_data)]))
  cumu_deaths <- as.numeric(colSums(region_deaths_raw[,13:ncol(covid_death_data)]))
  
  total_cases <- cumu_cases[length(cumu_cases)]
  total_deaths <- cumu_deaths[length(cumu_deaths)]
  
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
    if (normalize_by_npop) {
      plot(dates,cumu_cases/npop*100,main = paste(Province_State_input,Admin2_input,"cumulative cases (% pop)"))
      plot(dates,cumu_deaths/npop*100,main = paste(Province_State_input,Admin2_input,"cumulative deaths (% pop)"))
      plot(dates,cases/npop*100,main=paste(Province_State_input,Admin2_input,"daily cases (% pop)"))
      lines(dates,cases/npop*100)
      if (total_cases > 0) {
        plot(dates,cases/npop*100,main=paste(Province_State_input,Admin2_input,"log daily cases (% pop)"),log="y")
      }
      plot(dates,deaths/npop*100,main=paste(Province_State_input,Admin2_input,"daily deaths (% pop)"))
      if (total_deaths > 0) {
        plot(dates,deaths/npop*100,main=paste(Province_State_input,Admin2_input,"log daily deaths (% pop)"),log="y")
      }
    } else {
      plot(dates,cumu_cases,main = paste(Province_State_input,Admin2_input,"cumulative cases"))
      plot(dates,cumu_deaths,main = paste(Province_State_input,Admin2_input,"cumulative deaths"))
      plot(dates,cases,main=paste(Province_State_input,Admin2_input,"daily cases"))
      if (total_cases > 0) {
        plot(dates,cases,main=paste(Province_State_input,Admin2_input,"log daily cases"),log="y")
      }
      plot(dates,deaths,main=paste(Province_State_input,Admin2_input,"daily deaths"))
      if (total_deaths > 0) {
        plot(dates,deaths,main=paste(Province_State_input,Admin2_input,"log daily deaths"),log="y")
      }
    }
    
    
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