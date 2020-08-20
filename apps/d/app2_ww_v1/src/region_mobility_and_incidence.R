
region_mobility_and_incidence <- function(
  covid19,
  covid19_deaths,
  covid19_US,
  covid19_deaths_US,
  google_mobility,
  pop,
  Country.Region,
  Province.State,
  mobility_country,
  mobility_region,
  mobility_subregion,
  sigma_SEIR,
  gamma_SEIR,
  onset_date_override,
  turnover_date_override,
  mobility_window_half_width,
  incidence_window_half_width,
  xlim,
  pathname) {
  if (Province.State != "") {
    use_Province.State <- T
  } else {
    use_Province.State <- F
  }
  incidence <- jhu_covid_get_region_incidence_2(covid19,
                                                covid19_deaths,
                                                "2020-01-22",
                                                Country.Region,
                                                use_Province.State = use_Province.State,
                                                exclude_Province.State =  F,
                                                Province.State,
                                                F)
  mobility_and_incidence <- mobility_and_incidence_v6(incidence,
                                                      google_mobility,
                                                      pop, 
                                                      cumu_case_fraction_threshold =  1e-7,
                                                      sigma_SEIR = sigma_SEIR,
                                                      gamma_SEIR = sigma_SEIR,
                                                      window_for_current_R = 28,
                                                      plot_current_R_TF = F, # Do we plot (in green) fit for the current R?
                                                      US_TF = F,
                                                      mobility_region = mobility_region,
                                                      mobility_subregion = mobility_subregion,
                                                      mobility_country = mobility_country,
                                                      onset_date_override = onset_date_override,
                                                      turnover_date_override = turnover_date_override,
                                                      # Province.State,
                                                      mobility_window_half_width =   7,
                                                      incidence_window_half_width =  7, # This MUST be >= 1 (i.e. a minimum 3-day window)
                                                      diagnostic_mode_TF =  F,
                                                      title =  Country.Region,
                                                      xlim = xlim,
                                                      pathname = pathname)
  summary <- data.frame("country" = mobility_country,
                            "region" = mobility_region,
                            "subregion" = mobility_subregion,
                            "R" = mobility_and_incidence$initial_exp_growth$R,
                            "R_post_exp" = mobility_and_incidence$initial_exp_growth$R_post_exp,
                            "R_pct_change" = (mobility_and_incidence$initial_exp_growth$R - mobility_and_incidence$initial_exp_growth$R_post_exp)/mobility_and_incidence$initial_exp_growth$R,
                            "exp_phase_duration" = mobility_and_incidence$initial_exp_growth$exponential_portion_duration,
                            "mobility_min" = mobility_and_incidence$google_mobility$min_of_mean_rec_groc_trans_work,
                            "cumu_incidence" = incidence$cumu_cases[length(incidence$cumu_cases)],
                            "cumu_incidence_per_100k" = incidence$cumu_cases[length(incidence$cumu_cases)]/pop*100000
                            # "R_pct_change" = mobility_and_incidence$
  )
  output_to_return <- list("summary" = summary, "mobility_and_incidence" = mobility_and_incidence)
  return(output_to_return)
}