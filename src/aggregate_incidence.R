
aggregate_incidence <- function(incidence, N_days_to_aggregate) {
  dates_aggr <- seq.Date(incidence$dates[1], incidence$dates[length(incidence$dates)],by=N_days_to_aggregate)
  indices <- c(incidence$dates %in% dates_aggr)[1:length(incidence)]
  incidence_aggr <- incidence[indices,]
  cases <- c(0,diff(incidence_aggr$cumu_cases))
  deaths <- c(0,diff(incidence_aggr$cumu_deaths))
  incidence_aggr$cases <- cases
  incidence_aggr$deaths <- deaths
  return(incidence_aggr)
  # df <- data.frame("dates"=dates,"incidence"=incidence)
  # df_aggr <- subset(df,)
}