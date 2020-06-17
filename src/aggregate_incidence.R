
aggregate_incidence <- function(incidence_frame, N_days_to_aggregate) {
  browser()
  dates_aggr <- seq.Date(incidence_frame$dates[1], incidence_frame$dates[length(incidence_frame$dates)],by=N_days_to_aggregate)
  indices <- c(incidence_frame$dates %in% dates_aggr)[1:length(incidence_frame)]
  cumu_incidence_aggr <- incidence[indices]
  return(incidence_aggr)
  # df <- data.frame("dates"=dates,"incidence"=incidence)
  # df_aggr <- subset(df,)
}