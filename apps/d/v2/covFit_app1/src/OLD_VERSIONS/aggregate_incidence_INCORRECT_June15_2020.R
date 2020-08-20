
aggregate_incidence <- function(dates,aggr_dates,incidence) {
  indices <- c(dates %in% aggr_dates)[1:length(incidence)]
  incidence_aggr <- incidence[indices]
  return(incidence_aggr)
  # df <- data.frame("dates"=dates,"incidence"=incidence)
  # df_aggr <- subset(df,)
}