
death_lags <- function(incidence,lags_vector, plot_TF) {
  output_to_return <- data.frame("lag"=lags_vector,"lagged_current_cumu_incidence" = NA, "cfr" = NA )
  for (i in 1:length(lags_vector)) {
    cumu_cases_lagged <- lag(incidence$cumu_cases,lags_vector[i])
    cfr <- incidence$cumu_deaths/incidence$cumu_cases
    cfr_lagged <- incidence$cumu_deaths/cumu_cases_lagged
    df <- data.frame(incidence,"cfr"=cfr, "cumu_cases_lagged"=cumu_cases_lagged, "cfr_lagged"=cfr_lagged) 
    output_to_return$lagged_current_cumu_incidence[i] <- df$cumu_cases_lagged[length(cumu_cases_lagged)]
    output_to_return$cfr[i] <- cfr_lagged[length(cfr_lagged)]
    
    if (plot_TF) {
      plot1 <- ggplot(data=df,aes(x=dates,y=cumu_cases)) + geom_line() +
        geom_line(aes(x=dates, y=cumu_cases_lagged), linetype=2) +
        geom_line(aes(x=dates,y=cumu_deaths),color="red") +
        scale_y_log10() +
        labs(title = paste0("lag = ",lags_vector[i]))
      
      plot2 <- ggplot(data=df,aes(x=dates,y=cfr)) + geom_line(color="blue") +
        geom_line(aes(x=dates, y=cfr_lagged), color="blue", linetype=2) + 
        ylim(0,0.4)
      
      grid.arrange(plot1,plot2,nrow = 2)
    }
  }
  return(output_to_return)
}
  


