
find_turnover_point_v2 <- function(incidence_frame,min_turnover_date, max_turnover_date, max_date_to_consider) {
  df_nonzero <- incidence_frame %>%  subset(cases > 0)
  df_nonzero <- data.frame(df_nonzero, "log_cases" = log(df_nonzero$cases), "SSE" = Inf)
  
  # NOTE: for the purposes of finding the turnover point, we consider only the days up to day [max_date_to_consider].
  # However, for fitting the post-turnover regime, we consider all days (i.e. all of frame [df_nonzero])
  df <- df_nonzero %>% subset(dates <= max_date_to_consider)
  if (nrow(df) > 0) {
    first_index <- min(which(df$dates >= min_turnover_date))
    first_index <- max(first_index,3) # Can't have a segment with less than 3 points
    last_index <- max(which(df$dates <= max_turnover_date))
    
    if (first_index < last_index) {
      for (i in first_index:last_index) {
        df1 <- df[1:i,]
        df2 <- df[i:nrow(df),]
        fit1 <- lm(log_cases~dates, data=df1)
        fit2 <- lm(log_cases~dates, data=df2)
        SSE1 <- sum(fit1$residuals^2)
        SSE2 <- sum(fit2$residuals^2)
        df$SSE[i] <- SSE1 + SSE2
      }
      ind_best <- which.min(df$SSE)
      turnover_date <- df$dates[ind_best]
    } else {
      turnover_date <- NA
    }
    
  } else {
    turnover_date <- NA
  }
  
  # df1_best <- df_nonzero[1:ind_best,]
  # df2_best <- df_nonzero[ind_best:nrow(df_nonzero),]
  # fit1_best <- lm(log_cases~dates, data=df1_best)
  # fit2_best <- lm(log_cases~dates, data=df2_best)
  # newdata1 <- data.frame("dates"=df_nonzero$dates[1:ind_best])
  # newdata2 <- data.frame("dates"=df_nonzero$dates[(ind_best):nrow(df_nonzero)])
  # prediction1 <- predict(fit1_best, newdata=newdata1, interval="confidence")
  # prediction2 <- predict(fit2_best, newdata=newdata2, interval="confidence")
  # 
  # df1_best <- data.frame("dates"=df1_best$dates,  exp(prediction1))
  # df2_best <- data.frame("dates"=df2_best$dates,  exp(prediction2))
  # 
  # plot1 <- ggplot(data=df_nonzero, aes(x=dates, y=cases)) + geom_point() +
  #   geom_vline(xintercept = turnover_date) +
  #   geom_line(data=df1_best,aes(x=dates,y=fit), color="red", size=1) +
  #   geom_line(data=df2_best,aes(x=dates,y=fit), color="blue", size=1) +
  #   geom_ribbon(data=df1_best,aes(y=fit, ymin=lwr, ymax=upr),fill="red",alpha=0.4) +
  #   geom_ribbon(data=df2_best,aes(y=fit, ymin=lwr, ymax=upr),fill="blue",alpha=0.4)
  # print(plot1)
  # plot2 <- plot1 + scale_y_log10()
  # print(plot2)
  # 
  # browser()
  # result_to_return <- list("turnover_date"=turnover_date, "df1_best"=df1_best, "df2_best"=df2_best)
  result_to_return <- turnover_date
  return(result_to_return)
}