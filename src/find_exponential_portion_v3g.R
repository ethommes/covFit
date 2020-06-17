# _v3e adds calculation of latest R_eff, over the last N days, where N can be specified.
# _v3f: 
#   - outputs also onset date
#   - allows selecting whether or not to plot current R 

find_exponential_portion_v3g <- function(incidence_list,
                                         population,
                                         CFR, # NA to not perform correction
                                     N_days_to_aggregate, 
                                     daily_case_threshold,
                                     cumu_case_threshold,
                                     max_starting_index,
                                     onset_date_override, # set to NA for no override
                                     turnover_date_override, # set to NA for no override
                                     interval_type,
                                     window_for_current_R,
                                     predict_date, # date which to extrapolate current R portion
                                     plot_current_R_TF, # Do we plot (in green) the fit for the current R?
                                     sigma_SEIR,
                                     gamma_SEIR, 
                                     plot_TF,
                                     title,
                                     CFR_text,
                                     # county,
                                     # state,
                                     pathname) { 
  # title = paste(county,state)
  # Aggregate by the specified number of days, i.e. if N_days_to_aggregate = 2,
  # then combine every 2 days:
  dates <- incidence_list$dates
  cases <- incidence_list$cases
  
  # NOTE: deactivate ability to aggregate dates for now
  # dates_aggr <- seq.Date(dates[1], dates[length(dates)],by=N_days_to_aggregate)
  # cases_aggr <- aggregate_incidence(dates,dates_aggr,cases)
  dates_aggr <- dates
  cases_aggr <- cases
  incidence_aggr <- data.frame("dates"=dates_aggr,"cases"=cases_aggr)
  # Total number of cases: 
  cases_total <- incidence_list$cumu_cases[nrow(incidence_list)]
  latest_date <- incidence_list$dates[nrow(incidence_list)]
  
  # Date on which cumulative case threshold is reached:
  onset_index <- min(which(incidence_list$cumu_cases >= cumu_case_threshold))
  onset_date <- incidence_list$date[onset_index]
  
  
  # Get rid of days below the specified daily case threshold:
  indices <- (cases_aggr >= daily_case_threshold)
  frame_for_lm <- data.frame("dates" = dates_aggr, 
                             "y" = cases_aggr, 
                             "logy"=log(cases_aggr))[indices,]
  
  # Get rid of the days before the cumulative case threshold is reached, UNLESS we have specified an override
  # date for the onset date:
  if (is.na(onset_date_override)) {
    indices <- (frame_for_lm$dates >= onset_date)
    frame_for_lm <- frame_for_lm[indices,]
  }
 
  
  
  # max_starting_index <- 10 # The highest-index point that is considered as a candidate starting point for the regression
  
  max_nrow <- max_starting_index*nrow(frame_for_lm)
  fit_results_frame <- data.frame(
    "t_value_slope" = rep(NA,max_nrow),
    "i_index" = rep(NA,max_nrow),
    "j_index" = rep(NA,max_nrow)
  )
  # rsquared <- rep(NA,max_starting_index*nrow(frame_for_lm)) # length is too large, but we'll set it to this for simplicity
  # adj_rsquared <- rsquared
  # chiSquared <- rsquared
  # fstat <- rsquared
  # sigma <- rsquared
  # t_value_slope <- rsquared
  # max_residual <- rsquared
  # i_index <- rsquared
  # j_index <- rsquared
  
  
  index <- 0 # We will flatten the table generated in the following nested loop, using 'index' as the index
  for (i in 1:max_starting_index) { # loop over candidate first points, i.e. up to and including `max_starting_index`
    for (j in (i+5):nrow(frame_for_lm)) { # minimum window size is 6
      index <- index + 1
      fit_results_frame$i_index[index] <- i
      fit_results_frame$j_index[index] <- j
      subframe <- frame_for_lm[i:j,]
      fit <- lm(logy~dates, data=subframe)
      regr_summary <- summary(fit)
      # rsquared[index] <- regr_summary$r.squared
      # adj_rsquared[index] <- regr_summary$adj.r.squared
      # fstat[index] <- regr_summary$fstatistic[1]
      # sigma[index] <- regr_summary$sigma
      # max_residual[index] <- max(regr_summary$residuals)
      fit_results_frame$t_value_slope[index] <- regr_summary$coefficients["dates","t value"]
    }
  }
  # Lop off the un-needed rows from fit_results_frame:
  fit_results_frame <- fit_results_frame[1:index,]
  # if (county == "Fresno") {browser()}
  
  
  # Let's go with the maximum t test value as the metric of where things turn over, 
  # since that seems to be the generally most sharply-peaked metric:
  # Index of maximum t_value_slope:
  ind_max_t_value <- which.max(fit_results_frame$t_value_slope)
  # From the above, set our onset and turnover dates and corresponding indices, UNLESS on over-ride date has been 
  # supplied for one or the other 
  if (is.na(onset_date_override)) { 
    onset_index <- fit_results_frame$i_index[ind_max_t_value]
    onset_date <- frame_for_lm$dates[onset_index]
  } else {
    onset_date <- onset_date_override
    onset_index <- min(which(frame_for_lm$dates >= onset_date)) # allow for possibility that the exact override date had zero incidence and thus doesn't exist in frame_for_lm
  }
  if (is.na(turnover_date_override)) {
    turnover_index <- fit_results_frame$j_index[ind_max_t_value]
    turnover_date <- frame_for_lm$dates[turnover_index]
  } else {
    turnover_date <- turnover_date_override
    turnover_index <- min(which(frame_for_lm$dates >= turnover_date)) # allow for possibility that the exact override date had zero incidence and thus doesn't exist in frame_for_lm
  }
    
  exponential_portion <- frame_for_lm[onset_index:turnover_index,]
  
  # Analysis of exponential portion:
  exp_portion_fit <- lm(logy~dates,data=exponential_portion)
  
  newdata <- data.frame("dates" = seq(exponential_portion$dates[1],exponential_portion$dates[length(exponential_portion$dates)],by="day"))
  prediction <- predict(exp_portion_fit, newdata =newdata,interval=interval_type)
  exp_portion_fit_for_plot <- data.frame(newdata,exp(prediction))
  
  fit_summary <- summary(exp_portion_fit) 
  intercept <- fit_summary$coefficients["(Intercept)","Estimate"] 
  slope <- fit_summary$coefficients["dates","Estimate"]
  doubling_time <- log(2)/slope
  R <- R0_SEIR(slope,sigma_SEIR,gamma_SEIR)
  
  # Analysis of post-exponential portion.  If the number of points after this value is 
  # less than four, we can't sensibly
  # calculate a slope for the post-exponential period.  So, skip this part and fill in NAs
  # for everything
  if (turnover_index <= (nrow(frame_for_lm) - 10)) {
    post_exponential_portion <- frame_for_lm[(turnover_index+1):nrow(frame_for_lm),]
    post_exp_portion_fit <- lm(logy~dates,data=post_exponential_portion)
    
    newdata <- data.frame("dates" = seq(post_exponential_portion$dates[1],post_exponential_portion$dates[length(post_exponential_portion$dates)],by="day"))
    prediction <- predict(post_exp_portion_fit, newdata =newdata,interval=interval_type)
    post_exp_portion_fit_for_plot <- data.frame(newdata,exp(prediction))
    
    fit_summary_post_exp <- summary(post_exp_portion_fit) 
    intercept_post_exp <- fit_summary_post_exp$coefficients["(Intercept)","Estimate"] 
    slope_post_exp <- fit_summary_post_exp$coefficients["dates","Estimate"]
    doubling_time_post_exp <- log(2)/slope_post_exp
    R_post_exp <- R0_SEIR(slope_post_exp,sigma_SEIR,gamma_SEIR)
    R_percent_reduction <- (R - R_post_exp)/R*100
    
  } else {
    intercept_post_exp <- NA 
    slope_post_exp <- NA
    doubling_time_post_exp <- NA
    R_post_exp <- NA
    R_percent_reduction <- NA
  }
  
  # Estimate current R using the last [window_for_current_R] days:
  latest_index <- nrow(frame_for_lm)
  if (latest_index > window_for_current_R) {
    current_portion <- frame_for_lm[(latest_index - window_for_current_R):latest_index,]
    current_fit <- lm(logy~dates, data=current_portion)
    newdata <- data.frame("dates" = seq(current_portion$dates[1],predict_date,by="day"))
    prediction <- predict(current_fit, newdata =newdata,interval=interval_type)
    current_portion_fit_for_plot <- data.frame(newdata,exp(prediction))
    current_fit_summary <- summary(current_fit)
    intercept_current <- current_fit_summary$coefficients["(Intercept)","Estimate"]
    slope_current <- current_fit_summary$coefficients["dates","Estimate"]
    doubling_time_current <- log(2)/slope_current
    R_current <- R0_SEIR(slope_current,sigma_SEIR,gamma_SEIR)
  } else {
    intercept_current <- NA
    slope_current <- NA
    doubling_time_current <- NA
    R_current <- NA
    plot_current_R_TF <- F # deactivate plotting of the current/projection portion
  }
  

  # regression fit of values with index <= ind_max_t_value:
  # Post-exponential portion:
  if (population > 0) {
    SF <- 1e5/population # scale factor for plotting
  } else {
    SF <- 1
    pathname <- NA # Prevent plotting
  }
  
  y_label <- paste("cases per ",N_days_to_aggregate,"days per 100,000 population")
  # SF <- 1
  text_plot_1 <- paste0(
    CFR_text,
    "Total cases up to ",latest_date,":\n",cases_total," (",round(cases_total*1e5/population,0)," per 100,000)\n \n",
    "Initial exponential phase:\n",
    "   rho = ", round(slope,2),"\n",
    "   doubling time = ",round(doubling_time,2)," days (negative value = halving time)\n",
    "   R = ",round(R,2), "\n",
    "   turnover date = ",turnover_date,"\n"
  )
   

  text_plot_2 <- paste0(
    "Post-turnover phase:\n",
    "   rho = ", round(slope_post_exp,2),"\n",
    "   doubling time = ",round(doubling_time_post_exp,2)," days\n",
    "   R_post = ",round(R_post_exp,2),"\n",
    "                 = ",round(R_post_exp/R,2)," R\n",
    "                 = ",round(R_percent_reduction,2),"% reduction\n",
    "\n Last ",window_for_current_R, " days:\n",
    "   R = ",round(R_current,2),"\n",
    "   doubling time = ",round(doubling_time_current,2)," days\n"
  )
  
  the_plot <- ggplot(data=exp_portion_fit_for_plot) + 
    geom_line(aes(x=dates,y=fit*SF),color="red") + 
    geom_ribbon(aes(x=dates,ymin=lwr*SF, ymax=upr*SF),fill="red",alpha=0.4) + 
    # geom_line(data=post_exp_portion_fit_for_plot,aes(x=dates,y=fit),color="red") + 
    geom_point(data=incidence_aggr,aes(x=dates,y=cases*SF)) + 
    geom_point(data=exponential_portion,aes(x=dates,y=y*SF),color="red") + 
    # ylim(1,2*max(incidence_aggr$cases*SF))
    # annotate_textp(x=0.02,y=0.98, label=text_plot_1, size=9) +
    # annotate_textp(x=0.02,y=0.98,label=paste0(
    #   CFR_text,
    #   "Total cases up to ",latest_date,":\n",cases_total," (",round(cases_total*1e5/population,0)," per 100,000)\n \n",
    #   "Initial exponential phase:\n",
    #   "   rho = ", round(slope,2),"\n",
    #   "   doubling time = ",round(doubling_time,2)," days (negative value = halving time)\n",
    #   "   R = ",round(R,2), "\n",
    #   "   turnover date = ",turnover_date,"\n",
    #   "\nPost-turnover phase:\n",
    #   "   rho = ", round(slope_post_exp,2),"\n",
    #   "   doubling time = ",round(doubling_time_post_exp,2)," days\n",
    #   "   R_post = ",round(R_post_exp,2),"\n",
    #   "                 = ",round(R_post_exp/R,2)," R\n",
    #   "                 = ",round(R_percent_reduction,2),"% reduction\n",
    #   "\n Last ",window_for_current_R, " days:\n",
    #   "   R = ",round(R_current,2),"\n",
    #   "   doubling time = ",round(doubling_time_current,2)," days\n"), size=9) +
  
    # labs(title = paste("Initial exponential growth, ",title,sep=""), x="date", y = y_label)
    labs(x="date")
  
  
    
  
   
  
  
  
  if (!is.na(slope_post_exp)) {
    the_plot <- the_plot +
      geom_line(data=post_exp_portion_fit_for_plot,aes(x=dates,y=fit*SF),color="blue") +
      geom_ribbon(data = post_exp_portion_fit_for_plot, aes(x=dates,ymin=lwr*SF,ymax=upr*SF),fill="blue",alpha=0.4) +
      geom_point(data=post_exponential_portion,aes(x=dates,y=y*SF),color="blue")
    
  }
  # If plot_current_R_TF = TRUE, add on the current fit:
  if (plot_current_R_TF) {
    the_plot <- the_plot +
      geom_line(data=current_portion_fit_for_plot,aes(x=dates,y=fit*SF),color="darkgreen") +
      geom_ribbon(data = current_portion_fit_for_plot, aes(x=dates,ymin=lwr*SF,ymax=upr*SF),fill="green",alpha=0.4) +
      geom_point(data=current_portion,aes(x=dates,y=y*SF),color="darkgreen",shape=0, size=3)
      
      
      # geom_abline(intercept=intercept_current, slope=slope_current, color="green") +
      # geom_point(data=current_portion,aes(x=dates,y=logy),color="green")
  }
  
  # write_output is set to F if population = 1 [may want to add other conditions...]
  if (!is.na(pathname)) {
    the_plot_linear <- the_plot + ylim(0,1.5*max(incidence_aggr$cases*SF)) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) +
      annotate_textp(x=0.02,y=0.98, label=text_plot_1, size=9)
    filename <- paste0(title,"_exponential_growth.png")
    # ggsave(path=pathname, filename = filename)
    
    the_plot_log <- the_plot + scale_y_log10() +
      theme(axis.title.y = element_blank()) +
      annotate_textp(x=0.02,y=0.98, label=text_plot_2, size=9)
    filename <- paste0(title,"_exponential_growth_LOG.png",sep="")
    # ggsave(path=pathname, filename=filename)
  }
  gA <- ggplotGrob(the_plot_linear)
  gB <- ggplotGrob(the_plot_log)
  grid.newpage()
  # twopanel_plot <- grid.arrange(rbind(gA, gB),left = y_label, top=title)
  twopanel_plot <- arrangeGrob(rbind(gA, gB),left = y_label, top=title)
  
  
  
  # twopanel_plot <- grid.arrange(the_plot_linear, the_plot_log,nrow=2)
  filename <- paste0(title,"_exponential_fit.png")
  # ggsave(path=pathname, filename=filename)
  ggsave(path=pathname, filename=filename, twopanel_plot)

  # linear_plot <- ggplot(data=incidence_aggr,aes(x=dates,y=cases)) + 
  #   geom_point() +
  #   geom_line()
  


  # TEST: plot cumulative number of cases
  the_plot_TEST2 <- ggplot(data = incidence_list,aes(x=dates, y = log(cumu_cases/population))) + geom_point()
  
  if (plot_TF & !is.na(pathname)) {
    # print(linear_plot)
    print(the_plot)
    # print(the_plot_TEST)
    # print(the_plot_TEST2)
    
  }
  # result_to_return <- list(
  result_to_return <- data.frame(
    "onset_date" = onset_date,
    "turnover_date" = turnover_date,
    "exponential_portion_duration" = as.numeric(turnover_date - onset_date),
    "rho" = slope,
    "doubling_time" = doubling_time,
    "R" = R,
    "rho_post_exp" = slope_post_exp,
    "doubling_time_post_exp" = doubling_time_post_exp,
    "R_post_exp" = R_post_exp,
    "rho_current" = slope_current,
    "doubling_time_current" = doubling_time_current,
    "R_current" = R_current,
    "sigma_SEIR" = sigma_SEIR,
    "gamma_SEIR" = gamma_SEIR
    # "plot" = the_plot
  )
  return(result_to_return)
  
}