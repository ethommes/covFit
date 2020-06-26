
plot_current_vs_projected_incidence_states <- function(data_table, projection_date, pathname) {
  data_table_proj <- forecast_from_US_states_or_counties_frame(data_table, as.Date("2020-06-30"), projection_date)
  current_date <- data_table_proj$last_date[1]
  
  plt1 <- ggplot(data_table_proj,aes(x=incidence_current/pop*1e5, y = forecast_incidence_2_from_recent/pop*1e5,color="red")) + 
    geom_point() +
    geom_text_repel(aes(label=state_abbr),hjust=0.2,vjust=-0.5,size=3,color="darkgray") +
    geom_point(aes(x=incidence_current/pop*1e5, y=forecast_incidence_2_from_post_turnover/pop*1e5,color="black")) +
    labs(title=paste0("State daily incidence, current (", current_date, ") \nand ", projection_date, " projection"), x="current cases per day per 100k",
         y="projected cases per day per 100k") + 
    # xlim(0,75) + ylim(0,160) +
    geom_abline(slope=1,intercept=0) +
    scale_color_manual(name = "legend", labels=c("projected \nusing all \npost-turnover\n ", 
                                                 " \nprojected \nusing past\n2 weeks\n"), values = c("red"="red","black"="black"))
  
  plt2 <- plt1 + scale_x_log10() + scale_y_log10()
  
  plt3 <- plt2 + geom_errorbar(aes(ymin = forecast_incidence_2_from_post_turnover/pop*1e5,
                                     ymax=forecast_incidence_2_from_recent/pop*1e5),color="black")
  print(plt3)
  
  ggsave(path = pathname, filename = paste0("current_vs_projected_incidence_US_states_",projection_date,".png"))
}

#======================================

map_cumulative_incidence_US_states <- function(proc_US_states_frame) {
  df_plot1 <- proc_US_states_frame %>% subset(is.finite(cumu_cases_per_100k_corrected)) %>%
    subset(state_abbr != "") 
  df_plot2 <- data.frame("state"=df_plot1$state_abbr, "cases_per_100k" = df_plot1$cumu_cases_per_100k_corrected)
  df_plot2 <- df_plot2[5:nrow(df_plot2),]
  
  # base plot + linear version
  plot_incidence_map_all_us <- usmap::plot_usmap(data=df_plot2,values="cases_per_100k", color="darkgray", labels=T) + 
    labs(title=paste0("Cumulative incidence up to ", proc_US_states_frame$last_date[1]))
  plot_incidence_map_all_us_linear <- plot_incidence_map_all_us  +
    scale_fill_continuous(name = "cases per 100k")
  # print(plot_incidence_map_all_us_linear)
  
  # log version
  my_breaks <- c(1,10,100,1000,10000,100000)
  plot_incidence_map_all_us_log <- plot_incidence_map_all_us +
    scale_fill_gradient(name="cumulative\ncases\nper 100k", trans="log", breaks=my_breaks, labels=my_breaks,
                        low="white", high="red") 
  print(plot_incidence_map_all_us_log)
  ggsave(path=pathname_figs, filename=paste0("map_cumulative_incidence_", proc_US_states_frame$last_date[1], ".png"))
}

#=====================================

map_daily_incidence_US_states <- function(US_states_frame, projection_date, projection_baseline) {
  # - use NA for projection_date to map current incidence
  # - projection_baseline can have value of either "recent" or "post_turnover"
  
  # pre-process 
  my_breaks <- c(0.1,1,10,100,1000,10000,100000)
  if (is.na(projection_date)) {
    df_plot1 <- data.frame("state"=US_states_frame$state_abbr, "daily_cases_per_100k" = US_states_frame$incidence_current/proc_US_states_frame$pop*1e5)
    df_plot2 <- df_plot1 %>% subset(!is.na(state) & is.finite(daily_cases_per_100k)) 
    df_plot2 <- df_plot2[5:nrow(df_plot2),]
    title <- paste0("Current (", US_states_frame$last_date[1], ") incidence,\n",
                    "averaged over last ", US_states_frame$window_for_current[1], " days")
  } else {
    US_states_frame_proj <- forecast_from_US_states_or_counties_frame(US_states_frame, as.Date("2020-06-30"), projection_date)
    if (projection_baseline == "recent") {
      df_plot1 <- data.frame("state"=US_states_frame$state_abbr,
                             "daily_cases_per_100k" = US_states_frame_proj$forecast_incidence_2_from_recent/US_states_frame_proj$pop*1e5)
      df_plot2 <- df_plot1 %>% subset(!is.na(state) & is.finite(daily_cases_per_100k)) 
      df_plot2 <- df_plot2[5:nrow(df_plot2),]

      title <- paste0("Projected incidence for ", projection_date, ",\n",
                      "made on ", US_states_frame$last_date[1], ",\n",
                      "using incidence over last ", US_states_frame$window_for_current[1], " days")
    } else {
      df_plot1 <- data.frame("state"=US_states_frame$state_abbr,
                             "daily_cases_per_100k" = US_states_frame_proj$forecast_incidence_2_from_post_turnover/US_states_frame_proj$pop*1e5)
      df_plot2 <- df_plot1 %>% subset(!is.na(state) & is.finite(daily_cases_per_100k)) 
      df_plot2 <- df_plot2[5:nrow(df_plot2),]
      title <- paste0("Projected incidence for ", projection_date,",\n", 
                      "made on ", US_states_frame$last_date[1], ",\n",
                      "using full post-turnover phase of each state")
    }
  }
  plt1 <- plot_usmap(data=df_plot2,values="daily_cases_per_100k", color="darkgray", labels=T) +
    labs(title=title)
  plt1_log <- plt1 + scale_fill_gradient(name="daily cases\nper 100k", trans="log", breaks=my_breaks, labels=my_breaks, low="white", high="red")
  # print(plot_linear)
  print(plt1_log)
  ggsave(path=pathname_figs, filename=paste0("map_daily_incidence_projection_", projection_date, ".png"))
  
}

# =================

map_daily_incidence_US_counties <- function(US_counties_frame, regions_vector, projection_date, projection_baseline, 
                                            print_county_labels_TF, pathname) {
  US_counties_frame <- US_counties_frame[5:nrow(US_counties_frame),]
  US_counties_frame <- US_counties_frame %>% subset(is.finite(incidence_current))
  my_breaks <- c(0.1,1,10,100,1000,1e4)
  if (is.na(projection_date)) {
    df_plot2 <- data.frame("fips"=US_counties_frame$FIPS, "daily_cases_per_100k" = US_counties_frame$incidence_current/US_counties_frame$pop*1e5)
    df_plot2$daily_cases_per_100k[df_plot2$daily_cases_per_100k > 1e5] <- 1e5 # Can't infect more than the total population on one day!
    # df_plot2 <- df_plot1 %>% subset(is.finite(daily_cases_per_100k)) 
    # df_plot2 <- df_plot2[5:nrow(df_plot2),]
    # df_plot2 <- df_plot2[5:nrow(df_plot2),]
    title <- paste0("Current (", US_counties_frame$last_date[1], ") incidence,\n",
                    "averaged over last ", US_counties_frame$window_for_current[1], " days\n",
                    " ")
  } else {
    US_counties_frame_proj <- forecast_from_US_states_or_counties_frame(US_counties_frame, as.Date("2020-06-30"), projection_date)
    if (projection_baseline == "recent") {
      df_plot1 <- data.frame("fips"=US_counties_frame_proj$FIPS,
                             "daily_cases_per_100k" = US_counties_frame_proj$forecast_incidence_2_from_recent/US_counties_frame$pop*1e5)
      # df_plot1$daily_cases_per_100k <- min(1e5, df_plot1$daily_cases_per_100k)
      df_plot2 <- df_plot1 %>% subset(is.finite(daily_cases_per_100k))
      df_plot2$daily_cases_per_100k[df_plot2$daily_cases_per_100k > 1e5] <- 1e5
      # df_plot2 <- df_plot2[5:nrow(df_plot2),]
      
      title <- paste0("Projected incidence for ", projection_date, ",\n",
                      "made on ", US_counties_frame$last_date[1], ",\n",
                      "using incidence over last ", US_counties_frame$window_for_current[1], " days")
    } else {
      df_plot1 <- data.frame("fips"=US_counties_frame_proj$FIPS,
                             "daily_cases_per_100k" = US_counties_frame_proj$forecast_incidence_2_from_post_turnover/US_counties_frame$pop*1e5)
      # df_plot1$daily_cases_per_100k <- min(1e5, df_plot1$daily_cases_per_100k)
      df_plot2 <- df_plot1 %>% subset(is.finite(daily_cases_per_100k))
      df_plot2$daily_cases_per_100k[df_plot2$daily_cases_per_100k > 1e5] <- 1e5
      # df_plot2 <- df_plot2[5:nrow(df_plot2),]
      title <- paste0("Projected incidence for ", projection_date,",\n", 
                      "made on ", US_counties_frame_proj$last_date[1], ",\n",
                      "using full post-turnover phase of each county")
    }
  }
  
  incidence_max <- max(df_plot2$daily_cases_per_100k)
  if (is.na(regions_vector)) {
    plt1 <- plot_usmap(regions = "counties", data=df_plot2, values="daily_cases_per_100k", color="NA", labels=print_county_labels_TF) +
      labs(title=title)
  } else {
    plt1 <- plot_usmap(regions = "counties", include = regions_vector, data=df_plot2, values="daily_cases_per_100k", color="NA", labels=print_county_labels_TF) +
      labs(title=title)
  }
  
  plt1_log <- plt1 + scale_fill_gradient(name="daily cases\nper 100k", trans="log", breaks=my_breaks, labels=my_breaks, limits=c(0.1,incidence_max),low="white", high="red") +
    theme(legend.position = "right")
    # print(plot_linear)
  print(plt1_log)
  regions_vector_string <- paste0(regions_vector, collapse="_")
  filename <- paste0("map_county_daily_incidence_projection_", regions_vector_string, "_", projection_date, ".png")
  ggsave(path=pathname, filename=filename)
  
  return(df_plot2)

}