
read_google_mobility <- function(google_mobility,
                                mobility_country, 
                                mobility_region, 
                                mobility_subregion,
                                mobility_window_half_width) {
  df_google <- google_mobility[google_mobility$country_region == mobility_country &
                                 google_mobility$sub_region_1 == mobility_region &
                                 google_mobility$sub_region_2 == mobility_subregion,]
  df_google$date <- as.Date(df_google$date)
  if (nrow(df_google > 0)) {
    recreation <- frollmean(df_google$retail_and_recreation_percent_change_from_baseline, 
                            (mobility_window_half_width*2 + 1), 
                            align = "center") + 100
    grocery <- frollmean(df_google$grocery_and_pharmacy_percent_change_from_baseline, 
                         (mobility_window_half_width*2 + 1), 
                         align = "center") + 100
    parks <- frollmean(df_google$parks_percent_change_from_baseline, 
                       (mobility_window_half_width*2 + 1), 
                       align = "center") + 100
    transit <- frollmean(df_google$transit_stations_percent_change_from_baseline, 
                         (mobility_window_half_width*2 + 1), 
                         align = "center") + 100
    workplaces <- frollmean(df_google$workplaces_percent_change_from_baseline, 
                            (mobility_window_half_width*2 + 1), 
                            align = "center") + 100
    residential <- frollmean(df_google$residential_percent_change_from_baseline, 
                             (mobility_window_half_width*2 + 1), 
                             align = "center") + 100
    
    df_google <- data.frame(
      df_google,
      "recreation_roll" = recreation,
      "grocery_roll" = grocery,
      "parks_roll" = parks,
      "transit_roll" = transit,
      "workplaces_roll" = workplaces,
      "residential_roll" = residential
    )
    
    mean_rec_groc_trans_work <- rowMeans(df_google[,c('recreation_roll','grocery_roll', 'transit_roll', 'workplaces_roll')],na.rm = T)
    df_google <- data.frame(df_google,"mean_rec_groc_trans_work" = mean_rec_groc_trans_work)
    
    # i_max <- which.max(df_google$recreation_roll)
    # i_min <- which.min(df_google$recreation_roll)
    # recreation_max <- df_google$recreation_roll[i_max]
    # recreation_min <- df_google$recreation_roll[i_min]
    # date_recreation_max <- df_google$date[which.max(df_google$recreation_roll)]
    # date_recreation_min <- df_google$date[which.min(df_google$recreation_roll)]
    # date_recreation_mid_drop <- date_recreation_max + as.integer((date_recreation_min - date_recreation_max)/2)
    # # alternate method:
    # ind <- df_google$recreation_roll <= (recreation_max+recreation_min)/2; ind[is.na(ind)] <- F
    # date_recreation_mid_drop_v2 <- min(df_google$date[ind])
    
    # if (nrow(df_google) > 0) {
    #   browser()
    # }
    
    if (sum(!is.na(df_google$recreation_roll)) > 0) {
      i_max <- which.max(df_google$recreation_roll)
      i_min <- which.min(df_google$recreation_roll)
      max <- df_google$recreation_roll[i_max]
      min <- df_google$recreation_roll[i_min]
      ind <- df_google$recreation_roll <= (min+max)/2 & df_google$date >= df_google$date[i_max]; ind[is.na(ind)] <- F
      date_recreation_mid <- min(df_google$date[ind])
      # plot(df_google$date, df_google$recreation_roll, type="l"); abline(v = date_recreation_mid)
    } else {
      date_recreation_mid <- NA
    }
    
    if (sum(!is.na(df_google$grocery_roll)) > 0) {
      i_max <- which.max(df_google$grocery_roll)
      i_min <- which.min(df_google$grocery_roll)
      max <- df_google$grocery_roll[i_max]
      min <- df_google$grocery_roll[i_min]
      ind <- df_google$grocery_roll <= (min+max)/2 & df_google$date >= df_google$date[i_max]; ind[is.na(ind)] <- F
      date_grocery_mid <- min(df_google$date[ind])
      # lines(df_google$date, df_google$grocery_roll,col="lightgreen"); abline(v = date_grocery_mid,col="lightgreen")
    } else {
      date_grocery_mid <- NA
    }
    
    if (sum(!is.na(df_google$parks_roll)) > 0) {
      i_max <- which.max(df_google$parks_roll)
      i_min <- which.min(df_google$parks_roll)
      max <- df_google$parks_roll[i_max]
      min <- df_google$parks_roll[i_min]
      ind <- df_google$parks_roll <= (min+max)/2 & df_google$date >= df_google$date[i_max]; ind[is.na(ind)] <- F
      date_parks_mid <- min(df_google$date[ind])
      # lines(df_google$date, df_google$parks_roll,col="green"); abline(v = date_parks_mid,col="green")
    } else {
      date_parks_mid <- NA
    }
    
    if (sum(!is.na(df_google$transit_roll)) > 0) {
      i_max <- which.max(df_google$transit_roll)
      i_min <- which.min(df_google$transit_roll)
      max <- df_google$transit_roll[i_max]
      min <- df_google$transit_roll[i_min]
      ind <- df_google$transit_roll <= (min+max)/2 & df_google$date >= df_google$date[i_max]; ind[is.na(ind)] <- F
      date_transit_mid <- min(df_google$date[ind])
      # lines(df_google$date, df_google$transit_roll,col="cyan"); abline(v = date_transit_mid,col="cyan")
    } else {
      date_transit_mid <- NA
    }
    
    if (sum(!is.na(df_google$workplaces_roll)) > 0) {
      i_max <- which.max(df_google$workplaces_roll)
      i_min <- which.min(df_google$workplaces_roll)
      max <- df_google$workplaces_roll[i_max]
      min <- df_google$workplaces_roll[i_min]
      ind <- df_google$workplaces_roll <= (min+max)/2 & df_google$date >= df_google$date[i_max]; ind[is.na(ind)] <- F
      date_workplaces_mid <- min(df_google$date[ind])
      # lines(df_google$date, df_google$workplaces_roll,col="orange"); abline(v = date_workplaces_mid,col="orange")
    } else {
      date_workplaces_mid <- NA
    }
    
    
    # NOTE: residential is reversed (rise not drop)
    if (sum(!is.na(df_google$residential_roll)) > 0) {
      i_max <- which.max(df_google$residential_roll)
      i_min <- which.min(df_google$residential_roll)
      max <- df_google$residential_roll[i_max]
      min <- df_google$residential_roll[i_min]
      ind <- df_google$residential_roll >= (min+max)/2 & df_google$date >= df_google$date[i_min]; ind[is.na(ind)] <- F
      date_residential_mid <- min(df_google$date[ind])
      # lines(df_google$date, df_google$residential_roll,col="brown"); abline(v = date_residential_mid,col="brown")
    } else {
      date_residential_mid <- NA
    }
    if (sum(!is.na(df_google$mean_rec_groc_trans_work)) > 0) {
      i_max <- which.max(df_google$mean_rec_groc_trans_work)
      i_min <- which.min(df_google$mean_rec_groc_trans_work)
      max <- df_google$mean_rec_groc_trans_work[i_max]
      min <- df_google$mean_rec_groc_trans_work[i_min]
      ind <- df_google$mean_rec_groc_trans_work <= (min+max)/2 & df_google$date >= df_google$date[i_max]; ind[is.na(ind)] <- F
      date_mean_rec_groc_trans_work_mid <- min(df_google$date[ind])
      # lines(df_google$date, df_google$residential_roll,col="brown"); abline(v = date_residential_mid,col="brown")
      max_of_mean_rec_groc_trans_work <- max 
      min_of_mean_rec_groc_trans_work <- min
    } else {
      date_mean_rec_groc_trans_work_mid <- NA
      max_of_mean_rec_groc_trans_work <- NA 
      min_of_mean_rec_groc_trans_work <- NA
    }
    # # Min, max of mean (to output):
    # max_of_mean_rec_groc_trans_work <- max 
    # min_of_mean_rec_groc_trans_work <- min
    
    to_return <- list("df"=df_google,
                      "date_recreation_mid" = date_recreation_mid,
                      "date_grocery_mid" = date_grocery_mid,
                      "date_parks_mid" = date_grocery_mid,
                      "date_transit_mid" = date_transit_mid,
                      "date_workplaces_mid" = date_workplaces_mid,
                      "date_residential_mid" = date_residential_mid,
                      "date_mean_rec_groc_trans_work_mid" = date_mean_rec_groc_trans_work_mid,
                      "max_of_mean_rec_groc_trans_work" = max_of_mean_rec_groc_trans_work,
                      "min_of_mean_rec_groc_trans_work" = min_of_mean_rec_groc_trans_work
    )
  } else {
    to_return <- list("df"=df_google,
                      "date_recreation_mid" = NA,
                      "date_grocery_mid" = NA,
                      "date_parks_mid" = NA,
                      "date_transit_mid" = NA,
                      "date_workplaces_mid" = NA,
                      "date_residential_mid" = NA,
                      "date_mean_rec_groc_trans_work_mid" = NA,
                      "max_of_mean_rec_groc_trans_work" = NA,
                      "min_of_mean_rec_groc_trans_work" = NA
    )
  }
  
  
  
  return(to_return)
}