
combine_JHU_daily_snapshots <- function(start_date, end_date) {
  # Initialize data frame into which we'll stuff all the snapshots:
  df <- data.frame(
    Date = as.Date(character()),
    FIPS = character(),
    Admin2 = character(),
    Country_Region = character(),
    Last_Update = dmy_hm(),
    Lat = double(),
    Long_ = double(),
    Confirmed = integer(),
    Deaths = integer(),
    Recovered = integer(),
    Active = integer(),
    "Combined_Key" = character(),
    "Incidence_Rate" = double(),
    "Case-Fatality_Ratio" = double(),
    "Population" = integer()
  )
  
  n_days <- end_date - start_date 
  for (i in 0:n_days) {
    snapshot_date <- start_date + i 
    print(snapshot_date)
    snapshot_date_reformat <- format(snapshot_date, "%m-%d-%Y")
    snapshot_filename <- paste0(snapshot_date_reformat, ".csv")
    snapshot_fullpath <- paste0("../inputs/JHU_daily_snapshots/", snapshot_filename)
    if (!file.exists(snapshot_fullpath)) {
      # print(paste(snapshot_fullpath, "doesn't exist"))
      snapshot_URL <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", snapshot_filename)
      # print(snapshot_URL)
      download.file(snapshot_URL, destfile = snapshot_fullpath)
    }
    snapshot_frame <- read.csv(snapshot_fullpath)
    browser()
    
    # Earlier versions did not have the last two columns.  If so, add them as NA
    # (NOTE: in future see if we can compute missing incidence and CFR for a given 
    # region using later values of daily incidence and CFR, combined with daily cases):
    if (ncol(snapshot_frame) == 12) {
      snapshot_frame <- data.frame(snapshot_frame, "Incidence_Rate" = NA, "Case-Fatality_Ratio" = NA)
    }
    # Add on the date as the first column:
    snapshot_frame <- data.frame(snapshot_date,snapshot_frame)
    df <- rbind(df, snapshot_frame)
  }
  
  # Back-calculate the population for each region, using the last snapshot 
  # (needs to be one of the ones with the newer 14-column format that includes
  # incidence rate and CFR):
  df_for_pop <- snapshot_frame
  df_for_pop <- data.frame(df_for_pop, "Population" = df_for_pop$Confirmed*1e5/df_for_pop$Incidence_Rate)
  df_for_pop <- data.frame("Admin2" = df_for_pop$Admin2,
                           "Province_State" = df_for_pop$Province_State,
                           "Country_Region" = df_for_pop$Country_Region,
                           "Population" = df_for_pop$Population)
  df <- left_join(df, df_for_pop)
  
  # Go back and calculate incidence rate and CFR in order to fill it in 
  # for the earlier dates that didn't have these two columns.  For simplicity,
  # we'll just recalculate it for ALL the entries:
  df$Incidence_Rate <- df$Confirmed*1e5/df$Population
  df$Case.Fatality_Ratio <- df
  

  # snapshot_for_pop <- df %>% subset(Date == as.Date("2020-08-01"))
  

  
  return(df)
}