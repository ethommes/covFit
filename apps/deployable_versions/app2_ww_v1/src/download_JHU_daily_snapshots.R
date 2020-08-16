
download_JHU_daily_snapshots <- function(start_date, end_date) {
  # Data comes from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
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
  }
  
}