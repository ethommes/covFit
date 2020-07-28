
make_fips_table <- function() {
  # This data file contains also the county name, allowing us to add this as a column [comment out if you want to read from local file]
  fips_raw <-
    read_csv(
      "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
      col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp")
    )
  
  # Alternatively, if you already have it, read from local file, uncomment below and comment out above:
  # fips_raw <- read_csv("../../US_Census_data/national_county.txt",col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp"))
  
  fips <- data.frame("FIPS"=as.numeric(paste(fips_raw$statefp,fips_raw$countyfp,sep="")),"county_full_name" = fips_raw$county.name, "state_abbr" = fips_raw$state.abb)

  return(fips)
}