
augment_trial_site_sheet <- function(covid19_US,
                                     covid19_deaths_US,
                                     trial_sites_raw) {
  
  # Data from `noncensus` package, for mapping ZIP to FIPS:
  data("zip_codes")
  
  # This data file contains also the county name, allowing us to add this as a column:
  fips_raw <-
    read_csv(
      "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
      col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp")
    )
  fips <- data.frame(fips_raw,"fips"=paste(fips_raw$statefp,fips_raw$countyfp,sep=""))

  # Remove any lines from the trial sites sheet that have missing values for "City" and/or "State":
  trial_sites <- trial_sites_raw[complete.cases(trial_sites_raw$Zip),]
  n_sites <- nrow(trial_sites)
  
  # Attach county, placename and FIPS columns:
  trial_sites_augm <- data.frame(trial_sites,
                                 "city_from_zip" = rep(NA,n_sites),
                                 "counties" = rep(NA,n_sites), 
                                 "FIPS" = rep(NA,n_sites),
                                 "state_abbr" = rep(NA,n_sites)
  )
  for (i in 1:n_sites) {
    zip <- as.character(trial_sites_augm$Zip[i])
    zip_codes_line <- zip_codes[which(zip_codes$zip == zip),]
    fips_line <- fips[which(as.numeric(fips$fips) == zip_codes_line$fips),]
    if (nrow(zip_codes_line) == 1) {
      trial_sites_augm$city_from_zip[i] <- zip_codes_line$city
      trial_sites_augm$FIPS[i] <- zip_codes_line$fips
    }
    if (nrow(fips_line) == 1) {
      trial_sites_augm$counties[i] <- fips_line$county.name
      trial_sites_augm$state_abbr[i] <- fips_line$state.abb
    }
    # } else {
    #   print(trial_sites_augm[i,])
    # }
    # fips_line <- fips_line[which(fips$fips == ),]
    
    
  }


  # cty <- counties(cb = TRUE) %>%
  #   select(cty_id = GEOID, cty_name = NAME)
  # 
  # zc <- zctas(cb = TRUE)
  # 
  # zipcty <- st_join(zc, cty)
  # 
  # # Remove any lines from the trial sites sheet that have missing values for "City" and/or "State":
  # trial_sites <- trial_sites_raw[complete.cases(trial_sites_raw$Zip),]
  # 
  # # Attach county, placename and FIPS columns.  We will allow for up to 8 different counties
  # # per ZIP:
  # trial_sites_augm <- data.frame(trial_sites,
  #                                "State_fullname" = rep(NA,n_sites),
  #                                "County1" = rep(NA,n_sites), 
  #                                "County2" = rep(NA,n_sites),
  #                                "County3" = rep(NA,n_sites),
  #                                "County4" = rep(NA,n_sites),
  #                                "County5" = rep(NA,n_sites),
  #                                "County6" = rep(NA,n_sites),
  #                                "County7" = rep(NA,n_sites),
  #                                "County8" = rep(NA,n_sites),
  #                                "fips1" = rep(NA,n_sites),
  #                                "fips2" = rep(NA,n_sites),
  #                                "fips3" = rep(NA,n_sites),
  #                                "fips4" = rep(NA,n_sites),
  #                                "fips5" = rep(NA,n_sites),
  #                                "fips6" = rep(NA,n_sites),
  #                                "fips7" = rep(NA,n_sites),
  #                                "fips8" = rep(NA,n_sites)
  #                                # "place.name" = rep(NA,n_sites)
  #                                )
  # # Get index of "County1" column:
  # index_County1 <- grep("County1", colnames(trial_sites_augm))
  # # Get index of "fips1" column:
  # index_fips1 <- grep("fips1", colnames(trial_sites_augm))
  # browser()
  # for (i in 1:n_sites) {
  #   zip <- as.character(trial_sites_augm$Zip[i])
  #   zipcty_block <- zipcty[which(zipcty$ZCTA5CE10 == zip),]
  #   if (length(zipcty_block$cty_id) > 0 & length(zipcty_block$cty_name) > 0) {
  #     # if (nrow(zipcty_line) >= 7) {browser()}
  #     n_counties <- nrow(zipcty_block)
  #     for (j in 1:n_counties) {
  #       trial_sites_augm[i, (index_County1 + j - 1)] <- zipcty_block$cty_name[j]
  #       trial_sites_augm[i, (index_fips1 + j - 1)] <- zipcty_block$cty_id[j]
  #     }
  #   } else {browser()}
  # }
  return(trial_sites_augm)
  
  
  
  # ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
  # temp <- tempfile()
  # download.file(ZipCodeSourceFile , temp)
  # ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
  # unlink(temp)
  # df <- data.frame("zip" = ZipCodes$V2, "place" = ZipCodes$V3, "state.abbr" = ZipCodes$V5)
  # 
  # # Make the states and fips frames
  # states <- cbind(state.name, state.abb) %>% tbl_df()
  # fips <-
  #   read_csv(
  #     "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
  #     col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp")
  #   )
  # 
  # 
  # # Attach county, placename and FIPS columns:
  # n_sites <- nrow(trial_sites)
  # trial_sites_augm <- data.frame(trial_sites,
  #                                "State_fullname" = rep(NA,n_sites),
  #                                "County" = rep(NA,n_sites), 
  #                                "fips" = rep(NA,n_sites),
  #                                "place.name" = rep(NA,n_sites))
  # 
  # for (i in 1:n_sites) {
  #   zip <- trial_sites_augm$Zip[i]
  #   zipcode_to_fips_output <- zipcode_to_fips(zip,df,fips)
  #   trial_sites_augm$County[i] <- zipcode_to_fips_output$County
  # } 
  # browser()
  
  
  
  
  # for (i in 1:n_sites) {
  #   zip <- trial_sites_augm$Zip[i]
  #   zipcty_line <- zipcty[which(zipcty$ZCTA5CE10 == zip),]
  #   if (nrow(zipcty_line) > 1) {browser()}
  #   if (length(zipcty_line$cty_id) > 0 & length(zipcty_line$cty_name) > 0 & nrow(zipcty_line) == 1) {
  #     trial_sites_augm$County[i] <- zipcty_line$cty_name
  #     trial_sites_augm$fips[i] <- zipcty_line$cty_id
  #   }
  # }
}