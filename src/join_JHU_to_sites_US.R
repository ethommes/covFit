
join_JHU_to_sites_US <- function(df, input_path) {

  fullpath1 <- paste0(input_path,"us_cities_final.csv")
  fullpath2 <- paste0(input_path,"trial_sites.xlsx")
  # US_cities_with_FIPS <- read.csv("../inputs/from_Rob/us_cities_final.csv")
  US_cities_with_FIPS <- read.csv(fullpath1)
  # trial_sites <- read_excel("../../inputs/from_Rob/trial_sites.xlsx")
  trial_sites <- read_excel(fullpath2)
  trial_sites_trimmed <- trial_sites[,3:9]
  trial_sites_with_FIPS <- left_join(trial_sites_trimmed, US_cities_with_FIPS, by = c("Site city" = "Site.city", "Site State" = "Site.State"))
  
  df_US <- df %>% subset(FIPS != "")
  df_US$FIPS <- as.integer(df_US$FIPS)
  df_trial_sites_US <- left_join(trial_sites_with_FIPS, df_US, by = "FIPS" )
  return(df_trial_sites_US)
}