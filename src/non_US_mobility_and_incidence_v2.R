
non_US_mobility_and_incidence_v2 <- function(country, region, subregion, correction_factor, covid_data, covid_death_data, plot_only_linear_TF, base_input) {
    # correction_factor (>1) multiplies observed incidence to correct for under-reporting
  # inputs is a list
  # NOTE: will not work if mobility data and JHU data use a different country name format (e.g. "South Korea"
  # vs. "Korea, South")
  Country.Region <- country
  Province.State <- region
  Admin2 <- subregion
  mobility_country <- Country.Region
  
  # Hard-coded: Special exceptions in which mobility_country != Country.Region
  if (Country.Region=="US") mobility_country <- "United States"
  
  mobility_region <- Province.State
  mobility_subregion <- Admin2
  
  data("world_bank_pop")
  country_code <- countrycode(Country.Region, origin="country.name", destination="wb")
  pop_line <- world_bank_pop %>% subset(country==country_code) %>% subset(indicator=="SP.POP.TOTL")
  pop <- pop_line$`2017`
  
  input <- base_input
  input$covid_data <- covid_data
  input$covid_death_data <- covid_death_data
  input$Country.Region <- Country.Region
  input$Province.State <- Province.State
  input$Admin2 <- Admin2
  input$pop <- pop
  input$mobility_country <- mobility_country
  input$mobility_region <- Province.State
  input$mobility_subregion = Admin2
  # input$CFR = 1 # setting to NA means no CFR adjustment is made
  input$cfr_correction_factor <- correction_factor  
  input$lags_vector = c(10) 
  input$plot_cfr_lags_TF <- F
  # input$plot_TF <- T
  # input$plot_only_linear_TF <- plot_only_linear_TF
  temp <- region_mobility_and_incidence_v7(input)
  return(temp)
}