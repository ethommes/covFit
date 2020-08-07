

# Set the base inputs:
base_input_list <- list(
  pathname_figs = "../figs/DEMO",
  pathname_output = "../figs/DEMO",
  covid_data = covid19_US,
  covid_death_data = covid19_deaths_US,
  fips = fips,
  CFR = 0.004, # CDC best estimate, https://www.cdc.gov/coronavirus/2019-ncov/hcp/planning-scenarios.html
  cfr_observed = NA, # If not specified, is calculated and applied for a given region,
  lags_vector = c(10,0,5,10,15,20,30),
  plot_cfr_lags_TF = F,
  cases_per_100k_threshold = 0.1, # threshold for what we consider onset of outbreak 
  # google_mobility = google_mobility, # the data frame we read in from google above
  start_date = as.Date("2020-01-22"), # first date of JHU data
  pop = NA, # have to provide if not US; if US, it's provided in the deaths data
  Country.Region = NA,
  Province.State = NA,
  Admin2 = NA, # US county; not used if doing state-level or (for time being) another country
  mobility_country = NA,
  mobility_region = NA,
  mobility_subregion = NA,
  sigma_SEIR = 1/2.5, # SEIR model parameter
  gamma_SEIR = 1/2.5, # SEIR model parameter
  onset_date_override = NA, # this and next: if we need to over-ride what code picks
  turnover_date_override = NA,
  interval_type = "confidence", 
  mobility_window_half_width = 7, # rolling average window (days) for mobility data.  Misnomer; is now actually full width of window
  R_window_size = 14, # rolling average window for R
  most_recent_R_window = 30, #use this many of the most recent days when computing current R
  predict_from_date = as.Date("2020-12-31"), # date from which we're predicting.  Allows checking what are prediction would have been in the past.  Any date >= the current date makes prediction go from the current date
  predict_date = as.Date("2020-09-01"), # Date to which we're projecting
  xlim = c(as.Date("2020-03-01"), as.Date("2020-05-01")),
  total_case_threshold_for_turnover_calc = 1000, # INACTIVE
  plot_only_linear_TF = F, # don't plot the logarithmic panel?
  plot_TF = F, # Write plots for each region (to .png files)?
  plot_to_screen_TF = T, # plot output to screen?
  first_county_index = NA, 
  last_county_index = NA,
  rolling_mean_alignment = "center", 
  cfr_correction_factor = NA,
  filetype = "png",
  post_turnover_up_to_present_TF = F,
  plot_R_and_mobility_TF = F,
  manual_max_incidence = NA
)

ui <- fluidPage(
  titlePanel("covFit: COVID-19 incidence forecast"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("world_or_US", label=NULL, choices=list("World"="world", "US only"="US")),
      selectInput("countrySelect", "Country", choices = unique(covid19$Country.Region)),
      selectInput("regionSelect", "Region (province, state, etc.)", choices = NULL),
      
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
)

server <- function(input, output, session) {
  input_ready <- F
  observe({
    if (input$world_or_US == "world") {
      country_choices_list <- unique(covid19$Country.Region)
      df_temp <- covid19 %>% subset(Country.Region == country_choices_list[1])
      region_choices_list <- unique(df_temp$Province.State)
    } else if (input$world_or_US == "US") {
      country_choices_list <- c("US")
      region_choices_list <- unique(covid19_US$Province_State)
    }
    updateSelectInput(session, "countrySelect", choices = country_choices_list, selected = country_choices_list[1])
    updateSelectInput(session, "regionSelect", choices = region_choices_list, selected = region_choices_list[1])
  })
  
  # observe({
  #   if (input$world_or_US == "world") {
  #     df_temp <- covid19 %>% subset(Country.Region == req(input$countrySelect))
  #     region_choices_list <- unique(df_temp$Province.State)
  #   } else if (input$world_or_US == "US") {
  #     region_choices_list <- unique(covid19_US$Province_State)
  #   }
  #   updateSelectInput(session, "regionSelect", choices=region_choices_list)
  # })
  
  
  
  output$Plot <- renderPlot({
    if (isolate(input$world_or_US) == "world") {
      temp <- non_US_mobility_and_incidence_v2(country = input$countrySelect,
                                               region = input$regionSelect,
                                               subregion = "",
                                               correction_factor = NA, # hard-code for now
                                               covid_data = covid19,
                                               covid_death_data = covid19_deaths,
                                               plot_only_linear_TF = F,
                                               base_input = base_input_list)
      plot_to_render <- temp$plot
    } else if (isolate(input$world_or_US) == "US") {
      inp <- base_input_list
      inp$covid_data <- covid19_US
      inp$covid_death_data <- covid19_deaths_US
      inp$Country.Region <- input$countrySelect
      inp$Province.State <- input$regionSelect
      inp$Admin2 <- ""
      inp$mobility_subregion <- ""
      temp <- region_mobility_and_incidence_v7(inp)
      
      plot_to_render <- temp$plot
    }
    plot_to_render
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
