

# library(shiny)
# library(maps)
# library(dplyr)
# library(ggplot2)
# library(ggalt)
# library(totalcensus) # Revisit if we actually end up using this...
# library(readxl)
# library(writexl)
# # library(acs)
# library(tidyverse)
# # library(tigris)
# # library(sf)
# library(noncensus)
# library(usmap)
# library(lubridate)
# library(data.table)
# library(ggrepel)
# library(grid)
# library(gridExtra)
# library(countrycode)
# library(ggpubr)
# 
# setwd("src")
# file.sources <- list.files(pattern="*.R$")
# sapply(file.sources, source)
# setwd("..")



# covid19 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
# covid19_deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
# covid19_US <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
# covid19_deaths_US <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
# # Read in data file with FIPS codes and county land areas.
# # This is a US Census data file referenced by Wikipedia: https://en.wikipedia.org/wiki/County_statistics_of_the_United_States#cite_note-2010_qf_dataset-13
# #  - Description: https://web.archive.org/web/20150807220054/http://quickfacts.census.gov/qfd/download_data.html
# #  - Data dictionary: https://web.archive.org/web/20150821061818/http://quickfacts.census.gov/qfd/download/DataDict.txt
# #  - The actual file URL: https://web.archive.org/web/20130930014430/http://quickfacts.census.gov/qfd/download/DataSet.txt
# fips_and_land_area_table <- read.csv("inputs/DataSet_with_county_areas.csv")
# fips <- make_fips_table()

# Read in Google mobility data:
# download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", destfile = paste0("inputs/Global_Mobility_Report_",Sys.Date(),".csv"))
# google_mobility <- read.csv(paste0("inputs/Global_Mobility_Report_",Sys.Date(),".csv"))

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
countries_vector <- covid19$Country.Region
# locations_vector <- covid19$Country.Region
# locations_vector <- NULL

ui <- fluidPage(
  
  # Application title
  titlePanel("covFit: COVID-19 incidence forecast"),
  # titlePanel("COVID-19 BigBunnyFit: \"It's better than nada!\""),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("world_or_US", label=NULL, choices=list("World"="world", "US only"="US")),
      selectInput("countrySelect", "Country", choices=NULL),
      # selectInput("countrySelect", "Country", choices=countries_vector),
      selectInput("regionSelect", "Region (province, state etc)", choices=NULL),
      selectInput("subregionSelect", "County (US only)", choices=NULL),
      dateInput("predict_from_date", "Forecast from:"),
      dateInput("predict_date", "Forecast to:"),
      sliderInput("R_window_size", "rolling average window (days)", min=3, max=30, value = 14),
      checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE),
      numericInput("CFR", "CFR to enforce (10 day lag)", value = 0.004, min=0, max=0.1, step=0.001),
      checkboxInput("manual_y_scale", "Manual incidence (y) axis scaling", value=FALSE),
      numericInput("manual_max_incidence", "Max incidence to plot", value=100, min=1, max=1e4, step=1),
      width=3
    ),
    
    mainPanel(
      plotOutput("Plot")
      # plotOutput("distPlot", width = "500px", height="500px")
      # plotOutput("distPlot", width="75%")
    )
  )
)
server <- function(input, output, session) {
  
  # Country dropdown choices are dependent on whether radio button is set to World or US:
  
  observe({
    if (input$world_or_US == "world") {
      countries_vector <- as.list(covid19$Country.Region)
      names(countries_vector) <- covid19$Country.Region
    } else {
      countries_vector <- list(US="US")
    }
    updateSelectInput(session = session, inputId = "countrySelect", choices = countries_vector)
  })
  
  # countries_vector <- reactive({
  #   if (input$world_or_US == "world") {
  #     # countries_vector <- covid19$Country.Region
  #     countries_vector <- as.list(covid19$Country.Region)
  #     names(countries_vector) <- covid19$Country.Region
  #   } else {
  #     countries_vector <- list(US="US")
  #   }
  # })
  # observe({
  #   choices <- countries_vector()
  #   updateSelectInput(session = session, inputId = "countrySelect", choices = choices)
  # })
  
  # Regions dropdown:
  regions_vector <- reactive({
    if (input$world_or_US == "world") {
      df_temp <- covid19 %>% subset(Country.Region == req(input$countrySelect))
      # regions_vector <- c(" " = "", df_temp$Province.State)
      # regions_vector <- c("ALL", regions_vector)
      regions_vector <- as.list(df_temp$Province.State)
      names(regions_vector) <- df_temp$Province.State
      regions_vector <- c(ALL = "", regions_vector)
      
    } else if (input$world_or_US == "US") {
      regions_vector <- covid19_US$Province_State
    }
  })
  observe({
    updateSelectInput(session = session, inputId = "regionSelect", choices = regions_vector())
  })
  
  # Subregions dropdown:
  subregions_vector <- reactive({
    if (input$world_or_US == "world") {
      subregions_vector <- c("") 
    } else if (input$world_or$US == "US") {
      df_temp <- covid19_US %>% subset(Province_State == input$regionSelect)
      subregions_vector <- df_temp$Admin2
    }
  })
  observe({
    updateSelectInput(session = session, inputId = "subregionSelect", choices = subregions_vector())
  })
  
  
  
  output$Plot <- renderPlot({
    if (input$world_or_US == "world") {
      temp <- non_US_mobility_and_incidence_v2(country = input$countrySelect,
                                               region = input$regionSelect,
                                               subregion = input$subregionSelect,
                                               correction_factor = NA, # hard-code for now
                                               covid_data = covid19,
                                               covid_death_data = covid19_deaths,
                                               plot_only_linear_TF = F,
                                               base_input = base_input_list)
      plot_to_render <- temp$plot
    } else if (input$world_or_US == "US") {
      inp <- base_input_list
      inp$covid_data <- covid19_US
      inp$covid_death_data <- covid19_deaths_US
      inp$Country.Region <- input$countrySelect
      inp$Province.State <- input$regionSelect
      inp$Admin2 <- input$subregionSelect
      inp$mobility_subregion <- input$subregionSelect
      temp <- region_mobility_and_incidence_v7(inp)
      
      plot_to_render <- temp$plot
    }
    
    plot_to_render
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
