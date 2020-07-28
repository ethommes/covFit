

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



covid19 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid19_deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
covid19_US <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
covid19_deaths_US <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
# # Read in data file with FIPS codes and county land areas.
# # This is a US Census data file referenced by Wikipedia: https://en.wikipedia.org/wiki/County_statistics_of_the_United_States#cite_note-2010_qf_dataset-13
# #  - Description: https://web.archive.org/web/20150807220054/http://quickfacts.census.gov/qfd/download_data.html
# #  - Data dictionary: https://web.archive.org/web/20150821061818/http://quickfacts.census.gov/qfd/download/DataDict.txt
# #  - The actual file URL: https://web.archive.org/web/20130930014430/http://quickfacts.census.gov/qfd/download/DataSet.txt
fips_and_land_area_table <- read.csv("inputs/DataSet_with_county_areas.csv")
fips <- make_fips_table()

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
    cfr_observed = NA, # If not specified, is calculated and applied for a given region 
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
    plot_R_and_mobility_TF = F
)
countries_vector <- covid19$Country.Region

ui <- fluidPage(

    # Application title
    titlePanel("covFit: incidence rolling average and forecast"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("countrySelect", "Country", choices=countries_vector),
            dateInput("predict_from_date", "Forecast from:"),
            dateInput("predict_date", "Forecast to:"),
            sliderInput("R_window_size", "rolling average window (days)", min=3, max=30, value = 14),
            numericInput("CFR", "CFR to enforce (10 day lag)", value = 0.004, min=0, max=0.1),
            width=3
        ),
      
        mainPanel(
          plotOutput("Plot")
           # plotOutput("distPlot", width = "500px", height="500px")
          # plotOutput("distPlot", width="75%")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    # inputs <- base_input
    # inputs$Country.Region <- countryInput
    # browser()

    # plot_to_render <- reactive({
    #     input_list <- base_input_list
    #     input_list$Country.Region <- countrySelect
    #     browser()
    # })
    
    output$Plot <- renderPlot({
        
        inp <- base_input_list
        inp$plot_R_and_mobility_TF <- F
        inp$plot_only_linear_TF <- F
        inp$predict_from_date <- input$predict_from_date
        inp$predict_date <- input$predict_date
        inp$R_window_size <- input$R_window_size
        # if (input$CFR > 0) {
        #   inp$CFR <- input$CFR
        # } else {
        #   inp$CFR <- NA
        # }
        
        temp <- non_US_mobility_and_incidence(country = input$countrySelect,
                                      correction_factor = NA, # hard-code for now
                                      covid_data = covid19,
                                      covid_death_data = covid19_deaths,
                                      plot_only_linear_TF = F,
                                      base_input = inp)
        temp$plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
