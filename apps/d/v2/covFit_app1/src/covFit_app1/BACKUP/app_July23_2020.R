

library(shiny)
covid19 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid19_deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
covid19_US <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
covid19_deaths_US <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
# Read in data file with FIPS codes and county land areas.
# This is a US Census data file referenced by Wikipedia: https://en.wikipedia.org/wiki/County_statistics_of_the_United_States#cite_note-2010_qf_dataset-13
#  - Description: https://web.archive.org/web/20150807220054/http://quickfacts.census.gov/qfd/download_data.html
#  - Data dictionary: https://web.archive.org/web/20150821061818/http://quickfacts.census.gov/qfd/download/DataDict.txt
#  - The actual file URL: https://web.archive.org/web/20130930014430/http://quickfacts.census.gov/qfd/download/DataSet.txt
fips_and_land_area_table <- read.csv("../../../inputs/DataSet_with_county_areas.csv")
fips <- make_fips_table()

# Read in Google mobility data:
# download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", destfile = paste0("../Global_Mobility_Report_",Sys.Date(),".csv"))
# google_mobility <- read.csv(paste0("../Global_Mobility_Report_",Sys.Date(),".csv"))

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
    plot_to_screen_TF = F, # plot output to screen?
    first_county_index = NA, 
    last_county_index = NA,
    rolling_mean_alignment = "center", 
    cfr_correction_factor = NA,
    filetype = "png",
    post_turnover_up_to_present_TF = F,
    plot_R_and_mobility_TF = T
)
countries_vector <- covid19$Country.Region

ui <- fluidPage(

    # Application title
    titlePanel("[]"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("countrySelect", "Country", choices=countries_vector),
            sliderInput("bins", 
                        "Number of bins:", 
                        min = 3,
                        max = 100, 
                        value = 10)
            
        ),
      
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
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
    
    output$distPlot <- renderPlot({
        

        Country.Region <- input$countrySelect
        
        # FOR TEST: hardwire a bunch of stuff
        Province.State <- ""
        Admin2 <- ""
        mobility_subregion <- ""
        
        input_list <- base_input_list
        
        input_list$covid_data <- covid19
        input_list$covid_death_data <-covid19_deaths
        input_list$plot_only_linear_TF = F
        input_list$pathname_figs = "../figs/DEMO"
        input_list$Country.Region = Country.Region
        input_list$Province.State = Province.State
        input_list$Admin2 = Admin2
        input_list$mobility_country = Country.Region
        input_list$mobility_region = Province.State
        input_list$mobility_subregion = mobility_subregion
        input_list$lags_vector = c(10,0,5,10,15,20,30)
        input_list$plot_cfr_lags_TF <- F
        input_list$mobility_window_half_width = 14
        input_list$rolling_mean_alignment = "center"
        input_list$R_window_size = 14
        input_list$most_recent_R_window = 60 # number of most recent days to use in computing latest R
        input_list$plot_TF <- T # save plots (as .png files)?
        input_list$plot_to_screen_TF <- T
        input_list$predict_from_date <- as.Date("2020-05-01")
        input_list$predict_date = as.Date("2020-09-01") # Date to which we're projecting
        

        output_list <- region_mobility_and_incidence_v7(input_list)
        output_list$plot
        
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
