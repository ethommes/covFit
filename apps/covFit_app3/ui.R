
ui <- fluidPage(
  titlePanel("covFit"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("trial_sites_only", "Restrict to trial sites", value=FALSE),
      selectInput("countrySelect", "Country", choices=unique(df_master$Country_Region)),
      selectInput("regionSelect", "Province/State", choices = NULL),
      selectInput("subregionSelect", "Subregion", choices = NULL),
      selectInput("siteSelect", "Site", choices = NULL),
      dateInput("predict_data_start", "Forecast using data window from...", value = as.Date("2020-01-23")),
      dateInput("predict_from_date", "...to", value = today()),
      dateInput("predict_date", "Forecast up to:", value=as.Date(today() + 30)),
      # numericInput("y_offset", "y offset", min=-10, max=10, value = 0),
      # numericInput("y_scale", "y scale", min=1e-6, max = 1e6, value = 1),
      # numericInput("t_lag", "time lag", min = -30, max = 30, value = 0),
      sliderInput("R_window_size", "rolling average window (days)", min=3, max=30, value = 14),
      checkboxInput("apply_CFR_correction", "Apply CFR correction", value=TRUE),
      numericInput("CFR", "CFR to enforce (10 day lag)", value = 0.004, min=0, max=0.1, step=0.001),
      radioButtons("incidencePer", "Show incidence as:", choices = c("per day" = "raw", 
                                                                     "per day per 1k" = "per1k", 
                                                                     "per day per 10k" = "per10k", 
                                                                     "per day per 100k" = "per100k"),
                   selected = "per100k"
      ),
      checkboxInput("manual_y_scale", "Manual incidence (y) axis scaling", value=FALSE),
      numericInput("manual_max_incidence", "Max incidence to plot", value=100, min=1, max=1e4, step=1),
      downloadButton('downloadPlot', 'Download Plot'),
      width=2
    ),
    mainPanel(
      plotOutput("Plot", height="800px")
    )
  )
  
)