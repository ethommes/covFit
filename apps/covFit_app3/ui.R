
ui <- fluidPage(
  titlePanel("covFit"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("trial_sites_only", "Restrict to trial sites", value=FALSE),
      selectInput("countrySelect", "Country", choices=unique(df$Country_Region)),
      selectInput("regionSelect", "Province/State", choices = NULL),
      selectInput("subregionSelect", "Subregion", choices = NULL),
      selectInput("siteSelect", "Site", choices = NULL),
      dateInput("predict_from_date", "Forecast from:"),
      dateInput("predict_date", "Forecast to:"),
      sliderInput("R_window_size", "rolling average window (days)", min=3, max=30, value = 14),
      checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE),
      numericInput("CFR", "CFR to enforce (10 day lag)", value = 0.004, min=0, max=0.1, step=0.001),
      checkboxInput("manual_y_scale", "Manual incidence (y) axis scaling", value=FALSE),
      numericInput("manual_max_incidence", "Max incidence to plot", value=100, min=1, max=1e4, step=1),
      width=2
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
  
)