
fluidPage(
  fluidRow(
    column(2,
           checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE),
           numericInput("CFR", "CFR to enforce (10 day lag)", value = 0.004, min=0, max=0.1, step=0.001)
           ),
    column(2,
           dateInput("predict_from_date", "Forecast from:"),
           dateInput("predict_date", "Forecast to:", value = "2020-10-01")
           ),
    column(2,
           checkboxInput("manual_y_scale", "Manual incidence (y) axis scaling", value=FALSE),
           numericInput("manual_max_incidence", "Max incidence to plot", value=100, min=1, max=1e4, step=1)
           ),
    column(3,
           sliderInput("R_window_size", "rolling average window (days)", min=3, max=30, value = 14)
    )
  ),
  # sidebarPanel(
  #   checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE),
  #   dateInput("predict_from_date", "Forecast from:"),
  #   dateInput("predict_date", "Forecast to:"),
  #   sliderInput("R_window_size", "rolling average window (days)", min=3, max=30, value = 14),
  #   ),
  tabsetPanel(
    tabPanel("World", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("countrySelect", "Country", choices = unique(covid19$Country.Region)),
                            width=2
                            ),
               mainPanel(plotOutput("world_plot"))
             )
      
    ),
    tabPanel("US", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("stateSelect", "State", choices = unique(covid19_US$Province_State)),
                            selectInput("countySelect", "County", choices = NULL),
                            width=2
                            ),
                            # checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE)),
               mainPanel(plotOutput("US_plot"))
             )
      
    ),
    tabPanel("Brazil", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("stateSelectBR", "State", choices = NULL),
                            width=2
               ),
               # checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE)),
               mainPanel(plotOutput("BR_plot"))
             )
             
    ),
    tabPanel("South Africa", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("stateSelectSA", "State", choices = NULL),
               ),
               # checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE)),
               mainPanel(plotOutput("SA_plot"))
             )
             
    )
  )
)

