
fluidPage(
  sidebarPanel(
    checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE),
    dateInput("predict_from_date", "Forecast from:"),
    dateInput("predict_date", "Forecast to:"),
    sliderInput("R_window_size", "rolling average window (days)", min=3, max=30, value = 14),
    ),
  tabsetPanel(
    tabPanel("World", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("countrySelect", "Country", choices = unique(covid19$Country.Region))
                            ),
               mainPanel()
             )
      
    ),
    tabPanel("US", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("stateSelect", "State", choices = unique(covid19_US$Province_State))
                            ),
                            # checkboxInput("apply_CFR_correction", "Apply CFR correction", value=FALSE)),
               mainPanel()
             )
      
    )
  )
)

# fluidPage(
#   tabsetPanel(
#     tabPanel("World", fluid = TRUE,
#              sidebarLayout(
#                sidebarPanel(selectInput("countrySelect", "Country", choices = unique(covid19$Country.Region))
#              ),
#              mainPanel())
#   )
# )