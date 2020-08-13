
ui <- fluidPage(
  titlePanel("covFit"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countrySelect", "Country", choices=unique(df$Country_Region)),
      selectInput("regionSelect", "Province/State", choices = NULL),
      selectInput("subregionSelect", "Subregion", choices = NULL)
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
  
)