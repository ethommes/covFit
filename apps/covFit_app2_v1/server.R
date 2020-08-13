
server <- function(input, output, session) {
  regions_vector <- reactive({
      df_tmp <- df %>% subset(Country_Region == input$countrySelect)
      regions_vector <- c("ALL", unique(df_tmp$Province_State))
  })
  observe({
    updateSelectInput(session = session, inputId = "regionSelect", choices = regions_vector())
  })
  
  subregions_vector <- reactive({
    df_tmp <- df %>% subset(Country_Region == input$countrySelect) %>% subset(Province_State == input$regionSelect)
    subregions_vector <- c("ALL", unique(df_tmp$Admin2))
  })
  observe({
    updateSelectInput(session = session, inputId = "subregionSelect", choices = subregions_vector())
  })
  
  output$Plot <- renderPlot({
    inp <- base_input_list
    inp$Country_Region <- input$countrySelect
    
    # Set province/state:
    if (input$regionSelect == "ALL" | is.na(input$regionSelect)) {
      inp$Province_State <- ""
    } else {
      inp$Province_State <- input$regionSelect
    }
    
    # Set US county, if applicable
    if (input$subregionSelect == "ALL" | is.na(input$subregionSelect) ) {
      inp$Admin2 <- ""
    } else {
      inp$Admin2 <- input$subregionSelect
    }
    
    # Pull out the data for the selected region:
    
    inp <- make_incidence_frame_ww(df, inp)
    # inp$region_data <- region_data
    
    # inp$region_data <- region_data
    temp <- region_mobility_and_incidence_ww(inp)
    
    # plot_to_render <- plot.new()
    # plot_to_render
    plot_to_render <- temp$plot
    plot_to_render
  })
}

