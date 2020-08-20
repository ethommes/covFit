
function(input, output, session) {
  counties_vector <- reactive({
    df_temp <- covid19_US %>% subset(Province_State == input$stateSelect)
    counties_vector <- c("ALL", df_temp$Admin2)
  })
  observe({
    updateSelectInput(session = session, inputId = "countySelect", choices = counties_vector())
  })
  
  # inp_common <- reactive({
  #   # tmp <- input
  #   tmp2 <- base_input_list
  #   # inp_common <- base_input_list
  #   tmp2$plot_R_and_mobility_TF <- F
  #   inp_common <- tmp2
  #   
  # })
  
  inp_common <- reactive({
    tmp <- base_input_list
    tmp$plot_R_and_mobility_TF <- F
    tmp$plot_only_linear_TF <- F
    tmp$predict_from_date <- input$predict_from_date
    tmp$predict_date <- input$predict_date
    tmp$R_window_size <- input$R_window_size
    if (input$manual_y_scale) {
      tmp$manual_max_incidence <- input$manual_max_incidence
    } else {
      tmp$manual_max_incidence <- NA
    }
    if (input$apply_CFR_correction) {
      tmp$CFR <- tmp$CFR
    } else {
      tmp$CFR <- NA
    }
    inp_common <- tmp
  })
  
  output$world_plot <- renderPlot({
    inp <- inp_common()
    # browser()
    # # inp <- base_input_list
    # inp$plot_R_and_mobility_TF <- F
    # inp$plot_only_linear_TF <- F
    # inp$predict_from_date <- input$predict_from_date
    # inp$predict_date <- input$predict_date
    # inp$R_window_size <- input$R_window_size
    # if (input$manual_y_scale) {
    #   inp$manual_max_incidence <- input$manual_max_incidence
    # } else {
    #   inp$manual_max_incidence <- NA
    # }
    # if (input$apply_CFR_correction) {
    #   inp$CFR <- input$CFR
    # } else {
    #   inp$CFR <- NA
    # }
    temp <- non_US_mobility_and_incidence_v2(country = input$countrySelect,
                                             region = "",
                                             subregion = "",
                                             correction_factor = NA, # hard-code for now
                                             covid_data = covid19,
                                             covid_death_data = covid19_deaths,
                                             plot_only_linear_TF = F,
                                             base_input = inp)
    temp$plot
  })
  
  output$US_plot <- renderPlot({
    inp <- inp_common()
    # inp <- base_input_list
    inp$covid_data <- covid19_US
    inp$covid_death_data <- covid19_deaths_US
    
    if (input$countySelect == "ALL") {
      inp$Admin2 <- ""
    } else {
      inp$Admin2 <- input$countySelect
    }
    inp$Country.Region <- "US"
    inp$Province.State <- input$stateSelect
    inp$mobility_subregion <- inp$Admin2
    temp <- region_mobility_and_incidence_v7(inp)
    temp$plot
  })
  
}