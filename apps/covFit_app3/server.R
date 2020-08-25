
server <- function(input, output, session) {

  countries_vector <- reactive({
    if (input$trial_sites_only) {
      countries_vector <- unique(df_trial_sites$Country_Region)
    } else {
      countries_vector <- unique(df_master$Country_Region)
    }
    countries_vector <- countries_vector[!is.na(countries_vector)]
  })
  observe({
    updateSelectInput(session = session, inputId = "countrySelect", choices = countries_vector())
  })
  
  regions_vector <- reactive({
    if (input$trial_sites_only) {
      df <- df_trial_sites
    } else {
      df <- df_master
    }
    if (input$countrySelect == "US") {
      regions_vector <- c("ALL", "Puerto Rico", state.name)
    } else {
      df_tmp <- df %>% subset(Country_Region == input$countrySelect)
      regions_vector <- c("ALL", unique(df_tmp$Province_State))
    }
    
  })
  observe({
    updateSelectInput(session = session, inputId = "regionSelect", choices = regions_vector())
  })
  
  subregions_vector <- reactive({
    if (input$trial_sites_only) {
      df <- df_trial_sites
    } else {
      df <- df_master
    }
    df_tmp <- df %>% subset(Country_Region == input$countrySelect) %>% subset(Province_State == input$regionSelect)
    subregions_vector <- c("ALL", unique(df_tmp$Admin2))
  })
  observe({
    updateSelectInput(session = session, inputId = "subregionSelect", choices = subregions_vector())
  })
  
  sites_vector <- reactive({
    if (input$trial_sites_only) {
      df <- df_trial_sites %>% subset(Country_Region == input$countrySelect) 
      if (input$regionSelect != "ALL") {
        df <- df %>% subset(Province_State == input$regionSelect)
      } 
      if (input$subregionSelect != "ALL") {
        df <- df %>% subset(Admin2 == input$subregionSelect)
      }
      sites_vector <- unique(df$`Site name`)
    } else {
      sites_vector <- c("")
    }

  })
  observe({
    updateSelectInput(session = session, inputId = "siteSelect", choices = sites_vector())
  })
  
  plotInput <- function() {
    inp <- base_input_list
    inp$Country_Region <- input$countrySelect
    inp$Province_State <- req(input$regionSelect)
    inp$Admin2 <- req(input$subregionSelect)
    inp$predict_data_start <- input$predict_data_start
    inp$predict_from_date <- input$predict_from_date
    inp$predict_date <- input$predict_date
    inp$R_window_size <- input$R_window_size
    inp$incidencePer <- input$incidencePer
    if (input$manual_y_scale) {
      inp$manual_max_incidence <- input$manual_max_incidence
    } else {
      inp$manual_max_incidence <- NA
    }
    if (input$apply_CFR_correction) {
      inp$CFR <- input$CFR
    } else {
      inp$CFR <- NA
    }
    
    # Pull out the data for the selected region:
    
    if (input$trial_sites_only) {
      df <- df_trial_sites
    } else {
      df <- df_master
    }
    inp$filter_to_trial_site_YN <- input$trial_sites_only
    inp$trial_site <- input$siteSelect
    inp <- make_incidence_frame_ww(df, inp)
    # plot_to_render <- region_mobility_and_incidence_ww(inp)$plot
    plotInput <- region_mobility_and_incidence_ww(inp)$plot
    
  }
  
  output$Plot <- renderPlot({
    print(plotInput())
  })
  
  # output$Plot <- renderPlot({
  #   inp <- base_input_list
  #   inp$Country_Region <- input$countrySelect
  #   inp$Province_State <- req(input$regionSelect)
  #   inp$Admin2 <- req(input$subregionSelect)
  #   inp$predict_data_start <- input$predict_data_start
  #   inp$predict_from_date <- input$predict_from_date
  #   inp$predict_date <- input$predict_date
  #   inp$R_window_size <- input$R_window_size
  #   inp$incidencePer <- input$incidencePer
  #   if (input$manual_y_scale) {
  #     inp$manual_max_incidence <- input$manual_max_incidence
  #   } else {
  #     inp$manual_max_incidence <- NA
  #   }
  #   if (input$apply_CFR_correction) {
  #     inp$CFR <- input$CFR
  #   } else {
  #     inp$CFR <- NA
  #   }
  # 
  #   # Pull out the data for the selected region:
  #   
  #   if (input$trial_sites_only) {
  #     df <- df_trial_sites
  #   } else {
  #     df <- df_master
  #   }
  #   inp$filter_to_trial_site_YN <- input$trial_sites_only
  #   inp$trial_site <- input$siteSelect
  #   inp <- make_incidence_frame_ww(df, inp)
  #   plot_to_render <- region_mobility_and_incidence_ww(inp)$plot
  #   print(plot_to_render)
  #   
  # })
  
  # output$downloadPlot <- downloadHandler(
  #   filename = "Shinyplot.png",
  #   content = function(file) {
  #     png(file)
  #     print(plotInput())
  #     dev.off()
  #   }
  # )
  
  output$downloadPlot <- downloadHandler(
    # filename = "Shinyplot.png",
    filename = function() { paste0(input$subregionSelect, " ", input$regionSelect, " ", input$countrySelect, ".png") },
    content = function(file) {
      # ggsave(file, plotInput(), height=5, width=5)
      ggsave(file, plotInput())
      
    }
  )
}

