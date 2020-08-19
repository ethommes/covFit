
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
  
  output$Plot <- renderPlot({
    inp <- base_input_list
    inp$Country_Region <- input$countrySelect
    inp$Province_State <- req(input$regionSelect)
    inp$Admin2 <- req(input$subregionSelect)
    inp$predict_from_date <- input$predict_from_date
    inp$predict_date <- input$predict_date
    inp$R_window_size <- input$R_window_size
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
    }
    inp$filter_to_trial_site_YN <- input$trial_sites_only
    inp$trial_site <- input$siteSelect
    inp <- make_incidence_frame_ww(df, inp)
    temp <- region_mobility_and_incidence_ww(inp)
    
    # plot_to_render <- plot.new()
    # plot_to_render
    plot_to_render <- temp$plot
    plot_to_render
  })
}

