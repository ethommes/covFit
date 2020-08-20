
plot_all_non_US_countries <- function(covid_data, covid_death_data) {
  with(input, {
    countries_vector <- covid19$Country.Region
    # Remove duplicate country names:
    countries_vector <- countries_vector[!duplicated(countries_vector)]
    # Remove some entries, e.g. Diamond Princess:
    countries_vector <- countries_vector[
      countries_vector != "Diamond Princess" &
        countries_vector != "Eritrea" &
        countries_vector != "Holy See" &
        countries_vector != "Taiwan*" &
        countries_vector != "US" &
        countries_vector != "MS Zaandam" &
        countries_vector != "Western Sahara"
      ]
    
    countries_frame <- data.frame("country_names_JHU"=countries_vector,
                                  "country_names_mobility"=countries_vector,
                                  "correction_factor"=1)
    
    n_rows <- length(countries_frame$country_names_JHU)
    empty_date_vector <- rep(NA,n_rows); class(empty_date_vector) <- "Date"
    override_table_countries <- data.frame("countries"=countries_frame$country_names_JHU,onset_date_override=empty_date_vector, turnover_date_override =empty_date_vector)
    
    input$most_recent_R_window = 30
    input$pathname_figs <- "../figs/DEMO/trial_non_US_countries"
    input$override_table <- override_table_countries
    input$covid_data <- covid19
    input$covid_death_data <- covid19_deaths
    input$plot_only_linear_TF <- F
    input$plot_TF <- F
  })
}