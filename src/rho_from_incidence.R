
rho_from_incidence <- function(df, inputs) {
  y_offset <- inputs$rho_from_incidence_y_offset
  y_scale <- inputs$rho_from_incidence_y_scale
  t_lag <- inputs$rho_from_incidence_t_lag
  rho_model <- y_offset - y_scale*lag(exp(df$log_cases_roll), t_lag, na.pad = TRUE)
  df <- data.frame(df, "rho_model" = rho_model)
  
  return(df)
}