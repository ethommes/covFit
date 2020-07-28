
compute_current_rho_and_R <- function(incidence,window_for_current_R,sigma_SEIR,gamma_SEIR) {
  len <- nrow(incidence)
  incidence_subframe <- incidence[(len-window_for_current_R):len,]
  
  # Get rid of any days with zero incidence
  indices <- (incidence_subframe$cases) > 0
  if (sum(indices) >= 3) {
    incidence_for_lm <- incidence_subframe[indices,]
    incidence_for_lm <- data.frame(incidence_for_lm,"log_cases" = log(incidence_for_lm$cases))
    fit_summary <- summary(lm(log_cases~dates, data=incidence_for_lm))
    intercept_current <- fit_summary$coefficients["(Intercept)","Estimate"]
    rho_current <- fit_summary$coefficients["dates","Estimate"]
    R_current <- R0_SEIR(rho_current,sigma_SEIR,gamma_SEIR)
  } else {
    rho_current <- NA
    R_current <- NA
  }
    
  result_to_return <- data.frame(
    "rho_current" = rho_current,
    "R_current" = R_current
  )
  return(result_to_return)
  # fit <- lm(incidence_subframe)
}