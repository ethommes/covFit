smoothed_incidence_and_rho <- function(incidence,window,alignment) {
 # alignment = "right", "left" or "center"
        
 dates <- incidence$dates
 cases <- incidence$cases
 log_cases <- log(cases)
 log_cases_roll <- frollmean(log_cases, window, align=alignment, hasNA=T, na.rm=T)
 rho <- c(0,diff(log_cases_roll))
 rho_roll <- frollmean(rho, window, align=alignment, hasNA=T, na.rm=T)
 # R <- R0_SEIR(rho_roll, sigma_SEIR, gamma_SEIR)
 df <- data.frame(incidence, "log_cases"=log_cases, "log_cases_roll"=log_cases_roll, "rho_roll"=rho_roll)
 return(df)
}