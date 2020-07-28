
R0_and_R_0_from_Reff_and_finalsize  <- function(Reff, finalsize) {
  # finalsize_max <- uniroot.all(function (finalsize_max) Reff + 1/finalsize_max*log(1 - finalsize),c(0,1))
  # if (finalsize > finalsize_max) finalsize <- finalsize_max
  R_0_max <- 1 - finalsize - 1e-9
  R_0 <- uniroot.all(function (R_0) Reff/(1-R_0) + 1/finalsize*log(1 - 1/(1 - R_0)*finalsize), c(0,R_0_max))
  R0 <- Reff/(1-R_0)
  result <- data.frame("R0"=R0, "R_0_factor"=R_0)
  return(result)
}

finalSize <- function(R0, R_0) {
  # R_0_max <- 1 - 1/R0 - 1e-9
  finalsize_max <- 1 - R_0
  finalsize <- uniroot.all(function (finalsize) R0 + 1/finalsize*log(1 - finalsize/(1 - R_0)), c(0,finalsize_max))
  return(finalsize)
}

# average rho of the N lowest NLL_SSE solutions:
avrg_best_N_rho <- function(epifit_MC_output,N) {
  fraction <- N/length(epifit_MC_output$MC_results$rho)
  indices_to_avrg <- MC_output_subset_indices(epifit_MC_output,fraction,"vNLL_SSE")
  mean_rho <- mean(epifit_MC_output$MC_results$rho[indices_to_avrg])
  return(mean_rho)
}

# average gamma_recov of the N lowest NLL_SSE solutions:
avrg_best_N_gamma_recov <- function(epifit_MC_output,N) {
  fraction <- N/length(epifit_MC_output$MC_results$rho)
  indices_to_avrg <- MC_output_subset_indices(epifit_MC_output,fraction,"vNLL_SSE")
  mean_gamma_recov <- mean(epifit_MC_output$MC_results$gamma_recov[indices_to_avrg])
  return(mean_gamma_recov)
}

# Sherry Towers' alternatively-parameterized negative binomial function:
rnbinom_lsmith=function(n,mu,alpha){
  size = 1/alpha
  prob = size/(mu+size)
  return(rnbinom(n,size=size,prob=prob)) 
}

R0_SEIR <- function (rho,lambda,gamma) {
  R0 <- (rho+lambda)*(rho+gamma)/(lambda*gamma)
  return(R0)
}
Reff_SEIR <- function (rho,lambda,gamma,s) {
  Reff <- (rho+lambda)*(rho+gamma)*s/(lambda*gamma)
  return(Reff)
}

R0_SIR <- function(rho,gamma,r_0) {
  # r_0 = initial fraction immune
  Reff <- rho/gamma + 1
  R0 <- Reff/(1 - r_0)
  return(R0)
}

Reff_SIR <- function(rho,gamma) {
  # r_0 = initial fraction immune
  Reff <- rho/gamma + 1
  return(Reff)
}

rho_SEIR <- function(R0, lambda, gamma,s) {
  beta <- R0*gamma
  rho <- (-(lambda+gamma)+sqrt((lambda - gamma)^2 + 4*lambda*beta*s))/2
  return(rho)
}

rho_SEIR_from_beta <- function(beta,lambda,gamma,s) {
  rho <- (-(lambda+gamma) + sqrt((lambda - gamma)^2 +4*lambda*beta*s))/2
  return(rho)
}

rho_SIR <- function(R0,gamma,s) {
  Reff <- R0*s
  rho <- gamma*(Reff - 1)
  return(rho)
}




