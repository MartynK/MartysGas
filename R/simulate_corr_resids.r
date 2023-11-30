#' Simulate Correlated Residuals
#'
#' This function simulates a series of residuals over a specified number of days, 
#' incorporating a defined level of autocorrelation and standard deviation. It's 
#' useful for generating synthetic data that mimics the temporal correlation often 
#' seen in real-world time series data.
#'
#' @param n An integer specifying the number of days to simulate. Default is 365, 
#'          representing a year.
#' @param autocorrelation A numeric value representing the degree of autocorrelation 
#'                        between residuals. Default value is 0.8397.
#' @param total_sd A numeric value representing the total standard deviation of the 
#'                 residuals. Default value is 3.81.
#' @param cor_cutoff A numeric value representing the cutoff for the autocorrelation, 
#'                   beyond which the correlation is considered negligible. Default 
#'                   value is 0.05.
#'
#' @return A matrix of simulated residuals with dimensions 1 x n, where n is the 
#'         number of days.
#'
#' @examples
#' # Simulate correlated residuals for a default one-year period
#' simulated_data <- simulate_corr_resids()
#'
#' # Simulate correlated residuals for 100 days with specified parameters
#' simulated_data_custom <- simulate_corr_resids(n = 100, autocorrelation = 0.5, 
#'                                               total_sd = 2, cor_cutoff = 0.01)
#'
#' @importFrom mvtnorm rmvnorm
#' @export
simulate_corr_resids <- function(n = 365, 
                                 autocorrelation = 0.8397,
                                 total_sd = 3.81,
                                 cor_cutoff = 0.05) {
  # Create a covariance matrix
  cov_matrix <- matrix(0, n, n)
  
  # Determine the critical number of days beyond which correlation is negligible
  crit_n <- log(cor_cutoff) / log(autocorrelation)
  
  # Populate the covariance matrix based on autocorrelation and total standard deviation
  for (i in 1:n) {
    for (j in 1:n) {
      if (abs(i - j) < crit_n) {
        cov_matrix[i, j] <- (autocorrelation ^ abs(i - j)) * (total_sd ^ 2)
      }
    }
  }
  
  # Simulate the residuals using a multivariate normal distribution
  simulated_resids <- mvtnorm::rmvnorm(1, mean = rep(0, n), sigma = cov_matrix) %>% t
  
  return(simulated_resids)
}
