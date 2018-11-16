#' @title Simulates white noise
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#'@import stats
#'
#' @param start_seed A number
#' @param num_years A number
#' @param mu A number
#' @param sd A number
#'
#' @return a vector of simulated white noise times series of size equal to num_years
#'
#' @examples  white_noise(start_seed = 1.3, mu = 0, sd = .05)
#'
#' @export white_noise

white_noise <- function(start_seed, num_years=33,
                        mu , sd ) {

  y <- start_seed
  erro <- rnorm(num_years-1, mu, sd)
  y <- c(y, erro)

  # truncando
  y <- ifelse(y < .6, .6,
              ifelse(y > 1.4, 1.4, y))
  return(y)
}

