#' @title Simulates a random walk
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
#' @return a vector of simulated random walk times series of size equal to num_years
#'
#' @examples  random_walk(start_seed = 1.3, mu = 0, sd = .05)
#'
#' @export random_walk

random_walk <- function(start_seed, num_years=33,
                        mu = 0, sd = .05) {

  y <- start_seed
  for ( i in 2:num_years){
    erro <- rnorm(1, mu, sd)

    y[i] <- y[i-1] + erro
  }
  # truncando
  y <- ifelse(y < .6, .6,
              ifelse(y > 1.4, 1.4, y))

  return(y)
}
