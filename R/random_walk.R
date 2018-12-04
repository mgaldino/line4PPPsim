#' @title Simulates a random walk
#'
#' @description Auxiliar function. It computes a random walk, that is used to generate sensibility. In other words, its results are used as themultiplier of real demand. A value os 1.1 means for instance that the demand is 1.1 bigger than projected.
#'
#' @param start_seed A number. First point of the series.
#' @param num_years A number
#' @param mu A number
#' @param sd A number
#' @param beta A number. Effect of past step on the next step.
#'
#' @return a vector of simulated random walk times series of size equal to num_years
#'
#' @examples  random_walk(start_seed = 1.3, mu = 0, sd = .05)
#'
#' @export random_walk

random_walk <- function(start_seed, num_years=33,
                        mu = 0, sd = .05, beta=1) {

  y <- start_seed
  for ( i in 2:num_years){
    erro <- rnorm(1, mu, sd)

    y[i] <- beta*y[i-1] + erro
  }
  # truncando
  y <- ifelse(y < .6, .6,
              ifelse(y > 1.4, 1.4, y))

  return(y)
}
