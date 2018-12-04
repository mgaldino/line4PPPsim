#' @title Simulates a random walk for gen_num_passengers function
#'
#' @description Auxiliar function. It computes a random walk with jumps, that is used to generate forecasted demand.
#'
#' @param start_seed A number. First point of the series.
#' @param num_years A number
#' @param mu A number
#' @param sd A number
#' @param beta A number. Effect of past step on the next step.
#' @param jump logica. If TRUE, in the eight year of the series the forecasted demand increases by 25% relative to last value (plus error). Equivalent to set beta =1.25 for year 8.
#'
#' @return a vector of simulated random walk times series of size equal to num_years
#'
#' @examples  random_walk(start_seed = 196860)
#'
#' @export random_walk

random_walk <- function(start_seed=196860, num_years=33,
                        mu = start_seed, sd = .7*start_seed, beta=1, jump=T) {


  y <- c(0,0,0)
  y[4] <- start_seed
  for ( i in 5:num_years){
    erro <- rnorm(1, mu, sd)

    if(i == 8 & jump) { # jump
      y[i] <- 1.25*y[i-1] + erro
    } else {
      y[i] <- beta*y[i-1] + erro
      }

  }

  return(y)
}