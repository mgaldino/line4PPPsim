#' @title Simulates a vector of sensibilidade parameter
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#' @param start_seed A number
#' @param num_years A number
#' @param mu A number
#' @param sd A number
#' @param type A character. Either "random_walk", "white_noise" or "constant"
#'
#' @return a vector of a sensibilidade of size equal to num_years
#'
#' @examples  gen_sensibilidade(start_seed = 1.3, mu = 0, sd = .05)
#'
#' @export gen_sensibilidade

gen_sensibilidade <- function(start_seed, num_years = 33,
                              mu = 0, sd = .05, type = "random_walk") {

  switch(type,
         random_walk = random_walk(start_seed, num_years,
                                   mu, sd),
         white_noise = white_noise(start_seed, num_years,
                                   mu, sd),
         constant(start_seed, num_years))
}
