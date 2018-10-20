#' @title Simulates a constant time series
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#' @param start_seed A number
#' @param num_years A number
#'
#' @return a vector of a constant times series of size equal to num_years
#'
#' @examples  constant(start_seed = 1.3, mu = 0, sd = .05)
#'
#' @export constant

constant <- function(start_seed, num_years = 33) {

  y <- rep(start_seed, num_years)

}
