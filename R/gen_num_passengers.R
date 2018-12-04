#' @title Simulates fiscal impact of line 4 subway PPP
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#' @param sensibilidade A vector
#' @param start_seed A number. First point of the series.
#' @param num_years A number
#' @param mu A number
#' @param sd A number
#' @param beta A number. Effect of past step on the next step.
#' @param jump logical. If TRUE, in the eight year of the series the forecasted demand increases by 25% relative to last value (plus error). Equivalent to set beta =1.25 for year 8.
#' @param random_walk Logical. If TRUE, it will use a random walk to forecast demand (instead of a fixed forecast as in excel)
#'
#' @return A list of four elements, all with size 33: numPassengersExclusive, numPassengersIntegrated, demanda_projetada and demanda_real
#'
#' @examples  gen_num_passengers(sensibilidade = 1.3)
#'
#' @export gen_num_passengers

gen_num_passengers <- function (sensibilidade = 1, random_walk=F,
                                start_seed=196860, num_years=33,
                                mu = start_seed, sd = .7*start_seed, beta=1, jump=T) {

  demanda_projetada <- c(0, 0, 0, 196860, 204204, 211822, 214415, 284490,
                         264251,	271691,	295706,	278154,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183, 274183,	274183, 274183)


  perc_pass_exclusive_contract <- c(0, 0, 0, rep(.05, 4), rep(.1, 26))
  perc_pass_integrated_contract <- 1 - perc_pass_exclusive_contract

  if(random_walk == T) {
    demanda_real <- round(random_walk_passengers(start_seed, num_years,
                                           mu, sd, beta, jump))
  } else {
    demanda_real <- round(demanda_projetada * sensibilidade)
  }



  numPassengersExclusive <- round(demanda_real * perc_pass_exclusive_contract)
  numPassengersIntegrated <- round(demanda_real * perc_pass_integrated_contract)

  return(list(numPassengersExclusive, numPassengersIntegrated, demanda_projetada, demanda_real))
}

