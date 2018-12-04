#' @title Generates passengers for line 4
#'
#' @description It generate a forecast of passengers for all years of contract, with integrated and exclusie passengers.
#'
#' @param sensibilidade A vector
#' @param use_random_walk Logical. If TRUE, it will use a random walk to forecast demand (instead of a fixed forecast as in excel)
#' @param start_seed A number. First point of the series. Use only if random_walk = T
#' @param num_years A number
#' @param mu A number
#' @param sd A number
#' @param beta A number. Effect of past step on the next step.
#' @param jump logical. If TRUE, in the eight year of the series the forecasted demand increases by 25\% relative to last value (plus error). Equivalent to set beta =1.25 for year 8.
#'
#' @return A list of four elements, all with size 33: numPassengersExclusive, numPassengersIntegrated, demanda_projetada and demanda_real
#'
#' @examples  gen_num_passengers(sensibilidade = 1.3)
#'
#' @export gen_num_passengers

gen_num_passengers <- function (sensibilidade = 1, use_random_walk=F,
                                start_seed=196860, num_years=33,
                                mu = start_seed, sd = .07*start_seed,
                                beta=1, jump=F) {

  demanda_projetada <- c(0, 0, 0, 196860, 204204, 211822, 214415, 284490,
                         264251,	271691,	295706,	278154,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183, 274183,	274183, 274183)


  perc_pass_exclusive_contract <- c(0, 0, 0, rep(.05, 4), rep(.1, 26))
  perc_pass_integrated_contract <- 1 - perc_pass_exclusive_contract

  if(use_random_walk) {
    demanda_real <- round(random_walk_passengers(start_seed, num_years,
                                           mu, sd, beta, jump))
  } else {
    demanda_real <- round(demanda_projetada * sensibilidade)
  }



  numPassengersExclusive <- round(demanda_real * perc_pass_exclusive_contract)
  numPassengersIntegrated <- round(demanda_real * perc_pass_integrated_contract)

  return(list(numPassengersExclusive, numPassengersIntegrated, demanda_projetada, demanda_real))
}

