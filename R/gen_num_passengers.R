#' @title Simulates fiscal impact of line 4 subway PPP
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#' @param sensibilidade A vector
#'
#' @return A list of four elements, all with size 33: numPassengersExclusive, numPassengersIntegrated, demanda_projetada and demanda_real
#'
#' @examples  gen_num_passengers(sensibilidade = 1.3)
#'
#' @export gen_num_passengers

gen_num_passengers <- function (sensibilidade = 1) {
  demanda_projetada <- c(0, 0, 0, 196860, 204204, 211822, 214415, 284490,
                         264251,	271691,	295706,	278154,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183,	274183,	274183,	274183,
                         274183,	274183, 274183,	274183, 274183)


  perc_pass_exclusive_contract <- c(0, 0, 0, rep(.05, 4), rep(.1, 26))
  perc_pass_integrated_contract <- 1 - perc_pass_exclusive_contract

  demanda_real <- round(demanda_projetada * sensibilidade)

  numPassengersExclusive <- round(demanda_real * perc_pass_exclusive_contract)
  numPassengersIntegrated <- round(demanda_real * perc_pass_integrated_contract)

  return(list(numPassengersExclusive, numPassengersIntegrated, demanda_projetada, demanda_real))
}

