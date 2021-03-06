#' @title Generates passengers for line 4
#'
#' @description It generate a forecast of passengers for all years of contract, with integrated and exclusie passengers.
#'
#' @param sensibilidade A vector
#' @param num_years A number
#'
#' @return A list of four elements, all with size 33: numPassengersExclusive, numPassengersIntegrated, demanda_projetada and demanda_real
#'
#' @examples  gen_num_passengers(sensibilidade = 1.3)
#'
#' @export gen_num_passengers

gen_num_passengers <- function (sensibilidade = 1,
                                num_years=33) {


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


