#' @title Computes tariff revenut (receita tarifaria)
#'
#' @description Computes total revenue from tariffs. The formula is: Tariff revenue = ((Exclusive passengers x Implicit Tariff) + .5xIntegrated passengers x Implicit tariff)) x (.8 + .1xlqs + .1xlqm). lqs and lqm are set to 1 in this version.
#'
#' @param qualityAdjustment A number
#' @param num_years A number
#' @param t_0 A number
#' @param ipc_0 A number
#' @param ipgm_0 A number
#' @param a A number
#' @param b A number
#' @param ipc_realizado A number
#' @param igpm_realizado A number
#' @param sensibilidade A vector
#' @param ajuste_inflacao logical
#'
#' @return a vector of revenue of size equal to num_years - as a result of real demand as set by sensibilidade
#'
#' @examples  gen_num_passengers(sensibilidade = 1.3)
#'
#' @export gen_ticket_revenue

gen_ticket_revenue <- function(num_years = 30, t_0 = 2.14, ipc_0 = 1.1,
                               ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA, igpm_realizado=NA,
                               qualityAdjustment=1, sensibilidade = 1, ajuste_inflacao) {

  num_pass <- gen_num_passengers(sensibilidade = sensibilidade)

  numPassengersExclusive <- num_pass[[1]]
  numPassengersIntegrated <- num_pass[[2]]
  demanda_projetada <- num_pass[[3]]
  demanda_real <- num_pass[[4]]

  price_ticket <- gen_price_ticket_line4(num_years = num_years, t_0 = t_0,
                                         ipc_0 = ipc_0, ipgm_0 = ipgm_0,
                                         a =a,b= b, ipc_realizado = ipc_realizado,
                                         igpm_realizado = igpm_realizado,
                                         sensibilidade = sensibilidade, ajuste_inflacao)

  implicit_ticket_rev <- gen_implicit_ticket_revenue(numPassengersExclusive = numPassengersExclusive,
                                                     numPassengersIntegrated = numPassengersIntegrated,
                                                     price_ticket = price_ticket,
                                                     num_years = num_years, t_0 = t_0,
                                                     ipc_0 = ipc_0, ipgm_0 = ipgm_0,
                                                     a =a,b= b, ipc_realizado = ipc_realizado,
                                                     igpm_realizado = igpm_realizado,
                                                     sensibilidade = sensibilidade, ajuste_inflacao)

  rt <- (numPassengersExclusive * implicit_ticket_rev + numPassengersIntegrated *.5*implicit_ticket_rev)* qualityAdjustment
  return(rt)
}


