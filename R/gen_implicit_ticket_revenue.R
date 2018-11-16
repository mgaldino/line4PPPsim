#' @title Gera tarifa implicita da linha 4
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#' @param numPassengersExclusive A vector
#' @param numPassengersIntegrated A vector
#' @param price_ticket A vector
#' @param num_years A number
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
#' @return a vector of demand adjustement of size equal to num_years - input to compute implicit rate
#'
#' @examples  gen_num_passengers(sensibilidade = 1.3)
#'
#' @export gen_implicit_ticket_revenue

gen_implicit_ticket_revenue <- function (numPassengersExclusive,
                                         numPassengersIntegrated, price_ticket,
                                         num_years, t_0 = 2.14, ipc_0 = 1.1,
                                         ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA,
                                         igpm_realizado=NA, sensibilidade, ajuste_inflacao) {

  ano <- 1:num_years
  incid_period <- numeric()

  for ( i in ano) {
    if ( ano[i] <= 3) {
      incid_period[i] <- 0
    }
    if ( ano[i] == 4) {
      incid_period[i] <- .5
    }
    if ( ano[i] > 4 && ano[i] <= 11) {
      incid_period[i] <- 1
    }
    if ( ano[i] > 11) {
      incid_period[i] <- 0
    }
  }

  md <- gen_ajuste_demanda(sensibilidade, num_years, t_0 = t_0,
                           ipc_0 = ipc_0, ipgm_0 = ipgm_0,
                           a= a,b = b, ipc_realizado = ipc_realizado,
                           igpm_realizado = igpm_realizado)

  ti <- (md*incid_period + numPassengersExclusive*price_ticket  +   price_ticket*numPassengersIntegrated*.5)/(numPassengersExclusive + numPassengersIntegrated*.5 )
  ti <- ifelse(is.nan(ti), 2.08, ti)
  return(ti)

}


