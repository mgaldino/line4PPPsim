#' @title Generates forecasted demand for line 4 PPP
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
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
#' @return a vector of revenue of size equal to num_years - as a result of forecasted demand in contract
#'
#' @examples  gen_forecast_revenue(sensibilidade = 1.3, ajuste_inflacao = FALSE)
#'
#' @export gen_forecast_revenue

gen_forecast_revenue <- function(num_years = 30, t_0 = 2.14, ipc_0 = 1.1,
                                 ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA, igpm_realizado=NA,
                                 qualityAdjustment=1, ajuste_inflacao = FALSE, sensibilidade = 1.3) {

  num_pass <- gen_num_passengers(sensibilidade = 1)

  numPassengersExclusive <- num_pass[[1]]
  numPassengersIntegrated <- num_pass[[2]]
  demanda_projetada <- num_pass[[3]]
  demanda_real <- num_pass[[4]]



  price_ticket <- gen_price_ticket_line4(num_years = num_years, t_0 = t_0,
                                         ipc_0 = ipc_0, ipgm_0 = ipgm_0,
                                         a =a,b= b, ipc_realizado = ipc_realizado,
                                         igpm_realizado = igpm_realizado,
                                         sensibilidade = 1, ajuste_inflacao)


  rt <- (numPassengersExclusive * price_ticket + numPassengersIntegrated *.5*price_ticket)* qualityAdjustment
  return(rt)
}
