#' @title Gera tarifa da linha 4
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
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
#' @return a vector of price tickets o size equal to num_years
#'
#' @examples  gen_price_ticket_line4(num_years = 33)
#'
#' @export gen_price_ticket_line4

gen_price_ticket_line4 <- function(num_years, t_0 = 2.14, ipc_0 = 1.1,
                                   ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA,
                                   igpm_realizado=NA, sensibilidade = 1, ajuste_inflacao = F) {
  # esta função  calcula a TR: tarifa de remuneração

  if (ajuste_inflacao) {

    ano <- 1:num_years


    ipc <- sim_ipc(ipc_realizado = ipc_realizado)

    igpm <- sim_igpm(igpm_realizado = igpm_realizado)

    rt <- numeric()
    priceTicket <- numeric()


    for ( i in 1:num_years) {
      if(ano[i] <= 15) {
        priceTicket[i] <- t_0 * (a*(igpm[i]/ipgm_0 ) + b*(ipc[i]/ipc_0))
      } else {
        priceTicket[i] <- priceTicket[i-1] * ipc[i]/ipc[i-1]
      }
    }
  } else {
    priceTicket <- rep(2.08, num_years)
  }
  return(priceTicket)
}

