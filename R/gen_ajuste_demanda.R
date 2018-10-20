#' @title Simulates fiscal impact of line 4 subway PPP
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
#' @return a vector of demand adjustement of size equal to num_years - input to compute implicit rate
#'
#' @examples  gen_num_passengers(sensibilidade = 1.3)
#'
#' @export gen_price_ticket_line4

gen_ajuste_demanda <- function(sensibilidade = 1, num_years,
                               t_0 = 2.14,
                               ipc_0 , ipgm_0 ,
                               a,b, ipc_realizado = NA ,
                               igpm_realizado = NA, ajuste_inflacao = F) {

  stopifnot(.6 <= sensibilidade && sensibilidade <= 1.4)

  price_ticket <- gen_price_ticket_line4(num_years, t_0,
                                         ipc_0, ipgm_0,
                                         a, b, ipc_realizado = ipc_realizado,
                                         igpm_realizado = igpm_realizado, ajuste_inflacao)
  passengers <- gen_num_passengers()
  dp <- passengers[[3]]
  dr <- dp*sensibilidade
  pe <- passengers[[1]]
  pi <- passengers[[2]]
  aux <- .5*pi*price_ticket/(pi +pe) + price_ticket*pe / (pe +pi)


  if(.6 <= sensibilidade && sensibilidade <= .8) {
    md <- (.06*dp + .9*(.8*dp - dr))*aux
  }

  if(.8 < sensibilidade && sensibilidade <= .9) {
    md <- .06*(.9*dp - dr)*aux
  }

  if(.9 < sensibilidade && sensibilidade <= 1.1) {
    md <- rep(0, num_years)
  }

  if(1.1 < sensibilidade && sensibilidade <= 1.2) {
    md <- -.06*(dr - 1.1*dp)*aux
  }

  if(1.2 < sensibilidade && sensibilidade <= 1.4) {
    md <- -(.06*dp + .9*(dr - 1.2*dp))*aux
  }
  md <- round(ifelse(is.nan(md),0,md))
  return(md)

}

