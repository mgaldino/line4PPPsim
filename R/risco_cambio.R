#' @title Risco cambial
#'
#' @description computes total spending for a given sensibility in exhange rate
#'
#' @param sens_tx_c A number. How much above or below we expect the exchange rate to be. 1.1 means 10% higher than historical trend.
#' @param tx_c_base base exchange rate
#' @param exp_camb percent of expostion to foreign currency
#' @param exp_usd exposition to dollar
#' @param cambio_real Logical. If TRUE, will use real exchange rate up to 2017, then 3.77 as prediction for the following years. If not, will use tx_c_base and replicate.
#' @param start_year number. Year the series will start.
#' @param amp_erro number Amplitude of error term. In practice, it is the number we will multiply the normal error. If 1 it will generate normal errors.
#'
#'
#' @return amount that could be paid due to variation in exchange rate
#'
#' @examples  risco_cambio(sens_tx_c = 1.1)
#'
#' @export risco_cambio


risco_cambio <- function(sens_tx_c, tx_c_base = 3.77, cambio_real=F,
                         exp_camb = .5, exp_usd = 1, start_year, amp_erro,
                         igual_excel = F) {

  valor_ep <- 17021.599
  ep_n <- c(0, valor_ep, rep(0, 31))
  amortizacao_fin_lp1 <- c(0, 0, 0, 0, rep(45701.625, 8), rep(0, 21) )
  amortizacao_fin_lp2 <- c(rep(0, 9), rep(70579.2, 5), rep(0, 19) )
  pi_ep <- gen_interest_ep()
  pi_fin_lp1 <- gen_interest_lp(type_lp = 1)
  pi_fin_lp2 <- gen_interest_lp(type_lp = 2)

  if(cambio_real) {
    tx_c_base <- c(1.94833, 1.83416, 1.99833, 1.76083, 1.675, 1.95583, 2.15566,
                   2.35333, 3.33083, 3.49, 3.1925, 3.62, 3.68, 3.66, 3.7, rep(3.77, 18))

  } else {
    tx_c_base <- rep(tx_c_base, 33)
  }

  if(igual_excel) {
    dif_cambio <- tx_c_base - tx_c_base*sens_tx_c
  } else {
    dif_cambio <- tx_c_base - gen_cambio_futuro(mu = (sens_tx_c - 1)*0.6605721 )
  }

  result <- -(ep_n + amortizacao_fin_lp1 + amortizacao_fin_lp2 + pi_ep + pi_fin_lp1 + pi_fin_lp2)*exp_usd*(dif_cambio)*exp_camb

  return(result)

}
