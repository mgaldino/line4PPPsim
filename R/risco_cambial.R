#' @title Risco cambial
#'
#' @description computes totl spending for a given sensitibilit in exhange rate
#'
#' @param sens_tx_c A number, either 1 or 2 (for type of long term loan)
#' @param tx_c_base base exchange rate
#' @param exp_camb percent of expostion to foreign currency
#' @param exp_usd exposition to dollar
#'
#' @return amount that could be paid due to variation in exchange rate
#'
#' @examples  risco_cambio(sens_tx_c = 1.1)
#'
#' @export risco_cambio


risco_cambio <- function(sens_tx_c, tx_c_base = 3.77, exp_camb = .5, exp_usd = 1) {

  valor_ep <- 17021.599
  ep_n <- c(0, valor_ep, rep(0, 31))
  fin_lp1 <- gen_fin_lp1()
  fin_lp2 <- gen_fin_lp2()
  pi_ep <- gen_interest_ep()
  pi_fin_lp1 <- gen_interest_lp(type_lp = 1)
  pi_fin_lp2 <- gen_interest_lp(type_lp = 2)

  result <- sum(ep_n + fin_lp1 + fin_lp1 + pi_ep + pi_fin_lp1 + pi_fin_lp2)*exp_usd*(tx_c_base - tx_c_base*sens_tx_c)*exp_camb

  return(result)

}
