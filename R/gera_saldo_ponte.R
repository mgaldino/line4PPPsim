#' @title Saldo para emprestimo ponte
#'
#' @description computes balance between loan and amortiation (for 'emprestimo ponte')
#'
#' @param amortizacao A vector with amortizations data
#' @param emprestimo A vector with value of loan (repeat total value)
#'
#' @return balance of 'emprestimo ponte' after amortization
#'
#' @examples  gera_saldo_ponte(c(0, 100, 0), c(100, 100, 100))
#'
#' @export gera_saldo_ponte

gera_saldo_ponte <- function(amortizacao, emprestimo) {

  saldo_valor_ep <- emprestimo - amortizacao
  n <- length(amortizacao)
  for ( i in 2:n) {
    saldo_valor_ep[i] <- saldo_valor_ep[i-1] - amortizacao[i]
  }
  return(saldo_valor_ep)
}
