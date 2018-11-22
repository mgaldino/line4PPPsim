#' @title Saldo para emprestimo de longo prazo
#'
#' @description computes balance between loan and amortiation (for long term loan)
#'
#' @param amortizacao A vector with amortizations data
#' @param emprestimo A vector with value of loan for each period
#'
#' @return balance of long term loan after amortization
#'
#' @examples  gera_saldo_fin(c(0, 50, 50), c(100, 0, 0))
#'
#' @export gera_saldo_fin

gera_saldo_fin <- function(amortizacao, emprestimo, num_round=NA) {

  saldo_valor_ep <- cumsum(emprestimo - amortizacao)
  if(!is.na(num_round)) {
    saldo_valor_ep <- round(saldo_valor_ep, num_round)
  }

  return(saldo_valor_ep)
}

