#' @title Financiamento de longo prazo 1
#'
#' @description Funcao auxiliar que gera vetor de financiamento de longo prazo, para computar juros
#'
#' @return a vector financiamento de longo prazo
#'
#' @examples  gen_fin_lp1()
#'
#' @export gen_fin_lp1

gen_fin_lp1 <- function() {
  valor_fin_lp1 <- 365613
  parcela_fin_lp1 <- c(0, .293999, .469083, .2369171, rep(0, 29))
  fin_lp <-  valor_fin_lp1*parcela_fin_lp1
  return(fin_lp)
}
