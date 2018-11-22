#' @title Financiamento de longo prazo 2
#'
#' @description Funcao auxiliar que gera vetor de financiamento de longo prazo, para computar juros
#'
#' @return a vector financiamento de longo prazo
#'
#' @examples  gen_fin_lp2()
#'
#' @export gen_fin_lp2

gen_fin_lp2 <- function() {
  valor_fin_lp2 <- 352896
  parcela_fin_lp2 <- c(rep(0, 5), .2656335, .5216154, .1846011, .0281499, rep(0, 24) )
  fin_lp <- valor_fin_lp2*parcela_fin_lp2
  return(fin_lp)
}
