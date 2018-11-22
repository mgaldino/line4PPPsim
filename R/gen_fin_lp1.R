#' @title Interest paid in emprestimo ponte
#'
#' @description computes interest paid for emprestimo ponte
#'
#' @return a vector with interest paid for 'long tearm loan'emprestimo ponte' per period
#'
#' @examples  gen_interest_ep()
#'
#' @export gen_interest_ep

gen_fin_lp1 <- function() {
  valor_fin_lp1 <- 365613
  parcela_fin_lp1 <- c(0, .293999, .469083, .2369171, rep(0, 29))
  fin_lp <-  valor_fin_lp1*parcela_fin_lp1
  return(fin_lp)
}
