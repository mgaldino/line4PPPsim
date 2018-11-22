#' @title Interest paid in long term loan
#'
#' @description computes interest paid for long term loan
#'
#' @param type_lp A number, either 1 or 2 (for type of long term loan)
#' @param num_round a number to round numbers. If NA, there is no rounding.
#'
#' @return a vector with interest paid for long tearm loan per period
#'
#' @examples  gen_interest_lp(type_lp = 2)
#'
#' @export gen_interest_lp

gen_interest_lp <- function(type_lp = 1, num_round=0) {

  libor <- c(.0512, .0296878, .0154844, .0092379, .009, .0100226, .0067306, .006288, .01178, .0137473,
             .0178692, .0280, .0325, .0335, rep(.0335, 19))

  valor_fin_lp1 <- 365613
  valor_fin_lp2 <- 352896

  if (type_lp == 1 ) {
    amortizacao_fin_lp <- c(0, 0, 0, 0, rep(45701.625, 8), rep(0, 21) )
    parcela_fin_lp1 <- c(0, .293999, .469083, .2369171, rep(0, 29))
    fin_lp <-  valor_fin_lp1*parcela_fin_lp1
    tx_fin_lp <- .0275 + libor
  }

  if (type_lp == 2 ) {
    amortizacao_fin_lp <- c(rep(0, 9), rep(70579.2, 5), rep(0, 19) )
    parcela_fin_lp2 <- c(rep(0, 5), .2656335, .5216154, .1846011, .0281499, rep(0, 24) )
    fin_lp <- valor_fin_lp2*parcela_fin_lp2
    tx_fin_lp <- .025 + libor
  }

  saldo_valor_fin_lp <- gera_saldo_fin(amortizacao_fin_lp, fin_lp, num_round)
  saldo_fin_lp_media <- gera_media_saldo(saldo_valor_fin_lp)

  pi_fin_lp <- tx_fin_lp*saldo_fin_lp_media
  return(pi_fin_lp)

}
