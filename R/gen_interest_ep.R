#' @title Interest paid in emprestimo ponte
#'
#' @description computes interest paid for emprestimo ponte
#'
#' @return a vector with interest paid for 'long tearm loan'emprestimo ponte' per period
#'
#' @examples  gen_interest_ep()
#'
#' @export gen_interest_ep

gen_interest_ep <- function() {
  valor_ep <- 17021.599
  amortizacao_ep <- c(0, 17021.599, rep(0, 31))
  ep <- rep(valor_ep, 33)
  libor <- c(.0512, .0296878, .0154844, .0092379, .009, .0100226, .0067306, .006288, .01178, .0137473,
             .0178692, .0280, .0325, .0335, rep(.0335, 19))


  saldo_valor_ep <- gera_saldo_ponte(amortizacao_ep, ep)
  saldo_ep_media <- gera_media_saldo(saldo_valor_ep)

  tx_pi_ep <- .03 + libor

  pi_ep <- tx_pi_ep*saldo_ep_media
  return(pi_ep)
}
