#' @title Roll average for balance
#'
#' @description Auxuliar function
#'
#' @param saldo A vector of balance data
#'
#' @return mean of balances
#'
#' @examples  meu_saldo <- gera_saldo_ponte(c(0, 100, 0), c(100, 100, 100))
#' gera_saldo_ponte(meu_saldo)
#'
#' @export gera_media_saldo

gera_media_saldo <- function(saldo) {
  saldo_media <- numeric()
  saldo_media[1] <- mean(saldo[1])

  n <- length(saldo)
  for ( i in 2:n) {
    saldo_media[i] <- mean(c(saldo[i-1], saldo[i]))
  }
  return(saldo_media)
}
