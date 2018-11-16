#' @title Multa por Atraso Aditivo 4
#'
#' @description Computes the fine due to delay in construction, as per in "aditivo 4"
#'
#' @param num_meses_atraso a number - number of months that phase 2 is delayed (asinoriginal contract)
#' @param vec_meses_atrasados A vector with numbers - number of delay (in months) for each tranche. There are three tranches.
#'
#' @return total payment for the informed number of months delayed and delay per tranche
#'
#' @examples  aditivo4(num_meses_atraso = 10,
#' vec_meses_atrasados= c(10, 14, NA))
#'
#' @export aditivo4


aditivo4 <- function(num_meses_atraso, vec_meses_atrasados) {
  vec_meses_atrasados <- ifelse(is.na(vec_meses_atrasados), 0,vec_meses_atrasados)

  total <- sum(c(1740000, 580000, 1740000) *vec_meses_atrasados)
  return(total)

  # multa da fase 2, do contrato original
  multa2 <- 2335000*num_meses_atraso

  atraso2 <- ifelse(num_meses_atraso > 12, 12,num_meses_atraso)
  multa3 <- 1167500*atraso2

  total_final <- total + multa2 + multa3
  return(total_final)
}
