#' @title Multa por Atraso do Aditivo 3
#'
#' @description Computes the fine due to delay in construction, as per in "aditivo 3"
#'
#' @param vec_meses_atrasados_aditivo3 A vector with numbers - number of delay (in months) for each tranche. There are three tranches.
#'
#' @return total payment for the informed number of months delayed
#'
#' @examples  aditivo3(vec_meses_atrasados_aditivo3= c(10, 14, NA))
#'
#' @export aditivo3

aditivo3 <- function(vec_meses_atrasados_aditivo3) {
  # meses atrasados por ordem Se um trecho não estiver atrasado, colocar NA
  vec_meses_atrasados_aditivo3 <- ifelse(is.na(vec_meses_atrasados_aditivo3), 0,vec_meses_atrasados_aditivo3)

  # multa da fase 1, aditivo
  total <- sum(1740000*vec_meses_atrasados_aditivo3)

  return(total)
}
