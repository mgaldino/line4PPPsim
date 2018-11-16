#' @title Multa por Atraso Contrato Original
#'
#' @description Computes the fine due to delay in construction, as per in original contract
#'
#' @param num_meses_atraso_contrato A number
#'
#' @return total payment for the informed number of months delayed
#'
#' @examples  atraso_contrato(10)
#'
#' @export atraso_contrato

atraso_contrato <- function(num_meses_atraso_contrato = 18) {

    atraso1 <- ifelse(num_meses_atraso_contrato > 18, 18,num_meses_atraso_contrato)
    multa1 <- 5220000*atraso1

    multa2 <- 2335000*num_meses_atraso_contrato


    atraso2 <- ifelse(num_meses_atraso_contrato > 12, 12,num_meses_atraso_contrato)
    multa3 <- 1167500*atraso2

    total_final <- multa1 + multa2 + multa3
    return(total_final)
  }
