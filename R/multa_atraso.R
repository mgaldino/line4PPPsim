#' @title Simula multa por atraso em diferentes versoes do contrato
#'
#' @description Simula multa por atraso em diferentes versoes do contrato
#'
#' @param num_meses_atraso_phase1 Number of months delayed for phase 1 (oiriginal contract)
#' @param num_meses_atraso_phase2 Number of months delayed for phase 2 (oiriginal contract)
#' @param num_meses_atraso_phase2_equipamento Number of months delayed for equipment for phase 2 (oiriginal contract)
#' @param vec_meses_atrasados_aditivo3 Vector number of months delayed for each block (trecho) for phase 1 in 'aditivo 3'
#' @param vec_meses_atrasados_aditivo4_phase1 Vector with number of months delayed for each block (trecho) for phase 1 in 'aditivo 4'
#' @param type character. One of "contrato_original", "aditivo_3" or "aditivo_4"
#'
#' @return total payment for the informed number of months delayed
#'
#' @examples  multa_atraso(num_meses_atraso_phase2 = 12,
#' num_meses_atraso_phase2_equipamento = 12,
#' vec_meses_atrasados_aditivo3 = c(18, 14, NA), type= "aditivo_3")
#'
#' @export multa_atraso

multa_atraso <- function(num_meses_atraso_phase1, num_meses_atraso_phase2,
                         num_meses_atraso_phase2_equipamento, vec_meses_atrasados_aditivo3,
                         vec_meses_atrasados_aditivo4_phase1, type ) {

  switch(type,
         contrato_original = atraso_contrato_phase1(num_meses_atraso_phase1) + atraso_contrato_phase2(num_meses_atraso_phase2, num_meses_atraso_phase2_equipamento)),
         aditivo_3 = aditivo3(vec_meses_atrasados_aditivo3) + atraso_contrato_phase2(num_meses_atraso_phase2, num_meses_atraso_phase2_equipamento),
         aditivo_4 = aditivo4(vec_meses_atrasados_aditivo4_phase1) + atraso_contrato_phase2(num_meses_atraso_phase2, num_meses_atraso_phase2_equipamento))

}
