#' cpv2017: Censo de Población y Vivienda 2017 del Perú para el tidyverse
#'
#' Flujo de trabajo:
#' 1. [cpv_convertir()] convierte una sola vez la base REDATAM a Parquet.
#' 2. [cpv_tbl()] abre una tabla (persona, hogar, vivienda...) para
#'    consultarla con dplyr sin cargarla en memoria.
#' 3. [cpv_variables()] y [cpv_etiquetas()] ayudan a encontrar variables y
#'    entender sus códigos.
#' 4. [cpv_etiquetar()], [cpv_na()] y [cpv_geo()] dan formato a los resultados.
#'
#' La variable `factorpond` de la tabla persona es el factor de ponderación
#' del INEI: la suma de `factorpond` estima la población total (con omisión
#' censal), mientras que el conteo de filas es la población censada.
#'
#' @keywords internal
#' @importFrom rlang .data :=
"_PACKAGE"
