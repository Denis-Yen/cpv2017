#' Convertir la base REDATAM del censo a Parquet
#'
#' Lee la base REDATAM del CPV 2017 directamente desde sus archivos (`.dicX`,
#' `.ptr` y `.rbf`) y escribe un archivo Parquet comprimido por entidad, más
#' el diccionario de variables y etiquetas. Se ejecuta una sola vez; después
#' los datos se consultan con [cpv_tbl()].
#'
#' Cada tabla incluye su identificador (`persona_id`, `hogar_id`, ...), los
#' identificadores de sus entidades superiores (por ejemplo, `persona` trae
#' `hogar_id` y `vivienda_id`) y, en vivienda, hogar y persona, el `ubigeo`
#' del distrito (6 dígitos). Los valores se guardan con los códigos originales
#' de REDATAM, incluidos los de "no aplica" y "sin dato" (ver [cpv_na()]).
#'
#' @param dicx Ruta al diccionario `.dicX` de la base REDATAM. Los `.rbf` y
#'   `.ptr` deben estar en la misma carpeta.
#' @param destino Carpeta donde se escriben los Parquet. Por defecto,
#'   [cpv_dir()].
#' @param sobrescribir Si es `TRUE`, reemplaza una conversión existente.
#' @return La carpeta de destino, de forma invisible.
#' @export
#' @examples
#' \dontrun{
#' cpv_convertir("D:/CP2017PER/BaseD/BaseR/CPVPER2017D.dicX")
#' }
cpv_convertir <- function(dicx, destino = cpv_dir(), sobrescribir = FALSE) {
  dicx <- normalizePath(dicx, mustWork = TRUE)
  if (!grepl("\\.dicx$", dicx, ignore.case = TRUE)) {
    stop("Se necesita el diccionario en formato .dicX (XML).", call. = FALSE)
  }
  if (length(list.files(destino, "\\.parquet$")) && !sobrescribir) {
    stop("Ya existen archivos Parquet en ", destino,
         ". Usa `sobrescribir = TRUE` para reemplazarlos.", call. = FALSE)
  }
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  base <- dirname(dicx)
  t0 <- Sys.time()

  dic <- leer_dicx(dicx)
  ent <- dic$entidades
  vars <- dic$variables

  ubigeo <- list()  # código geográfico acumulado de cada registro, por entidad
  ancest <- list()  # ids de entidades superiores no geográficas, por entidad
  ent$n_registros <- NA_integer_

  for (i in seq_len(nrow(ent))) {
    e <- ent[i, ]
    ptr <- leer_ptr(file.path(base, e$archivo))
    n <- ptr[length(ptr)]
    ent$n_registros[i] <- n
    padre_id <- padre_desde_ptr(ptr)
    if (length(padre_id) != n) stop("Punteros inconsistentes en ", e$entidad, call. = FALSE)

    cols <- list()
    cols[[paste0(e$entidad, "_id")]] <- arrow::Array$create(seq_len(n), type = arrow::int32())
    tiene_padre <- !is.na(e$padre)
    if (tiene_padre) {
      cols[[paste0(e$padre, "_id")]] <- arrow::Array$create(padre_id, type = arrow::int32())
      padre_geo <- ent$geografica[ent$entidad == e$padre]
      ancest[[e$entidad]] <- c(
        if (!padre_geo) stats::setNames(list(padre_id), e$padre),
        lapply(ancest[[e$padre]], function(a) a[padre_id])
      )
      for (a in setdiff(names(ancest[[e$entidad]]), e$padre)) {
        cols[[paste0(a, "_id")]] <- arrow::Array$create(ancest[[e$entidad]][[a]], type = arrow::int32())
      }
    }

    dv <- vars[vars$entidad == e$entidad, ]
    valores <- list()
    for (j in seq_len(nrow(dv))) {
      v <- dv[j, ]
      ruta <- file.path(base, v$archivo)
      valores[[v$variable]] <- switch(
        v$formato,
        PCK = leer_pck(ruta, v$bits, n),
        CHR = leer_chr(ruta, v$bits, n),
        DBL = leer_dbl(ruta, n),
        { warning("Formato ", v$formato, " no soportado; se omite ", v$variable, call. = FALSE); NULL }
      )
    }

    # Código geográfico (ubigeo) y nombre limpio.
    if (e$geografica) {
      cod <- valores[[e$cod_ref]]
      ubigeo[[e$entidad]] <- if (tiene_padre && !is.null(ubigeo[[e$padre]])) {
        paste0(ubigeo[[e$padre]][padre_id], cod)
      } else cod
      cols$ubigeo <- arrow::Array$create(ubigeo[[e$entidad]])
      if (!is.na(e$nom_ref) && !is.null(valores[[e$nom_ref]])) {
        cols$nombre <- arrow::Array$create(sub(".*: ", "", valores[[e$nom_ref]]))
      }
    } else if (tiene_padre && !is.null(ubigeo[[e$padre]])) {
      ubigeo[[e$entidad]] <- ubigeo[[e$padre]][padre_id]
      cols$ubigeo <- arrow::Array$create(ubigeo[[e$entidad]])
    }

    for (nm in names(valores)) {
      cols[[nm]] <- arrow::Array$create(valores[[nm]], type = tipo_arrow(valores[[nm]]))
      valores[[nm]] <- NA
    }
    tabla <- do.call(arrow::Table$create, cols)
    rm(cols, valores)
    arrow::write_parquet(tabla, file.path(destino, paste0(e$entidad, ".parquet")),
                         compression = "zstd", compression_level = 9)
    rm(tabla)
    invisible(gc())
    message(sprintf("%-9s %11s registros  %3d variables  (%s)", e$entidad,
                    format(n, big.mark = ","), nrow(dv), format_tiempo(t0)))

    # Libera lo que ya no necesitan las entidades hijas.
    hijos <- ent$entidad[!is.na(ent$padre) & ent$padre == e$padre]
    if (tiene_padre && all(hijos %in% ent$entidad[seq_len(i)])) {
      ubigeo[[e$padre]] <- NULL
      ancest[[e$padre]] <- NULL
    }
  }

  arrow::write_parquet(ent[, c("entidad", "padre", "etiqueta", "geografica", "n_registros")],
                       file.path(destino, "diccionario_entidades.parquet"))
  arrow::write_parquet(vars[, c("entidad", "variable", "alias", "etiqueta", "tipo", "grupo",
                                "rango_min", "rango_max", "no_aplica", "sin_dato")],
                       file.path(destino, "diccionario_variables.parquet"))
  arrow::write_parquet(dic$etiquetas, file.path(destino, "diccionario_etiquetas.parquet"))
  limpiar_cache()
  message("Conversión terminada en ", format_tiempo(t0), ": ", destino)
  invisible(destino)
}

tipo_arrow <- function(x) {
  if (is.character(x)) return(arrow::utf8())
  if (is.double(x)) return(arrow::float64())
  r <- range(x, na.rm = TRUE)
  if (r[1] >= -128 && r[2] <= 127) arrow::int8()
  else if (r[1] >= -32768 && r[2] <= 32767) arrow::int16()
  else arrow::int32()
}

format_tiempo <- function(t0) {
  s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (s < 60) sprintf("%.0f s", s) else sprintf("%.1f min", s / 60)
}
