# URL predeterminada de los datos publicados (carpeta o release que contiene
# manifiesto.csv y los .parquet).
URL_DATOS <- "https://github.com/Denis-Yen/cpv2017/releases/download/datos-v1"

#' URL de descarga de los datos convertidos
#'
#' Se toma, en este orden, de la opción `cpv2017.url`, de la variable de
#' entorno `CPV2017_URL` o de la URL predeterminada del paquete.
#'
#' @return Una cadena con la URL base (puede estar vacía).
#' @keywords internal
#' @export
cpv_url <- function() {
  getOption("cpv2017.url", Sys.getenv("CPV2017_URL", URL_DATOS))
}

#' Descargar los datos ya convertidos
#'
#' Descarga los archivos Parquet del censo ya convertidos, para no tener que
#' conseguir la base REDATAM ni ejecutar [cpv_convertir()]. Se ejecuta una
#' sola vez; después los datos se consultan con [cpv_tbl()].
#'
#' La descarga se guía por un `manifiesto.csv` publicado junto a los datos,
#' con el tamaño y la firma MD5 de cada archivo. Cada archivo se verifica al
#' terminar de bajarse; si no coincide, se descarta y la función se detiene.
#' Los archivos ya descargados y correctos no se vuelven a bajar, así que si la
#' conexión se corta basta con volver a ejecutar la función.
#'
#' @param destino Carpeta donde se guardan los datos. Por defecto, [cpv_dir()].
#' @param url URL base donde están `manifiesto.csv` y los `.parquet` (por
#'   ejemplo, la de un release de GitHub:
#'   `https://github.com/usuario/repo/releases/download/datos-v1`).
#'   Por defecto, [cpv_url()].
#' @param sobrescribir Si es `TRUE`, vuelve a descargar todo aunque los
#'   archivos ya existan y estén correctos.
#' @return La carpeta de destino, de forma invisible.
#' @export
#' @examples
#' \dontrun{
#' # Descarga unos 940 MB
#' cpv_descargar()
#' cpv_tbl("persona") |> dplyr::count() |> dplyr::collect()
#' }
cpv_descargar <- function(destino = cpv_dir(), url = cpv_url(), sobrescribir = FALSE) {
  if (is.null(url) || !nzchar(url)) {
    stop("No hay una URL de descarga configurada.\n",
         "Indica `url = ...`, define la variable de entorno CPV2017_URL ",
         "o convierte la base REDATAM con cpv_convertir().", call. = FALSE)
  }
  url <- sub("/+$", "", url)
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  anterior <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(anterior), add = TRUE)

  man_tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(man_tmp), add = TRUE)
  bajar(paste0(url, "/manifiesto.csv"), man_tmp, quiet = TRUE)
  man <- leer_manifiesto(man_tmp)

  total <- sum(man$bytes)
  message(sprintf("Datos del CPV 2017: %d archivos, %s.", nrow(man), formato_bytes(total)))

  for (i in seq_len(nrow(man))) {
    a <- man[i, ]
    final <- file.path(destino, a$archivo)
    if (!sobrescribir && file.exists(final) && archivo_ok(final, a)) {
      message(sprintf("  %-32s ya descargado", a$archivo))
      next
    }
    message(sprintf("  %-32s %s", a$archivo, formato_bytes(a$bytes)))
    parcial <- paste0(final, ".parcial")
    on.exit(unlink(parcial), add = TRUE)
    bajar(paste0(url, "/", a$archivo), parcial, quiet = a$bytes < 5e6)
    if (!archivo_ok(parcial, a)) {
      unlink(parcial)
      stop("El archivo ", a$archivo, " lleg\u00f3 incompleto o da\u00f1ado (no coincide su ",
           "tama\u00f1o o firma MD5). Vuelve a ejecutar cpv_descargar().", call. = FALSE)
    }
    unlink(final)
    file.rename(parcial, final)
  }
  file.copy(man_tmp, file.path(destino, "manifiesto.csv"), overwrite = TRUE)
  limpiar_cache()
  message("Listo: ", normalizePath(destino, winslash = "/"))
  invisible(destino)
}

#' Escribir el manifiesto para publicar los datos
#'
#' Para quien publica los datos: genera `manifiesto.csv` (nombre, tamaño y
#' firma MD5 de cada `.parquet`) en la carpeta de los datos convertidos. Se
#' sube junto con los `.parquet` al lugar de publicación, y
#' [cpv_descargar()] lo usa para verificar cada archivo.
#'
#' @param dir Carpeta con los `.parquet`. Por defecto, [cpv_dir()].
#' @return El manifiesto (tibble), de forma invisible.
#' @export
cpv_manifiesto <- function(dir = cpv_dir()) {
  archivos <- sort(list.files(dir, pattern = "\\.parquet$"))
  if (!length(archivos)) stop("No hay archivos .parquet en ", dir, call. = FALSE)
  rutas <- file.path(dir, archivos)
  man <- tibble::tibble(archivo = archivos,
                        bytes = file.size(rutas),
                        md5 = unname(tools::md5sum(rutas)))
  utils::write.csv(man, file.path(dir, "manifiesto.csv"), row.names = FALSE)
  invisible(man)
}

# ---- Auxiliares --------------------------------------------------------------

bajar <- function(url, archivo, quiet = FALSE) {
  r <- tryCatch(utils::download.file(url, archivo, mode = "wb", quiet = quiet),
                error = function(e) e, warning = function(w) w)
  if (inherits(r, "condition") || !identical(as.integer(r), 0L)) {
    unlink(archivo)
    detalle <- if (inherits(r, "condition")) conditionMessage(r) else paste("c\u00f3digo", r)
    stop("No se pudo descargar ", url, "\n", detalle, call. = FALSE)
  }
}

leer_manifiesto <- function(archivo) {
  man <- utils::read.csv(archivo, colClasses = "character")
  if (!all(c("archivo", "bytes", "md5") %in% names(man)) || !nrow(man)) {
    stop("El manifiesto descargado no es v\u00e1lido.", call. = FALSE)
  }
  if (any(grepl("[/\\\\]|^\\.", man$archivo))) {
    stop("El manifiesto contiene nombres de archivo no permitidos.", call. = FALSE)
  }
  man$bytes <- as.numeric(man$bytes)
  man
}

archivo_ok <- function(ruta, fila) {
  file.size(ruta) == fila$bytes && unname(tools::md5sum(ruta)) == fila$md5
}

formato_bytes <- function(b) {
  if (b >= 1e9) sprintf("%.2f GB", b / 1e9)
  else if (b >= 1e6) sprintf("%.0f MB", b / 1e6)
  else sprintf("%.0f KB", b / 1e3)
}
