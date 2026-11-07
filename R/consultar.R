#' Carpeta de los datos convertidos
#'
#' `cpv_dir()` devuelve la carpeta donde están (o se escribirán) los Parquet.
#' Se toma, en este orden, de la opción `cpv2017.dir`, de la variable de
#' entorno `CPV2017_DIR` o de la carpeta de datos del usuario
#' (`tools::R_user_dir("cpv2017", "data")`). Para fijarla de forma permanente,
#' agrega `CPV2017_DIR=ruta/a/los/parquet` a tu archivo `.Renviron`.
#'
#' `cpv_usar_dir()` la cambia para la sesión actual.
#'
#' @param ruta Carpeta con los archivos Parquet.
#' @return La ruta de la carpeta.
#' @export
cpv_dir <- function() {
  getOption("cpv2017.dir",
            Sys.getenv("CPV2017_DIR", tools::R_user_dir("cpv2017", "data")))
}

#' @rdname cpv_dir
#' @export
cpv_usar_dir <- function(ruta) {
  ruta <- normalizePath(ruta, mustWork = TRUE)
  options(cpv2017.dir = ruta)
  limpiar_cache()
  invisible(ruta)
}

alias_entidades <- c(
  departamento = "departam", departamentos = "departam", departam = "departam",
  provincia = "provinci", provincias = "provinci", provinci = "provinci",
  distrito = "distrito", distritos = "distrito",
  vivienda = "vivienda", viviendas = "vivienda",
  hogar = "hogar", hogares = "hogar",
  persona = "persona", personas = "persona", poblacion = "persona"
)

ruta_parquet <- function(nombre, dir = cpv_dir()) {
  f <- file.path(dir, paste0(nombre, ".parquet"))
  if (!file.exists(f)) {
    stop("No se encontró ", f, ".\nConvierte primero la base con cpv_convertir() ",
         "o indica la carpeta con cpv_usar_dir().", call. = FALSE)
  }
  f
}

#' Abrir una tabla del censo para consultarla con dplyr
#'
#' Devuelve la tabla como un conjunto de datos de 'arrow' sin cargarla en
#' memoria. Se usa con los verbos de dplyr (`filter()`, `select()`,
#' `mutate()`, `group_by()`, `summarise()`, `count()`, ...) y el resultado se
#' trae a R con `collect()`.
#'
#' @param entidad `"persona"`, `"hogar"`, `"vivienda"`, `"distrito"`,
#'   `"provincia"` o `"departamento"` (también en plural).
#' @param dir Carpeta de los Parquet. Por defecto, [cpv_dir()].
#' @return Un `arrow::Dataset`.
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Población censada por sexo en Lima (departamento 15)
#' cpv_tbl("persona") |>
#'   filter(substr(ubigeo, 1, 2) == "15") |>
#'   count(c5p2) |>
#'   collect() |>
#'   cpv_etiquetar(c5p2)
#' }
cpv_tbl <- function(entidad, dir = cpv_dir()) {
  clave <- alias_entidades[tolower(entidad)]
  if (length(entidad) != 1 || is.na(clave)) {
    stop("Entidad desconocida: ", entidad, ". Usa una de: persona, hogar, vivienda, ",
         "distrito, provincia, departamento.", call. = FALSE)
  }
  arrow::open_dataset(ruta_parquet(clave, dir))
}

# ---- Diccionario (con caché por sesión) -------------------------------------
cache <- new.env(parent = emptyenv())
limpiar_cache <- function() rm(list = ls(cache), envir = cache)

leer_diccionario <- function(cual, dir = cpv_dir()) {
  clave <- paste(cual, dir)
  if (is.null(cache[[clave]])) {
    cache[[clave]] <- tibble::as_tibble(arrow::read_parquet(ruta_parquet(paste0("diccionario_", cual), dir)))
  }
  cache[[clave]]
}

#' Buscar variables en el diccionario
#'
#' @param buscar Texto o expresión regular que se busca (sin distinguir
#'   mayúsculas) en el nombre, alias y etiqueta de la variable. `NULL`
#'   devuelve todas.
#' @param entidad Limita la búsqueda a una entidad (ver [cpv_tbl()]).
#' @param dir Carpeta de los Parquet.
#' @return Un tibble con una fila por variable. La columna `tipo` indica si la
#'   variable es `"categórica"` o `"numérica"` (edad, horas, cantidades...).
#' @export
#' @examples
#' \dontrun{
#' cpv_variables("internet")
#' cpv_variables(entidad = "vivienda")
#' }
cpv_variables <- function(buscar = NULL, entidad = NULL, dir = cpv_dir()) {
  v <- leer_diccionario("variables", dir)
  if (!is.null(entidad)) {
    v <- v[v$entidad == alias_entidades[tolower(entidad)], ]
  }
  if (!is.null(buscar)) {
    texto <- paste(v$variable, v$alias, v$etiqueta)
    v <- v[grepl(buscar, texto, ignore.case = TRUE), ]
  }
  v
}

#' Ver las etiquetas (categorías) de una variable
#'
#' @param variable Nombre de la variable (por ejemplo `"c5p2"`).
#' @param dir Carpeta de los Parquet.
#' @return Un tibble con el código (`valor`) y su `etiqueta`.
#' @export
cpv_etiquetas <- function(variable, dir = cpv_dir()) {
  e <- leer_diccionario("etiquetas", dir)
  e[e$variable == tolower(variable), c("variable", "valor", "etiqueta")]
}

#' Reemplazar los códigos de "no aplica" y "sin dato" por NA
#'
#' REDATAM guarda con un código propio los casos en que la pregunta no aplica
#' (por ejemplo, el nivel educativo de un niño de 1 año) o no tiene respuesta.
#' Esta función los convierte en `NA`. Funciona tanto con datos ya traídos con
#' `collect()` como con una consulta sin ejecutar de [cpv_tbl()].
#'
#' @param data Un data frame o una consulta de [cpv_tbl()].
#' @param ... Columnas a tratar (sintaxis de `dplyr::select()`). Si no se
#'   indica ninguna, se tratan todas las que tengan esos códigos.
#' @param dir Carpeta de los Parquet.
#' @return `data` con los códigos reemplazados por `NA`.
#' @export
cpv_na <- function(data, ..., dir = cpv_dir()) {
  cols <- columnas(data, ..., dir = dir)
  v <- leer_diccionario("variables", dir)
  for (col in cols) {
    fila <- v[v$variable == col, ]
    if (!nrow(fila)) next
    codigos <- stats::na.omit(c(fila$no_aplica, fila$sin_dato))
    # Un código dentro del rango válido es un valor real (p. ej. 0 personas).
    if (!is.na(fila$rango_min)) {
      codigos <- codigos[codigos < fila$rango_min | codigos > fila$rango_max]
    }
    if (!length(codigos)) next
    data <- dplyr::mutate(data, !!col := dplyr::if_else(
      .data[[col]] %in% !!as.integer(codigos), NA, .data[[col]]))
  }
  data
}

#' Convertir códigos en etiquetas
#'
#' Reemplaza los códigos de las variables categóricas por sus etiquetas, como
#' factores ordenados según el código. Se aplica a datos ya traídos con
#' `collect()`.
#'
#' @param data Un data frame.
#' @param ... Columnas a etiquetar (sintaxis de `dplyr::select()`). Si no se
#'   indica ninguna, se etiquetan todas las variables categóricas presentes.
#' @param na Si es `TRUE` (por defecto), los códigos de "no aplica" y
#'   "sin dato" se convierten en `NA` en lugar de etiquetarse.
#' @param dir Carpeta de los Parquet.
#' @return `data` con las columnas etiquetadas como factores.
#' @export
#' @examples
#' \dontrun{
#' cpv_tbl("hogar") |>
#'   count(c3p213) |>
#'   collect() |>
#'   cpv_etiquetar()
#' }
cpv_etiquetar <- function(data, ..., na = TRUE, dir = cpv_dir()) {
  if (!is.data.frame(data) || inherits(data, c("arrow_dplyr_query", "ArrowObject"))) {
    stop("cpv_etiquetar() trabaja sobre datos en R: usa collect() antes.", call. = FALSE)
  }
  v <- leer_diccionario("variables", dir)
  e <- leer_diccionario("etiquetas", dir)
  cols <- if (...length()) columnas(data, ..., dir = dir) else
    intersect(names(data), v$variable[v$tipo == "categórica"])
  if (na) data <- cpv_na(data, tidyselect::all_of(cols), dir = dir)
  for (col in cols) {
    et <- e[e$variable == col, ]
    if (!nrow(et)) next
    x <- data[[col]]
    clave <- if (is.character(x)) et$valor else suppressWarnings(as.numeric(et$valor))
    ok <- !is.na(clave)
    clave <- clave[ok]; lab <- et$etiqueta[ok]
    sin <- setdiff(stats::na.omit(unique(x)), clave)  # códigos sin etiqueta
    niveles <- c(clave, sin)
    etiquetas <- c(lab, as.character(sin))
    orden <- order(niveles)
    data[[col]] <- factor(match(x, niveles[orden]), levels = seq_along(niveles),
                          labels = etiquetas[orden])
  }
  data
}

#' Tabla de ubigeos con sus nombres
#'
#' @param nivel `"distrito"` (por defecto), `"provincia"` o `"departamento"`.
#' @param dir Carpeta de los Parquet.
#' @return Un tibble con el `ubigeo` y los nombres de departamento, provincia y
#'   distrito, útil para unir (`left_join()`) con resultados agregados.
#' @export
#' @examples
#' \dontrun{
#' cpv_tbl("persona") |>
#'   count(ubigeo) |>
#'   collect() |>
#'   left_join(cpv_geo(), by = "ubigeo")
#' }
cpv_geo <- function(nivel = c("distrito", "provincia", "departamento"), dir = cpv_dir()) {
  nivel <- match.arg(nivel)
  leer <- function(x) tibble::as_tibble(arrow::read_parquet(ruta_parquet(x, dir)))
  dep <- leer("departam")[, c("departam_id", "ubigeo", "nombre")]
  names(dep) <- c("departam_id", "ubigeo_dep", "departamento")
  if (nivel == "departamento") return(stats::setNames(dep[, 2:3], c("ubigeo", "departamento")))
  prov <- leer("provinci")[, c("provinci_id", "departam_id", "ubigeo", "nombre")]
  names(prov)[3:4] <- c("ubigeo_prov", "provincia")
  prov <- dplyr::left_join(prov, dep, by = "departam_id")
  if (nivel == "provincia") {
    return(dplyr::select(prov, ubigeo = "ubigeo_prov", "departamento", "provincia"))
  }
  dist <- leer("distrito")[, c("provinci_id", "ubigeo", "nombre")]
  dist <- dplyr::left_join(dist, prov, by = "provinci_id")
  dplyr::select(dist, "ubigeo", "departamento", "provincia", distrito = "nombre")
}

columnas <- function(data, ..., dir) {
  if (...length()) {
    nombres <- if (inherits(data, c("arrow_dplyr_query", "Dataset", "ArrowTabular"))) {
      names(dplyr::select(data, ...))
    } else {
      names(tidyselect::eval_select(rlang::expr(c(...)), data))
    }
    return(nombres)
  }
  intersect(names(data), leer_diccionario("variables", dir)$variable)
}
