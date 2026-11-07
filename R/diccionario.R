# Lectura del diccionario REDATAM en formato XML (.dicX, UTF-8).

xml_txt <- function(nodo, ruta) {
  r <- xml2::xml_find_first(nodo, ruta)
  if (inherits(r, "xml_missing")) NA_character_ else xml2::xml_text(r)
}

xml_num <- function(nodo, ruta) suppressWarnings(as.numeric(xml_txt(nodo, ruta)))

archivo_rbf <- function(x) basename(gsub("\\\\", "/", x))

# Una variable es "numérica" (cantidad: edad, horas, nro. de habitaciones...)
# cuando la mayoría de sus etiquetas contienen su propio valor como número;
# en las categóricas (sexo, material de paredes...) eso no ocurre.
es_numerica <- function(valor, etiqueta) {
  if (length(valor) < 5) return(FALSE)
  contiene <- mapply(function(v, e) grepl(paste0("(^|\\D)", v, "(\\D|$)"), e),
                     sub("^0+(?=\\d)", "", valor, perl = TRUE), etiqueta)
  mean(contiene) >= 0.8
}

leer_dicx <- function(dicx) {
  x <- xml2::read_xml(dicx)

  ent_xml <- xml2::xml_find_all(x, "//entity")
  entidades <- tibble::tibble(
    entidad   = tolower(vapply(ent_xml, xml_txt, "", "./name")),
    padre     = vapply(ent_xml, function(e) {
      p <- xml2::xml_parent(e)
      if (xml2::xml_name(p) == "entity") tolower(xml_txt(p, "./name")) else NA_character_
    }, ""),
    etiqueta  = vapply(ent_xml, xml_txt, "", "./label"),
    archivo   = archivo_rbf(vapply(ent_xml, xml_txt, "", "./filename")),
    cod_ref   = tolower(vapply(ent_xml, xml_txt, "", "./refCode")),
    nom_ref   = tolower(vapply(ent_xml, xml_txt, "", "./refLabel"))
  )
  entidades$geografica <- !is.na(entidades$cod_ref)

  var_xml <- xml2::xml_find_all(x, "//entity/variable")
  variables <- tibble::tibble(
    entidad   = tolower(vapply(var_xml, function(v) xml_txt(xml2::xml_parent(v), "./name"), "")),
    variable  = tolower(vapply(var_xml, xml_txt, "", "./name")),
    alias     = vapply(var_xml, xml_txt, "", "./alias"),
    etiqueta  = vapply(var_xml, xml_txt, "", "./label"),
    grupo     = vapply(var_xml, xml_txt, "", "./group"),
    formato   = vapply(var_xml, xml_txt, "", "./varDicChoice/datasetType"),
    bits      = as.integer(vapply(var_xml, xml_txt, "", "./varDicChoice/datasetSize")),
    archivo   = archivo_rbf(vapply(var_xml, xml_txt, "", "./filename")),
    rango_min = vapply(var_xml, xml_num, 0, "./range/rangeMin"),
    rango_max = vapply(var_xml, xml_num, 0, "./range/rangeMax"),
    no_aplica = vapply(var_xml, xml_num, 0, "./notappl"),
    sin_dato  = vapply(var_xml, xml_num, 0, "./missing")
  )

  etiquetas <- do.call(rbind, lapply(var_xml, function(v) {
    el <- xml2::xml_find_all(v, "./valueLabels/valueLabelElement")
    if (!length(el)) return(NULL)
    tibble::tibble(
      variable = tolower(xml_txt(v, "./name")),
      valor    = xml2::xml_text(xml2::xml_find_first(el, "./value")),
      etiqueta = xml2::xml_text(xml2::xml_find_first(el, "./label"))
    )
  }))

  variables$tipo <- vapply(variables$variable, function(v) {
    e <- etiquetas[etiquetas$variable == v, ]
    if (!nrow(e)) "sin etiquetas" else if (es_numerica(e$valor, e$etiqueta)) "numérica" else "categórica"
  }, "", USE.NAMES = FALSE)

  if (anyDuplicated(variables$variable)) {
    stop("Hay nombres de variable repetidos entre entidades.", call. = FALSE)
  }
  list(entidades = entidades, variables = variables, etiquetas = etiquetas)
}
