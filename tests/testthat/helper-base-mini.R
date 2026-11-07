# Escritores mínimos de archivos REDATAM, para generar bases de prueba.

escribir_pck <- function(valores, nbits, archivo) {
  bits <- as.vector(vapply(valores, function(v) as.integer(intToBits(v))[nbits:1], integer(nbits)))
  bits <- c(bits, integer((32 - length(bits) %% 32) %% 32))
  palabras <- matrix(bits, nrow = 32)
  bytes <- apply(palabras, 2, function(w) {
    b <- matrix(w, nrow = 8)                       # columnas: byte más significativo primero
    rev(colSums(b * 2^(7:0)))                      # little-endian
  })
  writeBin(as.raw(as.vector(bytes)), archivo)
}

escribir_chr <- function(texto, ancho, archivo) {
  raw <- unlist(lapply(texto, function(t) {
    b <- charToRaw(iconv(t, "UTF-8", "CP1252"))
    c(b, rep(charToRaw(" "), ancho - length(b)))
  }))
  writeBin(raw, archivo)
}

escribir_ptr <- function(ptr, archivo) writeBin(as.integer(ptr), archivo, size = 4, endian = "little")

etiquetas_xml <- function(valores, etiquetas) {
  paste0("<valueLabels>",
         paste0("<valueLabelElement><value>", valores, "</value><label>", etiquetas,
                "</label></valueLabelElement>", collapse = ""),
         "</valueLabels>")
}

var_xml <- function(nombre, formato, tam, archivo, extra = "") {
  sprintf("<variable><name>%s</name><label>Etiqueta %s</label>%s<filename>.\\%s</filename><varDicChoice><datasetType>%s</datasetType><datasetSize>%d</datasetSize></varDicChoice></variable>",
          nombre, nombre, extra, archivo, formato, tam)
}

# 2 departamentos, 3 viviendas (la 2.ª sin personas) y 5 personas.
crear_base_mini <- function(dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- function(x) file.path(dir, x)
  escribir_ptr(c(0, 2), f("M-1.ptr"))
  escribir_ptr(c(0, 2, 3), f("M-2.ptr"))
  escribir_ptr(c(0, 2, 2, 5), f("M-3.ptr"))
  escribir_chr(c("01", "02"), 2, f("ccdd.rbf"))
  escribir_chr(c("Departamento: Amazonas", "Departamento: Áncash"), 30, f("nccdd.rbf"))
  escribir_pck(c(1, 5, 2), 3, f("v1.rbf"))
  escribir_pck(c(1, 2, 3, 2, 0), 2, f("sexo.rbf"))
  escribir_pck(c(0, 45, 115, 7, 30), 7, f("edad.rbf"))
  writeBin(c(1.5, 1, 0.98, 1.2, 1.1), f("fp.rbf"), size = 8, endian = "little")

  sexo_extra <- paste0("<range><rangeMin>1</rangeMin><rangeMax>2</rangeMax></range><notappl>0</notappl><missing>3</missing>",
                       etiquetas_xml(1:2, c("Hombre", "Mujer")))
  edad_extra <- paste0("<range><rangeMin>0</rangeMin><rangeMax>115</rangeMax></range><notappl>116</notappl><missing>117</missing>",
                       etiquetas_xml(0:5, paste("Edad", 0:5, "años")))
  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?><redDictionary_XML><root><name>MINI</name>',
    '<entity><name>DEPARTAM</name><label>Departamento</label><refCode>CCDD</refCode><refLabel>NCCDD</refLabel><filename>.\\M-1.ptr</filename>',
    var_xml("CCDD", "CHR", 2, "ccdd.rbf"), var_xml("NCCDD", "CHR", 30, "nccdd.rbf"),
    '<entity><name>VIVIENDA</name><label>Vivienda</label><filename>.\\M-2.ptr</filename>',
    var_xml("V1", "PCK", 3, "v1.rbf", etiquetas_xml(1:5, c("Uno", "Dos", "Tres", "Cuatro", "Cinco"))),
    '<entity><name>PERSONA</name><label>Persona</label><filename>.\\M-3.ptr</filename>',
    var_xml("SEXO", "PCK", 2, "sexo.rbf", sexo_extra),
    var_xml("EDAD", "PCK", 7, "edad.rbf", edad_extra),
    var_xml("FP", "DBL", 64, "fp.rbf"),
    "</entity></entity></entity></root></redDictionary_XML>")
  writeLines(enc2utf8(xml), f("MINI.dicX"), useBytes = TRUE)
  f("MINI.dicX")
}
