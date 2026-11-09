# Genera la muestra incluida en el paquete (inst/extdata/muestra): todas las
# tablas restringidas a dos distritos pequeños, con el diccionario completo.
# Se ejecuta desde la carpeta del paquete con los datos completos disponibles.
#
# Los archivos se guardan SIN compresión: algunas compilaciones mínimas de
# 'arrow' en Linux (por ejemplo, la de los servidores de CRAN) no traen zstd
# ni snappy, y la muestra debe poder leerse en cualquier instalación.
library(dplyr)
origen <- cpv2017::cpv_dir()
destino <- "inst/extdata/muestra"
distritos <- c("040403", "151013")   # Ayo (Arequipa) y Huampara (Lima)

guardar <- function(tabla, filtro) {
  n <- nchar(filtro[1])
  d <- arrow::open_dataset(file.path(origen, paste0(tabla, ".parquet"))) |>
    filter(substr(ubigeo, 1, !!n) %in% filtro) |>
    arrange(.data[[paste0(tabla, "_id")]]) |>
    compute()
  arrow::write_parquet(d, file.path(destino, paste0(tabla, ".parquet")),
                       compression = "uncompressed")
  d$num_rows
}
print(c(
  persona  = guardar("persona", distritos),
  hogar    = guardar("hogar", distritos),
  vivienda = guardar("vivienda", distritos),
  distrito = guardar("distrito", distritos),
  provinci = guardar("provinci", unique(substr(distritos, 1, 4))),
  departam = guardar("departam", unique(substr(distritos, 1, 2)))
))
for (f in c("entidades", "variables", "etiquetas")) {
  nombre <- paste0("diccionario_", f, ".parquet")
  arrow::write_parquet(arrow::read_parquet(file.path(origen, nombre)),
                       file.path(destino, nombre), compression = "uncompressed")
}
