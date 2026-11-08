# Genera la muestra incluida en el paquete (inst/extdata/muestra): todas las
# tablas restringidas a dos distritos pequeños, con el diccionario completo.
# Se ejecuta desde la carpeta del paquete con los datos completos disponibles.
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
                       compression = "zstd", compression_level = 9)
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
for (f in list.files(origen, "^diccionario_.*[.]parquet$")) {
  file.copy(file.path(origen, f), file.path(destino, f), overwrite = TRUE)
}
