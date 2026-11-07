base_dir <- file.path(tempdir(), "base_mini")
out_dir <- file.path(tempdir(), "parquet_mini")
unlink(c(base_dir, out_dir), recursive = TRUE)
dicx <- crear_base_mini(base_dir)
suppressMessages(cpv_convertir(dicx, out_dir))

test_that("cpv_convertir escribe una tabla por entidad y el diccionario", {
  esperados <- c("departam", "vivienda", "persona", "diccionario_entidades",
                 "diccionario_variables", "diccionario_etiquetas")
  expect_setequal(sub("\\.parquet$", "", list.files(out_dir)), esperados)
  expect_error(cpv_convertir(dicx, out_dir), "sobrescribir")
})

test_that("las tablas tienen ids, enlaces y ubigeo correctos", {
  dep <- as.data.frame(cpv_tbl("departamento", out_dir) |> dplyr::collect())
  expect_identical(dep$nombre, c("Amazonas", "Áncash"))
  expect_identical(dep$ubigeo, c("01", "02"))

  viv <- as.data.frame(cpv_tbl("vivienda", out_dir) |> dplyr::collect())
  expect_identical(viv$vivienda_id, 1:3)
  expect_identical(viv$departam_id, c(1L, 1L, 2L))
  expect_identical(viv$ubigeo, c("01", "01", "02"))
  expect_identical(as.integer(viv$v1), c(1L, 5L, 2L))

  per <- as.data.frame(cpv_tbl("personas", out_dir) |> dplyr::collect())
  expect_identical(per$vivienda_id, c(1L, 1L, 3L, 3L, 3L))
  expect_identical(per$ubigeo, c("01", "01", "02", "02", "02"))
  expect_identical(as.integer(per$edad), c(0L, 45L, 115L, 7L, 30L))
  expect_equal(per$fp, c(1.5, 1, 0.98, 1.2, 1.1))
})

test_that("el diccionario clasifica variables y guarda etiquetas", {
  v <- cpv_variables(dir = out_dir)
  expect_identical(v$tipo[v$variable == "sexo"], "categórica")
  expect_identical(v$tipo[v$variable == "edad"], "numérica")
  expect_identical(nrow(cpv_variables("sexo", dir = out_dir)), 1L)
  expect_identical(cpv_etiquetas("sexo", out_dir)$etiqueta, c("Hombre", "Mujer"))
})

test_that("cpv_na y cpv_etiquetar dan formato a los códigos", {
  per <- cpv_tbl("persona", out_dir) |> dplyr::collect()
  et <- cpv_etiquetar(per, dir = out_dir)
  expect_s3_class(et$sexo, "factor")
  expect_identical(as.character(et$sexo), c("Hombre", "Mujer", NA, "Mujer", NA))
  expect_type(et$edad, "integer")  # numérica: no se etiqueta por defecto

  sin_na <- cpv_etiquetar(per, sexo, na = FALSE, dir = out_dir)
  expect_identical(as.character(sin_na$sexo), c("Hombre", "Mujer", "3", "Mujer", "0"))

  # cpv_na también funciona sobre la consulta sin ejecutar (arrow).
  perezosa <- cpv_tbl("persona", out_dir) |> cpv_na(sexo, dir = out_dir) |> dplyr::collect()
  expect_identical(as.integer(perezosa$sexo), c(1L, 2L, NA, 2L, NA))
})

test_that("cpv_geo devuelve nombres por ubigeo", {
  g <- cpv_geo("departamento", dir = out_dir)
  expect_identical(g$departamento, c("Amazonas", "Áncash"))
})
