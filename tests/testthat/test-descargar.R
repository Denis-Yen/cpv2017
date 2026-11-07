# Simula la publicación con una carpeta local servida por URL file://.
url_local <- function(dir) {
  ruta <- normalizePath(dir, winslash = "/")
  paste0(if (startsWith(ruta, "/")) "file://" else "file:///", ruta)
}

preparar_publicacion <- function() {
  base <- file.path(tempdir(), "dl_base")
  pub <- file.path(tempdir(), "dl_publicado")
  unlink(c(base, pub), recursive = TRUE)
  suppressMessages(cpv_convertir(crear_base_mini(base), pub))
  cpv_manifiesto(pub)
  pub
}

test_that("cpv_descargar baja y verifica los archivos publicados", {
  pub <- preparar_publicacion()
  dest <- file.path(tempdir(), "dl_destino")
  unlink(dest, recursive = TRUE)

  expect_message(cpv_descargar(dest, url_local(pub)), "Listo")
  pq <- list.files(pub, "\\.parquet$")
  expect_setequal(list.files(dest, "\\.parquet$"), pq)
  expect_identical(unname(tools::md5sum(file.path(dest, pq))),
                   unname(tools::md5sum(file.path(pub, pq))))
  expect_identical(nrow(dplyr::collect(cpv_tbl("persona", dest))), 5L)

  # Segunda vez: no vuelve a bajar lo que ya está bien.
  expect_message(cpv_descargar(dest, url_local(pub)), "ya descargado")
  expect_false(any(grepl("\\.parcial$", list.files(dest))))
})

test_that("cpv_descargar rechaza un archivo dañado y no deja restos", {
  pub <- preparar_publicacion()
  man <- utils::read.csv(file.path(pub, "manifiesto.csv"))
  man$md5[man$archivo == "persona.parquet"] <- "00000000000000000000000000000000"
  utils::write.csv(man, file.path(pub, "manifiesto.csv"), row.names = FALSE)

  dest <- file.path(tempdir(), "dl_danado")
  unlink(dest, recursive = TRUE)
  expect_error(suppressMessages(cpv_descargar(dest, url_local(pub))), "dañado")
  expect_false(file.exists(file.path(dest, "persona.parquet")))
  expect_false(any(grepl("\\.parcial$", list.files(dest))))
})

test_that("cpv_descargar explica qué hacer si no hay URL", {
  expect_error(cpv_descargar(tempdir(), url = ""), "CPV2017_URL")
})

test_that("el manifiesto no acepta rutas fuera de la carpeta", {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(archivo = "../malo.parquet", bytes = 1, md5 = "x"), f, row.names = FALSE)
  expect_error(leer_manifiesto(f), "no permitidos")
})
